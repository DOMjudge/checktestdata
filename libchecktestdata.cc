/*
   Libchecktestdata -- check testdata according to specification.

   It's licensed under the 2-clause BSD license, see the file COPYING.
 */

#define _XOPEN_SOURCE 700

#include <iostream>
#include <iomanip>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <map>
#include <set>
#include <regex>
#include <type_traits>
#include <cctype>
#include <cstdlib>
#include <cstdarg>
#include <climits>
#include <getopt.h>
#include <time.h>
#include <cstdlib>
#include <boost/variant.hpp>
#include <boost/exception_ptr.hpp>
#include <boost/exception/diagnostic_information.hpp>
#include <gmpxx.h>

#include "parser.h"
#include "libchecktestdata.hpp"
#include "databuffer.hpp"

using namespace std;

namespace checktestdata {

class doesnt_match_exception {};
class eof_found_exception {};
class generate_exception {};

const int display_before_error = 65;
const int display_after_error  = 50;

size_t prognr;
const command *currcmd;

gmp_randclass gmp_rnd(gmp_randinit_default);

// Simple function to get a random integer in the range [0,n-1].
// Assumes that the gmp_rnd global variable has been initialized.
unsigned long get_random(unsigned long n)
{
	mpz_class x = gmp_rnd.get_z_range(n);
	return x.get_ui();
}

databuffer data;
vector<command> program;

// This stores array-type variables like x[i,j] as string "x" and
// vector of the indices. Plain variables are stored using an index
// vector of zero length.
typedef map<vector<bigint>,value_t> indexmap;
typedef map<value_t,set<vector<bigint>>> valuemap;
map<string,indexmap> variable, preset;
map<string,valuemap> rev_variable, rev_preset;

// List of loop starting commands like REP, initialized in checksyntax.
set<string> loop_cmds;

int whitespace_ok;
int debugging;
int quiet;
int gendata;

void debug(const char *, ...) __attribute__((format (printf, 1, 2)));

void debug(const char *format, ...)
{
	va_list ap;
	va_start(ap,format);

	if ( debugging ) {
		fprintf(stderr,"debug: ");

        if ( format!=NULL ) {
			vfprintf(stderr,format,ap);
        } else {
			fprintf(stderr,"<no debug data?>");
        }

		fprintf(stderr,"\n");
	}

	va_end(ap);
}

void readprogram(istream &in)
{
	debug("parsing script...");

	Parser parseprog(in);
	try {
		if ( parseprog.parse()!=0 ) {
			cerr << "parse error reading program" << endl;
			exit(exit_failure);
		}
	} catch ( exception& e ) {
		cerr << "parse error: " << e.what() << endl;
		exit(exit_failure);
	}

	program = parseprog.parseResult.args;

	// Add (implicit) EOF command at end of input
	program.push_back(command("EOF"));

	// Check for correct REP ... END nesting
	vector<string> levels;
	for (size_t i=0; i<program.size(); i++) {
		if ( loop_cmds.count(program[i].name()) ||
		     program[i].name() == "IF" )
			levels.push_back(program[i].name());

		if ( program[i].name()=="END" ) {
			if ( levels.size()==0 ) {
				cerr << "incorrect END statement" << endl;
				exit(exit_failure);
			}
			levels.pop_back();
		}

		if ( program[i].name()=="ELSE" ) {
			if ( levels.size()==0 || levels.back()!="IF") {
				cerr << "incorrect ELSE statement" << endl;
				exit(exit_failure);
			}
			levels.pop_back();
			levels.push_back(program[i].name());
		}
	}
	if ( levels.size()>0 ) {
		cerr << "unclosed block statement(s)" << endl;
		exit(exit_failure);
	}
}

void readtestdata(istream &in)
{
	debug("reading testdata...");

	stringstream ss;
	ss << in.rdbuf();
	if ( in.fail() ) {
		cerr << "error reading testdata" << endl;
		exit(exit_failure);
	}

	data = databuffer(ss.str());
}

void error(string msg = string())
{
	if ( gendata ) {
		cerr << "ERROR: in command " << *currcmd << ": " << msg << endl << endl;
		throw generate_exception();
	}

	debug("error at datanr = %d\n",(int)data.pos());

	if ( !quiet ) {
		cerr << data.prev(display_before_error) << endl;
		cerr << string(min(data.lpos(),(size_t)display_before_error),' ') << '^';
		cerr << data.next(display_after_error) << endl << endl;

		cerr << "ERROR: line " << data.line()+1 << " character " << data.lpos()+1;
		cerr << " of testdata doesn't match " << *currcmd;
		if ( msg.length()>0 ) cerr << ": " << msg;
		cerr << endl << endl;
	}

	throw doesnt_match_exception();
}

long string2int(string s)
{
	long res;
	char *ptr;

	res = strtol(s.c_str(),&ptr,10);
	if ( *ptr!='\0' || res==LONG_MIN || res==LONG_MAX ) {
		error("cannot parse integer: `" + s + "'");
	}

	return res;
}

// forward declarations
value_t eval(const expr&);
bigint evalAsInt(const expr& e);

value_t getvar(const expr& var, int use_preset = 0)
{
	// Construct index array. The cast to bigint automatically
	// verifies that the index value is of type bigint.
	vector<bigint> ind;
	for(size_t i=0; i<var.nargs(); i++) {
		ind.push_back(evalAsInt(var.args[i]));
	}
	if ( use_preset ) {
		if ( preset.count(var.val) && preset[var.val].count(ind) ) {
			return preset[var.val][ind];
		}
		return value_t();
	} else {
		if ( gendata && preset.count(var.val) && preset[var.val].count(ind) ) {
			return preset[var.val][ind];
		}
		// Avoid double lookups on the hot path
		auto it = variable.find(var.val);
		if (it != variable.end()) {
			auto& map = it->second;
			auto it2 = map.find(ind);
			if (it2 != map.end()) {
				return it2->second;
			}
		}
	}
	cerr << "variable " << var << " undefined in " << program[prognr] << endl;
	exit(exit_failure);
}

void setvar(const expr& var, value_t val, int use_preset = 0)
{
	// Construct index array. The cast to bigint automatically
	// verifies that the index value is of type bigint.
	vector<bigint> ind;
	for(size_t i=0; i<var.nargs(); i++) {
		ind.push_back(evalAsInt(var.args[i]));
	}

	map<string,indexmap> *varlist = &variable;
	map<string,valuemap> *revlist = &rev_variable;
	if ( use_preset ) {
		varlist = &preset;
		revlist = &rev_preset;
	}

	// Remove previously existing value -> index:
	if ( (*varlist)[var].count(ind) ) {
		(*revlist)[var][val].erase(ind);
		if ( (*revlist)[var][val].size()==0 ) (*revlist)[var].erase(val);
	}
	(*varlist)[var][ind] = val;
	(*revlist)[var][val].insert(ind);
}

void setvars(const vector<parse_t>& varlist, int use_preset = 0)
{
	for(size_t i=0; i<varlist.size(); i++) {
		setvar(varlist[i].args[0],eval(varlist[i].args[1]),use_preset);
	}
}

void unsetvars(const args_t& varlist)
{
	for(size_t i=0; i<varlist.size(); i++) {
		variable.erase(varlist[i].val);
		rev_variable.erase(varlist[i].val);
	}
}

value_t value(const expr& x)
{
	debug("value '%s'",x.val.c_str());
	if ( x.cache.val.which()!=value_none ) {
		debug("eval cached");
		return x.cache;
	}

	if ( x.op=='S' ) return x.cache = value_t(x.val);
	if ( isalpha(x.val[0]) ) return getvar(x);

	mpz_class intval;
	mpf_class fltval;
	if ( intval.set_str(x.val,0)==0 ) {
		bigint c = bigint(intval);
		c.shrink();
		x.cachedLong = c.small;
		return x.cache = value_t(c);
	}
	else if ( fltval.set_str(x.val,0)==0 ) {
		// Set sufficient precision:
		if ( fltval.get_prec()<4*x.val.length() ) {
			fltval.set_prec(4*x.val.length());
			fltval.set_str(x.val,0);
		}
		return x.cache = value_t(fltval);
	}
	return value_t();
}


template<class A, class B>
struct arith_result {
	typedef typename conditional<
		is_same<A,bigint>::value && is_same<B,bigint>::value,
			bigint,
			mpf_class
			>::type type;
};

template<class A, class B> struct arith_compatible {
	constexpr static bool value = (is_same<bigint,A>::value || is_same<mpf_class,A>::value) &&
		(is_same<bigint,B>::value || is_same<mpf_class,B>::value);
};

template<class A, class B> struct is_comparable {
	constexpr static bool value = arith_compatible<A,B>::value ||
		(is_same<A,string>::value && is_same<B,string>::value);
};

/* We define overloaded arithmetic and comparison operators.
 * As they are all identical, the code is captured in two macro's
 * below, except for the modulus and unary minus.
 */
#define DECL_VALUE_BINOP(op,name) \
struct arithmetic_##name : public boost::static_visitor<value_t> {\
	template<class A, class B, typename enable_if<!arith_compatible<A,B>::value,int>::type = 0>\
	value_t operator()(const A& a, const B& b) const {\
		cerr << "cannot compute " << a << " " #op " " << b << endl; \
		exit(exit_failure);\
	}\
	template<class A, class B, typename enable_if<arith_compatible<A,B>::value,int>::type = 0,\
		class C = typename arith_result<A,B>::type>\
	value_t operator()(const A& a, const B& b)const  {\
		return value_t(C(a op b));\
	}\
};\
value_t operator op(const value_t &x, const value_t &y) \
{ \
	return boost::apply_visitor(arithmetic_##name(), x.val, y.val);\
}

#define DECL_VALUE_CMPOP(op,name) \
struct arithmetic_##name : public boost::static_visitor<bool> {\
	template<class A, class B, typename enable_if<!is_comparable<A,B>::value,int>::type = 0>\
	bool operator()(const A& a, const B& b)const  {\
		cerr << "cannot compute " << a << " " #op " " << b << endl; \
		exit(exit_failure);\
	}\
	template<class A, class B, typename enable_if<is_comparable<A,B>::value,int>::type = 0>\
	bool operator()(const A& a, const B& b)const  {\
		return a op b;\
	}\
};\
bool operator op(const value_t &x, const value_t &y) \
{ \
	return boost::apply_visitor(arithmetic_##name(), x.val, y.val);\
}

DECL_VALUE_BINOP(+,plus)
DECL_VALUE_BINOP(-,minus)
DECL_VALUE_BINOP(*,times)
DECL_VALUE_BINOP(/,div)

DECL_VALUE_CMPOP(>,g)
DECL_VALUE_CMPOP(<,l)
DECL_VALUE_CMPOP(>=,ge)
DECL_VALUE_CMPOP(<=,le)
DECL_VALUE_CMPOP(==,e)
DECL_VALUE_CMPOP(!=,ne)

value_t operator -(const value_t &x)
{
	return value_t(bigint(0)) - x;
}

value_t operator %(const value_t &x, const value_t &y)
{
	const bigint *xp, *yp;
	if ( (xp = boost::get<const bigint>(&x.val)) && (yp = boost::get<const bigint>(&y.val))) {
		auto res = *xp;
		res %= *yp;
		return value_t(res);
	}
	cerr << "can only use modulo on integers in " << program[prognr] << endl;
	exit(exit_failure);
}

struct pow_visitor : public boost::static_visitor<value_t> {
	template<class B, class E>
	value_t operator()(const B&, const E&) const {
		cerr << "only integer exponents allowed in " << program[prognr] << endl;
		exit(exit_failure);
	}
	template<class B>
	value_t operator()(const B& b, const bigint& e) const {
		if(!e.fits_ulong_p()) {
			cerr << "integer exponent " << e
				<< " does not fit in unsigned long in " << program[prognr] << endl;
			exit(exit_failure);
		}
		unsigned long f = e.get_ui();
		return pow(b, f);
	}
	value_t pow(const bigint& b, unsigned long e) const {
		mpz_class res;
		mpz_pow_ui(res.get_mpz_t(), b.to_mpz().get_mpz_t(), e);
		bigint res2(res);
		res2.shrink();
		return value_t(res2);
	}
	value_t pow(const mpf_class& b, unsigned long e) const {
		mpf_class res;
		mpf_pow_ui(res.get_mpf_t(), b.get_mpf_t(), e);
		return value_t(res);
	}
	template<class B>
	value_t pow(const B&, unsigned long) const {
		cerr << "exponentiation base must be of arithmetic type in "
			 << program[prognr] << endl;
		exit(exit_failure);
	}
};

value_t pow(const value_t &x, const value_t &y)
{
	return boost::apply_visitor(pow_visitor(), x.val, y.val);
}

value_t evalfun(args_t funargs)
{
	string fun = funargs[0].val;
	if ( fun=="STRLEN" ) {
		string str = eval(funargs[1]).getstr();
		return value_t(bigint(str.length()));
	}

	cerr << "unknown function '" << fun << "' in "
		 << program[prognr] << endl;
	exit(exit_failure);
}

bool cachable(const expr& e)
{
	switch ( e.op ) {
	case 'I':
	case 'F':
	case 'S':
		return true;

	case '+':
	case '-':
	case '*':
	case '%':
	case '/':
	case '^':
	case 'n':
	case '?':
	case '|':
	case '&':
	case '!':
	case 'f':
		for(size_t i=0; i<e.nargs(); i++) if ( !cachable(e.args[i]) ) return false;
		return true;
	}
	return false;
}

value_t eval(const expr& e)
{
	debug("eval op='%c', val='%s', #args=%d",e.op,e.val.c_str(),(int)e.args.size());
	if ( e.cache.val.which()!=value_none ) {
		debug("eval cached");
		return e.cache;
	}
	value_t res;
	switch ( e.op ) {
	case 'I':
	case 'F':
	case 'S':
	case 'v': res = value(e); break;
	case 'n': res = -eval(e.args[0]); break;
	case '+': res = eval(e.args[0]) + eval(e.args[1]); break;
	case '-': res = eval(e.args[0]) - eval(e.args[1]); break;
	case '*': res = eval(e.args[0]) * eval(e.args[1]); break;
	case '/': res = eval(e.args[0]) / eval(e.args[1]); break;
	case '%': res = eval(e.args[0]) % eval(e.args[1]); break;
	case '^': res = pow(eval(e.args[0]),eval(e.args[1])); break;
	case 'f': res = evalfun(e.args); break;
	default:
		cerr << "unknown arithmetic operator '" << e.op << "' in "
		     << program[prognr] << endl;
		exit(exit_failure);
	}
	if ( cachable(e) ) {
		e.cache = res;
		if ( res.val.which()==value_int ) {
			bigint x = res;
			e.cachedLong = x.small;
		}
	}
	return res;
}

bigint evalAsInt(const expr& e)
{
	if ( e.cachedLong != LONG_MIN ) {
		return bigint(e.cachedLong);
	}
	return eval(e);
}

bool compare(const expr& cmp)
{
	string op = cmp.val;
	value_t l = eval(cmp.args[0]);
	value_t r = eval(cmp.args[1]);

	if ( op=="<"  ) return l<r;
	if ( op==">"  ) return l>r;
	if ( op=="<=" ) return l<=r;
	if ( op==">=" ) return l>=r;
	if ( op=="==" ) return l==r;
	if ( op=="!=" ) return l!=r;

	cerr << "unknown compare operator " << op << " in "
	     << program[prognr] << endl;
	exit(exit_failure);
}

bool unique(const args_t& varlist)
{
	debug("unique, #args=%d",(int)varlist.size());

	vector<decltype(&variable[""])> vars;

	// First check if all variables exist.
	for(size_t i=0; i<varlist.size(); i++) {
		if ( !variable.count(varlist[i].val) ) {
			cerr << "variable " << varlist[i].val << " undefined in "
				 << program[prognr] << endl;
			exit(exit_failure);
		}
		vars.push_back(&variable[varlist[i].val]);
	}

	// Check if all variables have equal numbers of indices. Then we
	// can later check if they have the same indices by comparing the
	// indices to that of the first variable.
	for(size_t i=1; i<vars.size(); i++) {
		if ( vars[0]->size()!=vars[i]->size() ) {
			error("variables " + varlist[0].val + " and " + varlist[i].val +
			      " have different indices");
		}
	}
	// Now check if all tuples are unique.
	vector<pair<vector<value_t>,const indexmap::key_type*>> tuples;
	for(indexmap::iterator it=vars[0]->begin();
		it!=vars[0]->end(); ++it) {
		const vector<bigint> &index = it->first;
		vector<value_t> tuple;
		for(size_t i=0; i<vars.size(); i++) {
			auto it = vars[i]->find(index);
			if ( it == vars[i]->end() ) {
				string s;
				s = "index [";
				for(size_t j=0; j<index.size(); j++) {
					if ( j>0 ) s += ",";
					s += index[j].get_str();
				}
				s += "] not defined for variable " + varlist[i].val;
				error(s);
			}
			tuple.push_back(it->second);
		}
		tuples.emplace_back(tuple,&index);
	}
	sort(begin(tuples), end(tuples));
	for(size_t i = 0; i + 1 < tuples.size(); ++i) {
		if(tuples[i].first == tuples[i+1].first) {
			auto tuple = tuples[i].first;
			auto index = *tuples[i].second;
			string s;
			s = "non-unique tuple (" + tuple[0].tostr();
			for(size_t j=1; j<tuple.size(); j++) s += "," + tuple[j].tostr();
			s += ") found at index [";
			for(size_t j=0; j<index.size(); j++) {
				if ( j>0 ) s += ",";
				s += index[j].get_str();
			}
			s += "]";
			error(s);
		}
	}
	return true;
}

bool inarray(const expr& e, const expr& array)
{
	string var = array.val;
	value_t val = eval(e);

	debug("inarray, value = %s, array = %s",val.tostr().c_str(),var.c_str());

	if ( !variable.count(var) ) {
		cerr << "variable " << var << " undefined in "
			 << program[prognr] << endl;
		exit(exit_failure);
	}

	return rev_variable[var].count(val) && rev_variable[var][val].size()>0;
}

bool dotest(const test& t)
{
	debug("test op='%c', #args=%d",t.op,(int)t.args.size());
	switch ( t.op ) {
	case '!': return !dotest(t.args[0]);
	case '&': return dotest(t.args[0]) && dotest(t.args[1]);
	case '|': return dotest(t.args[0]) || dotest(t.args[1]);
	case 'E':
		if ( gendata ) {
			return (get_random(10) < 3);
		} else {
			return data.eof();
		}
	case 'M':
		if ( gendata ) {
			return (get_random(2) == 0);
		} else {
			return !data.eof() && t.args[0].val.find(data.peek())!=string::npos;
		}
	case 'U': return unique(t.args);
	case 'A': return inarray(t.args[0],t.args[1]);
	case '?': return compare(t);
	default:
		cerr << "unknown test " << t.op << " in " << program[prognr] << endl;
		exit(exit_failure);
	}
}

void checkspace()
{
	if ( data.eof() ) error("end of file");

	if ( whitespace_ok ) {
		// First check at least one space-like character
		if ( !isspace_notnewline(data.readchar()) ) error();
		// Then greedily read non-newline whitespace
		data.readwhitespace();
	} else {
		if ( data.readchar()!=' ' ) error();
	}
}

void checknewline()
{
	// Trailing whitespace before newline
	if ( whitespace_ok ) data.readwhitespace();

	if ( data.eof() ) error("end of file");
	if ( data.readchar()!='\n' ) error();

	// Leading whitespace after newline
	if ( whitespace_ok ) data.readwhitespace();
}

#define MAX_MULT 10
int getmult(string &exp, unsigned int &index)
{
	index++;

	if (index >= exp.length()) {
		return 1;
	}

	int min = 0;
	int max = MAX_MULT;
	switch (exp[index]) {
	case '?':
		index++;
		max = 1;
		break;
	case '+':
		index++;
		min = 1;
		break;
	case '*':
		index++;
		break;
	case '{':
		index++;
		{
			int end = exp.find_first_of('}', index);
			string minmaxs = exp.substr(index, end - index);
			int pos = minmaxs.find_first_of(',');
			if (pos == -1) {
				min = max = string2int(minmaxs);
			} else {
				string mins = minmaxs.substr(0, pos);
				string maxs = minmaxs.substr(pos + 1);
				min = string2int(mins);
				if (maxs.length() > 0) {
					max = string2int(maxs);
				}
			}
			index = end + 1;
		}
		break;
	default:
		min = 1;
		max = 1;
	}

	return (min + get_random(1 + max - min));
}

string genregex(string exp)
{
	unsigned int i = 0;
	string res;
	debug("genregex '%s'",exp.c_str());
	while (i < exp.length()) {
		switch (exp[i]) {
		case '\\':
			{
				i++;
				char c = exp[i];
				int mult = getmult(exp, i);
				for (int cnt = 0; cnt < mult; cnt++) {
					res += c;
				}
			}
			break;
		case '.':
			{
				int mult = getmult(exp, i);
				for (int cnt = 0; cnt < mult; cnt++) {
					res += (char) (' ' + get_random((int) ('~' - ' ')));
				}
			}
			break;
		case '[':
			{
				set<char> possible;
				bool escaped = false, inverted = false;
				if ( i + 1 < exp.length() && exp[i+1] == '^' ) {
					inverted = true;
					i++;
				}
				while (i + 1 < exp.length()) {
					i++;
					if (escaped) {
						escaped = false;
					} else if (exp[i] == ']') {
						break;
					} else if (exp[i] == '\\') {
						escaped = true;
						continue;
					}
					if (i + 2 < exp.length() && exp[i + 1] == '-' && exp[i] != '[' && exp[i+2] != ']') {
						char from = exp[i];
						i += 2;
						char to = exp[i];
						if (to == '\\') {
							i++;
							to = exp[i];
						}
						while (from <= to) {
							possible.insert(from);
							from++;
						}
					} else {
						possible.insert(exp[i]);
					}
				}
				vector<char> possibleVec;
				if ( inverted ) {
					for (char c = ' '; c <= '~'; c++) {
						if ( !possible.count(c) ) possibleVec.push_back(c);
					}
				} else {
					copy(possible.begin(), possible.end(), std::back_inserter(possibleVec));
				}
				int mult = getmult(exp, i);
				for (int cnt = 0; cnt < mult; cnt++) {
					res += possibleVec[get_random(possibleVec.size())];
				}
			}
			break;
		case '(':
			{
				i++;
				vector<string> alternatives;
				int depth = 0;
				int begin = i;
				bool escaped = false;
				while (depth > 0 || escaped || exp[i] != ')') {
					if (escaped) {
						escaped = false;
					} else if (exp[i] == '\\') {
						escaped = true;
					} else if (depth == 0 && exp[i] == '|') {
						alternatives.push_back(exp.substr(begin, i - begin));
						begin = i + 1;
					} else if (exp[i] == '(') {
						depth++;
					} else if (exp[i] == ')') {
						depth--;
					}
					i++;
				}
				alternatives.push_back(exp.substr(begin, i - begin));
				int mult = getmult(exp, i);
				for (int cnt = 0; cnt < mult; cnt++) {
					res += genregex(alternatives[get_random(alternatives.size())]);
				}
			}
			break;
		default:
			{
				char c = exp[i];
				int mult = getmult(exp, i);
				for (int cnt = 0; cnt < mult; cnt++) {
					res += c;
				}
			}
			break;
		}
	}
	return res;
}

// Parse {min,max}decimals in FLOATP command.
void getdecrange(const command& cmd, int *decrange)
{
	// Read {min,max}decimals range for float.
	for(int i=0; i<2; i++) {
		value_t arg = eval(cmd.args[2+i]);
		if ( arg.val.which()!=value_int ) {
			error((i==0 ? "min":"max")+string("decimal is not an integer"));
		}
		bigint val = arg;
		if ( val<0 || val>=INT_MAX ) {
			error(string("the value of ")+(i==0 ? "min":"max")+"decimal is out of range");
		}
		decrange[i] = val.get_si();
	}
	if ( decrange[0]>decrange[1] ) error("invalid decimals range specified");
}

void gentoken(command cmd, ostream &datastream)
{
	currcmd = &cmd;
	debug("generating token %s", cmd.name().c_str());

	if ( cmd.name()=="SPACE" ) datastream << ' ';

	else if ( cmd.name()=="NEWLINE" ) datastream << '\n';

	else if ( cmd.name()=="INT" ) {
		bigint lo = eval(cmd.args[0]);
		bigint hi = eval(cmd.args[1]);
		bigint x(lo.to_mpz() + gmp_rnd.get_z_range((hi - lo + 1).to_mpz()));

		if ( cmd.nargs()>=3 ) {
			// Check if we have a preset value, then override the
			// random generated value
			value_t y = getvar(cmd.args[2],1);
			if ( !boost::get<none_t>(&y.val) ) {
				x = y;
				if ( x<lo || x>hi ) {
					error("preset value for '" + string(cmd.args[2]) + "' out of range");
				}
			}
			setvar(cmd.args[2],value_t(x));
		}

		datastream << x.get_str();
	}

	else if ( cmd.name()=="FLOAT" || cmd.name()=="FLOATP" ) {
		mpf_class lo = eval(cmd.args[0]);
		mpf_class hi = eval(cmd.args[1]);

		char floatspec = 'f'; // Default to fixed point notation
		int ndecimals = 15; // Default number of decimals shown

		size_t nbaseargs = 2;
		int decrange[2];
		if ( cmd.name()=="FLOATP" ) {
			nbaseargs = 4;
			getdecrange(cmd,decrange);
			ndecimals = decrange[1];
		}

		if ( cmd.nargs()>=nbaseargs+2 ) {
			if ( cmd.args[nbaseargs+1].name()=="SCIENTIFIC" ) floatspec = 'E';
			else if ( cmd.args[nbaseargs+1].name()=="FIXED" ) floatspec = 'f';
			else {
				cerr << "invalid option in " << program[prognr] << endl;
				exit(exit_failure);
			}
		}

		mpf_class x(lo + gmp_rnd.get_f()*(hi-lo));

		if ( cmd.nargs()>=nbaseargs+1 ) {
			// Check if we have a preset value, then override the
			// random generated value
			value_t y = getvar(cmd.args[nbaseargs],1);
			if ( !boost::get<none_t>(&y.val) ) {
				x = y;
				if ( x<lo || x>hi ) {
					error("preset value for '" + string(cmd.args[nbaseargs]) +
					      "' out of range");
				}
			}
			setvar(cmd.args[2],value_t(x));
		}

		char tmp[256];
		gmp_snprintf(tmp,255,(string("%.*F")+floatspec).c_str(),ndecimals,x.get_mpf_t());
		datastream << tmp;
	}

	else if ( cmd.name()=="STRING" ) {
		string str = eval(cmd.args[0]).getstr();
		datastream << str;
	}

	else if ( cmd.name()=="REGEX" ) {
		string regexstr = eval(cmd.args[0]).getstr();
//		regex e1(regex, regex::extended); // this is only to check the expression
		string str = genregex(regexstr);
		datastream << str;
		if ( cmd.nargs()>=2 ) setvar(cmd.args[1],value_t(str));
	}

	else if ( cmd.name()=="ASSERT" ) {
		if ( !dotest(cmd.args[0]) ) error("assertion failed");
	}

	else if ( cmd.name()=="SET" ) {
		setvars(cmd.args);
	}

	else if ( cmd.name()=="UNSET" ) {
		unsetvars(cmd.args);
	}

	else {
		cerr << "unknown command " << program[prognr] << endl;
		exit(exit_failure);
	}
	currcmd = nullptr;
}

void checktoken(const command& cmd)
{
	currcmd = &cmd;
	debug("checking token %s at %lu,%lu",
	      cmd.name().c_str(),data.line(),data.lpos());

	if ( cmd.name()=="SPACE" ) checkspace();

	else if ( cmd.name()=="NEWLINE" ) checknewline();

	else if ( cmd.name()=="INT" ) {
		// Accepts format (0|-?[1-9][0-9]*), i.e. no leading zero's
		// and no '-0' accepted.
		string num;
		if ( data.peek()=='-' ) {
			data.readchar();
			num += '-';
		}
		while ( isdigit(data.peek()) ) {
			num += data.readchar();
		}

		bigint lo = evalAsInt(cmd.args[0]);
		bigint hi = evalAsInt(cmd.args[1]);

//		debug("%s <= %s <= %s",lo.get_str().c_str(),num.c_str(),hi.get_str().c_str());
		if ( cmd.nargs()>=3 ) debug("'%s' = '%s'",
		                            const_cast<char *>(cmd.args[2].c_str()),
		                            const_cast<char *>(num.c_str()));

		if ( num.size()==0 ) error();
		if ( num.size()>=2 && num[0]=='0' ) error("prefix zero(s)");
		if ( num.size()>=1 && num[0]=='-' &&
		     (num.size()==1 || num[1]=='0') ) error("invalid minus sign (-0 not allowed)");

		bigint x(num);

		if ( x<lo || x>hi ) error("value out of range");
		if ( cmd.nargs()>=3 ) setvar(cmd.args[2],value_t(x));
	}

	else if ( cmd.name()=="FLOAT" || cmd.name()=="FLOATP" ) {
		// Accepts format -?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?
		// i.e. no leading zeros, and the exponent is not allowed
		// with the FIXED option and required with the SCIENTIFIC option.

		size_t nbaseargs = 2;
		int decrange[2];
		if ( cmd.name()=="FLOATP" ) {
			// Read {min,max}decimals range for float.
			nbaseargs = 4;
			getdecrange(cmd,decrange);
		}

		int opt = 0; // 1 = scientific, 2 = fixed.
		if ( cmd.nargs()>=nbaseargs+2 ) {
			if ( cmd.args[nbaseargs+1].name()=="SCIENTIFIC" ) opt = 1;
			else if ( cmd.args[nbaseargs+1].name()=="FIXED" ) opt = 2;
			else {
				cerr << "invalid option in " << program[prognr] << endl;
				exit(exit_failure);
			}
		}

		size_t start = data.pos();
		// Match optional minus sign:
		if ( data.peek()=='-' ) data.readchar();
		// Match base with optional decimal dot:
		if ( !isdigit(data.peek()) ) error("digit expected");
		size_t digitpos = data.pos(), dotpos = string::npos;
		char first_digit = data.peek();
		while ( (isdigit(data.peek()) ||
		         (dotpos==string::npos && digitpos!=data.pos() && data.peek()=='.')) ) {
			if ( data.readchar()=='.' ) dotpos = data.pos()-1;
		}
		// Check that there are no leading zeros:
		if ( first_digit=='0' && dotpos-digitpos>1 ) error("prefix zero(s)");
		// Check that a dot is followed by a digit again:
		if ( !isdigit(data.peek(-1)) ) error("digit expected");

		size_t exppos = data.pos();
		bool has_exp = false;
		// Match exponent:
		if ( opt==1 || (opt==0 && toupper(data.peek())=='E') ) {
			if ( toupper(data.readchar())!='E' ) error("exponent 'E' expected");
			has_exp = true;
			if ( data.peek()=='-' || data.peek()=='+' ) data.readchar();
			char c = '!';
			while ( isdigit(data.peek()) ) c = data.readchar();
			if ( !isdigit(c) ) error("digit expected");
		}

		if ( cmd.name()=="FLOATP" ) {
			if ( has_exp && (first_digit=='0' || dotpos!=digitpos+1) ) {
				error("exactly one non-zero before the decimal dot expected");
			}
			int ndecimals = (dotpos==string::npos ? 0 : exppos - dotpos - 1);
			if ( ndecimals<decrange[0] || ndecimals>decrange[1] ) {
				char tmp[100];
				sprintf(tmp,"%d not in [%d,%d]",ndecimals,decrange[0],decrange[1]);
				error("number of decimals not within specified range: "+string(tmp));
			}
		}

		string matchstr = data.prev(data.pos()-start);

		debug("parsing float '%s', exponent = %d",matchstr.c_str(),has_exp);

		mpf_class x(matchstr,4*matchstr.length());

		mpf_class lo = eval(cmd.args[0]);
		mpf_class hi = eval(cmd.args[1]);

		if ( x<lo || x>hi ) error("value out of range");
		if ( cmd.nargs()>=nbaseargs+1 ) setvar(cmd.args[nbaseargs],value_t(x));
	}

	else if ( cmd.name()=="STRING" ) {
		string str = eval(cmd.args[0]).getstr();
		for (size_t i=0; i<str.size(); i++) {
			if ( data.eof() ) error("premature end of file");
			if ( data.readchar()!=str[i] ) error();
		}

		debug("'%s' = '%s'",str.c_str(),cmd.args[0].c_str());
	}

	else if ( cmd.name()=="REGEX" ) {
		string str = eval(cmd.args[0]).getstr();
		regex regexstr(str,regex::extended|regex::nosubs|regex::optimize);
		smatch res;
		string matchstr;

		if ( !regex_search(data.curr(),data.end(),res,regexstr,
		                   regex_constants::match_continuous) ) {
			error();
		} else {
			size_t match_len = res[0].second - res[0].first;
			size_t match_end = data.pos() + match_len;
			matchstr = data.next(match_len);
			while ( data.pos()<match_end ) data.readchar();
		}
		debug("'%s' = '%s'",matchstr.c_str(),str.c_str());

		if ( cmd.nargs()>=2 ) setvar(cmd.args[1],value_t(matchstr));
	}

	else if ( cmd.name()=="ASSERT" ) {
		if ( !dotest(cmd.args[0]) ) error("assertion failed");
	}

	else if ( cmd.name()=="SET" ) {
		setvars(cmd.args);
	}

	else if ( cmd.name()=="UNSET" ) {
		unsetvars(cmd.args);
	}

	else {
		cerr << "unknown command " << program[prognr] << endl;
		exit(exit_failure);
	}
	currcmd = nullptr;
}

// This function processes the outer control structure commands both
// for checking and generating testdata (as determined by the global
// variable 'gendata').
void checktestdata(ostream &datastream)
{
	datastream << setprecision(float_precision);

	while ( true ) {
		const command &cmd = program[prognr];
		currcmd = &cmd;

		if ( cmd.name()=="EOF" ) {
			if ( gendata ) {
				debug("done: EOF found");
				return;
			} else {
				debug("checking EOF");
				if ( !data.eof() ) error();
				throw eof_found_exception();
			}
		}

		else if ( loop_cmds.count(cmd.name()) ) {

			// Current and maximum loop iterations.
			unsigned long i = 0, times = ULONG_MAX;
			unsigned loopvar = 0; // Optional variable for loop iteration present.

			if ( cmd.name()=="REPI" || cmd.name()=="WHILEI" ) {
				loopvar = 1;
				setvar(cmd.args[0],value_t(bigint(i)));
			}

			if ( cmd.name()=="REP" || cmd.name()=="REPI" ) {
				bigint n = eval(cmd.args[loopvar]);
				if ( !n.fits_ulong_p() ) {
					cerr << "'" << n << "' does not fit in an unsigned long in "
						 << program[prognr] << endl;
					exit(exit_failure);
				}
				times = n.get_ui();
			}

			// Begin and end of loop commands
			int loopbegin, loopend;

			loopbegin = loopend = prognr + 1;

			for(int looplevel=1; looplevel>0; ++loopend) {
				string cmdstr = program[loopend].name();
				if ( loop_cmds.count(cmdstr) || cmdstr=="IF") looplevel++;
				if ( cmdstr=="END" ) looplevel--;
			}

			// Run loop...
			debug("running %s loop, commands %d - %d, max. times = %ld",
			      cmd.name().c_str(),loopbegin,loopend,times);

			while ( ((cmd.name()=="REP"   || cmd.name()=="REPI")   && i<times) ||
			        ((cmd.name()=="WHILE" || cmd.name()=="WHILEI") &&
			         dotest(cmd.args[loopvar])) ) {

				debug("loop iteration %ld/%ld",i+1,times);
				prognr = loopbegin;
				if ( i>0 && cmd.nargs()>=loopvar+2 ) {
					if ( gendata ) {
						gentoken(cmd.args[loopvar+1], datastream);
					} else {
						checktoken(cmd.args[loopvar+1]);
					}
				}
				checktestdata(datastream);
				i++;
				if ( loopvar ) setvar(cmd.args[0],value_t(bigint(i)));
			}

			// And skip to end of loop
			prognr = loopend;
		}

		else if ( cmd.name()=="IF" ) {

			// Find line numbers of matching else/end
			int ifnr   = prognr;
			int elsenr = -1;
			int endnr  = prognr+1;

			for(int looplevel=1; looplevel>0; ++endnr) {
				string cmdstr = program[endnr].name();
				if ( loop_cmds.count(cmdstr) || cmdstr=="IF") looplevel++;
				if ( cmdstr=="END" ) looplevel--;
				if ( cmdstr=="ELSE" && looplevel==1) elsenr = endnr;
			}
			endnr--;

			debug("IF statement, if/else/end commands: %d/%d/%d",
			      ifnr,elsenr,endnr);

			// Test and execute correct command block
			if (dotest(cmd.args[0])) {
				debug("executing IF clause");
				prognr = ifnr+1;
				checktestdata(datastream);
			}
			else if (elsenr!=-1) {
				debug("executing ELSE clause");
				prognr = elsenr+1;
				checktestdata(datastream);
			}

			prognr = endnr+1;
		}

		else if ( cmd.name()=="END" || cmd.name()=="ELSE" ) {
			debug("scope closed by %s",cmd.name().c_str());
			prognr++;
			return;
		}

		else {
			if ( gendata ) {
				gentoken(cmd, datastream);
			} else {
				checktoken(cmd);
			}
			prognr++;
		}
	}
	currcmd = nullptr;
}

void init_checktestdata(std::istream &progstream, int opt_mask)
{
	// Output floats with high precision:
	cout << setprecision(float_precision);
	cerr << setprecision(float_precision);
	mpf_set_default_prec(256);

	// Check the options bitmask
	if (opt_mask & opt_whitespace_ok) whitespace_ok = 1;
	if (opt_mask & opt_debugging    ) debugging = 1;
	if (opt_mask & opt_quiet        ) quiet = 1;

	// Initialize block_cmds here, as a set cannot be initialized on
	// declaration.
	loop_cmds.insert("REP");
	loop_cmds.insert("REPI");
	loop_cmds.insert("WHILE");
	loop_cmds.insert("WHILEI");

	// Read program and testdata
	readprogram(progstream);

	if ( debugging ) {
		for(size_t i=0; i<program.size(); i++) cerr << program[i] << endl;
	}

	// Initialize random generator
	struct timespec time;
	clock_gettime(CLOCK_REALTIME,&time);
	mpz_class seed = 1000000000 * mpz_class(time.tv_sec) + time.tv_nsec;
	gmp_rnd.seed(seed);

	// Initialize current position in program.
	prognr = 0;
}

bool gentestdata(ostream &datastream)
{
	// Generate random testdata
	gendata = 1;

	try {
		checktestdata(datastream);
	} catch (generate_exception) {
		return false;
	}

	return true;
}

bool checksyntax(istream &datastream)
{
	ofstream dummy;

	gendata = 0;
	readtestdata(datastream);

	// If we ignore whitespace, skip leading whitespace on first line
	// as a special case; other lines are handled by checknewline().
	if ( whitespace_ok ) data.readwhitespace();

	try {
		checktestdata(dummy);
	}
	catch (doesnt_match_exception) {
		return false;
	}
	catch (eof_found_exception) {}

	return true;
}

// This doesn't support variables with indices (yet?).
bool parse_preset_list(std::string str)
{
	std::istringstream in(str);
	Parser parselist(in, (int)Parser::PARSE_ASSIGNLIST);

	debug("parsing preset list '%s'", str.c_str());

	try {
		if ( parselist.parse()!=0 ) {
			cerr << "parse error reading preset list" << endl;
			return false;
		}
	} catch ( exception& e ) {
		cerr << "parse error: " << e.what() << endl;
		return false;
	}

	try {
		setvars(parselist.parseResult.args,1);
	} catch ( boost::exception& e ) {
		cerr << "error in boost variant: " << boost::diagnostic_information(e) << endl;
		return false;
	}

	return true;
}

} // namespace checktestdata
