/*
   Libchecktestdata -- check testdata according to specification.

   It's licensed under the 2-clause BSD license, see the file COPYING.
 */

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
#include <sys/time.h>
#include <cstdlib>
#include <boost/variant.hpp>
#include <boost/exception_ptr.hpp>
#include <boost/exception/diagnostic_information.hpp>
#include <gmpxx.h>

#include "parser.h"
#include "libchecktestdata.hpp"

using namespace std;

namespace checktestdata {

struct value_none {};
ostream& operator<<(ostream& os, const value_none&) {
	return os << "<no value>";
}

struct value_t {
	boost::variant<mpz_class, mpf_class, string, value_none> val;

	value_t(): val(value_none()) {}
	explicit value_t(mpz_class x): val(x) {}
	explicit value_t(mpf_class x): val(x) {}
	explicit value_t(string x): val(x) {}

	operator mpz_class() const;
	operator mpf_class() const;

	// This is a member function instead of a casting operator, since
	// otherwise the string could be used in other implicit casts.
	string getstr() const;

	// This converts any value type to a string representation.
	string tostr() const;
};

class doesnt_match_exception {};
class eof_found_exception {};
class generate_exception {};

ostream& operator <<(ostream &os, const value_t &val)
{
	return os << val.val;
}

string value_t::tostr() const
{
	stringstream ss;
	ss << *this;
	return ss.str();
}

const int display_before_error = 65;
const int display_after_error  = 50;

size_t prognr, datanr, linenr, charnr, extra_ws;
command currcmd;

gmp_randclass gmp_rnd(gmp_randinit_default);

string data;
vector<command> program;

// This stores array-type variables like x[i,j] as string "x" and
// vector of the indices. Plain variables are stored using an index
// vector of zero length.
typedef map<vector<mpz_class>,value_t> indexmap;
map<string,indexmap> variable, preset;

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

 	data = ss.str();
}

void error(string msg = string())
{
	if ( gendata ) {
		cerr << "ERROR: in command " << currcmd << ": " << msg << endl << endl;
		throw generate_exception();
	}

	size_t fr = max(0,int(datanr)-display_before_error);
	size_t to = min(data.size(),datanr+display_after_error);

	debug("error at datanr = %d, %d - %d\n",(int)datanr,(int)fr,(int)to);

	if ( !quiet ) {
		cerr << data.substr(fr,datanr-fr) << endl;
		cerr << string(min(charnr,(size_t)display_before_error),' ') << '^';
		cerr << data.substr(datanr,to-datanr) << endl << endl;

		cerr << "ERROR: line " << linenr+1 << " character " << charnr+1;
		cerr << " of testdata doesn't match " << currcmd;
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

value_t::operator mpz_class() const
{
	return boost::get<mpz_class>(val);
}

value_t::operator mpf_class() const
{
	if(const mpz_class* p = boost::get<mpz_class>(&val))
		return *p;
	return boost::get<mpf_class>(val);
}

string value_t::getstr() const
{
	return boost::get<string>(val);
}

value_t eval(expr); // forward declaration

value_t getvar(expr var, int use_preset = 0)
{
	// Construct index array. The cast to mpz_class automatically
	// verifies that the index value is of type mpz_class.
	vector<mpz_class> ind;
	for(size_t i=0; i<var.nargs(); i++) {
		ind.push_back(mpz_class(eval(var.args[i])));
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
		if ( variable.count(var.val) && variable[var.val].count(ind) ) {
			return variable[var.val][ind];
		}
	}
	cerr << "variable " << var << " undefined in " << program[prognr] << endl;
	exit(exit_failure);
}

void setvar(expr var, value_t val, int use_preset = 0)
{
	// Construct index array. The cast to mpz_class automatically
	// verifies that the index value is of type mpz_class.
	vector<mpz_class> ind;
	for(size_t i=0; i<var.nargs(); i++) {
		ind.push_back(mpz_class(eval(var.args[i])));
	}

	if ( use_preset ) {
		preset[var][ind] = val;
	} else {
		variable[var][ind] = val;
	}
}

void setvars(vector<parse_t> varlist, int use_preset = 0)
{
	for(size_t i=0; i<varlist.size(); i++) {
		setvar(varlist[i].args[0],eval(varlist[i].args[1]),use_preset);
	}
}

void unsetvars(args_t varlist)
{
	for(size_t i=0; i<varlist.size(); i++) {
		variable.erase(varlist[i].val);
	}
}

value_t value(expr x)
{
	debug("value '%s'",x.val.c_str());

	if ( x.op=='S' ) return value_t(x.val);
	if ( isalpha(x.val[0]) ) return getvar(x);

	mpz_class intval;
	mpf_class fltval;
	if ( intval.set_str(x.val,0)==0 ) return value_t(intval);
	else if ( fltval.set_str(x.val,0)==0 ) {
		// Set sufficient precision:
		if ( fltval.get_prec()<4*x.val.length() ) {
			fltval.set_prec(4*x.val.length());
			fltval.set_str(x.val,0);
		}
		return value_t(fltval);
	}
	return value_t();
}


template<class A, class B>
struct arith_result {
	typedef typename conditional<
		is_same<A,mpz_class>::value && is_same<B,mpz_class>::value,
			mpz_class,
			mpf_class
			>::type type;
};

template<class A, class B> struct arith_compatible {
	constexpr static bool value = (is_same<mpz_class,A>::value || is_same<mpf_class,A>::value) &&
		(is_same<mpz_class,B>::value || is_same<mpf_class,B>::value);
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
	return value_t(mpz_class(0)) - x;
}

value_t operator %(const value_t &x, const value_t &y)
{
	const mpz_class *xp, *yp;
	if ( (xp = boost::get<const mpz_class>(&x.val)) && (yp = boost::get<const mpz_class>(&y.val))) {
		auto res = *xp;
		res %= *yp;
		return value_t(res);
	}
	cerr << "can only use modulo on integers in " << program[prognr] << endl;
	exit(exit_failure);
}

struct pow_visitor : public boost::static_visitor<value_t> {
	template<class B, class E>
	value_t operator()(const B& b, const E& e) const {
		cerr << "only integer exponents allowed in " << program[prognr] << endl;
		exit(exit_failure);
	}
	template<class B>
	value_t operator()(const B& b, const mpz_class& e) const {
		if(!e.fits_ulong_p()) {
			cerr << "integer exponent " << e
				<< " does not fit in unsigned long in " << program[prognr] << endl;
			exit(exit_failure);
		}
		return pow(b, e);
	}
	value_t pow(const mpz_class& b, const mpz_class& e) const {
		mpz_class res;
		mpz_pow_ui(res.get_mpz_t(), b.get_mpz_t(), e.get_ui());
		return value_t(res);
	}
	value_t pow(const mpf_class& b, const mpz_class& e) const {
		mpf_class res;
		mpf_pow_ui(res.get_mpf_t(), b.get_mpf_t(), e.get_ui());
		return value_t(res);
	}
	template<class B>
	value_t pow(const B&, const mpz_class&) const {
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
		return value_t(mpz_class(str.length()));
	}

	cerr << "unknown function '" << fun << "' in "
		 << program[prognr] << endl;
	exit(exit_failure);
}

value_t eval(expr e)
{
	debug("eval op='%c', val='%s', #args=%d",e.op,e.val.c_str(),(int)e.args.size());
	switch ( e.op ) {
	case 'I':
	case 'F':
	case 'S':
	case 'v': return value(e);
	case 'n': return -eval(e.args[0]);
	case '+': return eval(e.args[0]) + eval(e.args[1]);
	case '-': return eval(e.args[0]) - eval(e.args[1]);
	case '*': return eval(e.args[0]) * eval(e.args[1]);
	case '/': return eval(e.args[0]) / eval(e.args[1]);
	case '%': return eval(e.args[0]) % eval(e.args[1]);
	case '^': return pow(eval(e.args[0]),eval(e.args[1]));
	case 'f': return evalfun(e.args);
	default:
		cerr << "unknown arithmetic operator '" << e.op << "' in "
		     << program[prognr] << endl;
		exit(exit_failure);
	}
}

bool compare(expr cmp)
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

bool unique(args_t varlist)
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
		const vector<mpz_class> &index = it->first;
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

bool inarray(expr e, expr array)
{
	string var = array.val;
	value_t val = eval(e);

	debug("inarray, value = %s, array = %s",val.tostr().c_str(),var.c_str());

	if ( !variable.count(var) ) {
		cerr << "variable " << var << " undefined in "
			 << program[prognr] << endl;
		exit(exit_failure);
	}

	for(indexmap::iterator it=variable[var].begin(); it!=variable[var].end(); ++it) {
		if ( it->second==val ) return true;
	}
	return false;
}

bool dotest(test t)
{
	debug("test op='%c', #args=%d",t.op,(int)t.args.size());
	switch ( t.op ) {
	case '!': return !dotest(t.args[0]);
	case '&': return dotest(t.args[0]) && dotest(t.args[1]);
	case '|': return dotest(t.args[0]) || dotest(t.args[1]);
	case 'E': if ( gendata ) {
			  return (random() % 10 < 3);
		  } else {
			  return datanr>=data.size();
		  }
	case 'M': if ( gendata ) {
			  return (random() % 2 == 0);
		  } else {
			  return datanr<data.size() && t.args[0].val.find(data[datanr])!=string::npos;
		  }
	case 'U': return unique(t.args);
	case 'A': return inarray(t.args[0],t.args[1]);
	case '?': return compare(t);
	default:
		cerr << "unknown test " << t.op << " in " << program[prognr] << endl;
		exit(exit_failure);
	}
}

int isspace_notnewline(char c) { return isspace(c) && c!='\n'; }

void readwhitespace()
{
	while ( datanr<data.size() && isspace_notnewline(data[datanr]) ) {
		datanr++;
		charnr++;
		extra_ws++;
	}
}

void checkspace()
{
	if ( datanr>=data.size() ) error("end of file");

	if ( whitespace_ok ) {
		// First check at least one space-like character
		if ( !isspace_notnewline(data[datanr++]) ) error();
		charnr++;
		// Then greedily read non-newline whitespace
		readwhitespace();
	} else {
		if ( data[datanr++]!=' ' ) error();
		charnr++;
	}
}

void checknewline()
{
	// Trailing whitespace before newline
	if ( whitespace_ok ) readwhitespace();

	if ( datanr>=data.size() ) error("end of file");
	if ( data[datanr++]!='\n' ) error();
	linenr++;
	charnr=0;

	// Leading whitespace after newline
	if ( whitespace_ok ) readwhitespace();

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

	return (min + random() % (1 + max - min));
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
					res += (char) (' ' + (random() % (int) ('~' - ' ')));
				}
			}
			break;
		case '[':
			{
				set<char> possible;
				bool escaped = false;
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
				copy(possible.begin(), possible.end(), std::back_inserter(possibleVec));
				int mult = getmult(exp, i);
				for (int cnt = 0; cnt < mult; cnt++) {
					res += possibleVec[random() % possibleVec.size()];
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
					res += genregex(alternatives[random() % alternatives.size()]);
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

void gentoken(command cmd, ostream &datastream)
{
	currcmd = cmd;
	debug("generating token %s at %lu,%lu",
	      cmd.name().c_str(),(unsigned long)linenr,(unsigned long)charnr);

	if ( cmd.name()=="SPACE" ) datastream << ' ';

	else if ( cmd.name()=="NEWLINE" ) datastream << '\n';

	else if ( cmd.name()=="INT" ) {
		mpz_class lo = eval(cmd.args[0]);
		mpz_class hi = eval(cmd.args[1]);
		mpz_class x(lo + gmp_rnd.get_z_range(hi - lo + 1));

		if ( cmd.nargs()>=3 ) {
			// Check if we have a preset value, then override the
			// random generated value
			value_t y = getvar(cmd.args[2],1);
			if ( !boost::get<value_none>(&y.val) ) {
				x = y;
				if ( x<lo || x>hi ) {
					error("preset value for '" + string(cmd.args[2]) + "' out of range");
				}
			}
			setvar(cmd.args[2],value_t(x));
		}

		datastream << x.get_str();
	}

	else if ( cmd.name()=="FLOAT" ) {
		mpf_class lo = eval(cmd.args[0]);
		mpf_class hi = eval(cmd.args[1]);

		// Safe old I/O format flags to restore later
		ios_base::fmtflags flg = datastream.flags();
		if ( cmd.nargs()>=4 ) {
			if ( cmd.args[3].name()=="SCIENTIFIC" ) datastream << scientific;
			else if ( cmd.args[3].name()=="FIXED" ) datastream << fixed;
			else {
				cerr << "invalid option in " << program[prognr] << endl;
				exit(exit_failure);
			}
		}

		mpf_class x(lo + gmp_rnd.get_f()*(hi-lo));

		if ( cmd.nargs()>=3 ) {
			// Check if we have a preset value, then override the
			// random generated value
			value_t y = getvar(cmd.args[2],1);
			if ( !boost::get<value_none>(&y.val) ) {
				x = y;
				if ( x<lo || x>hi ) {
					error("preset value for '" + string(cmd.args[2]) + "' out of range");
				}
			}
			setvar(cmd.args[2],value_t(x));
		}

		datastream << x;

		// Restore saved I/O formatting
		datastream.flags(flg);
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
}

void checktoken(command cmd)
{
	currcmd = cmd;
	debug("checking token %s at %lu,%lu",
	      cmd.name().c_str(),(unsigned long)linenr,(unsigned long)charnr);

	if ( cmd.name()=="SPACE" ) checkspace();

	else if ( cmd.name()=="NEWLINE" ) checknewline();

	else if ( cmd.name()=="INT" ) {
		// Accepts format (0|-?[1-9][0-9]*), i.e. no leading zero's
		// and no '-0' accepted.
		string num;
		size_t len = 0;
		while ( datanr<data.size() &&
		        (isdigit(data[datanr+len]) ||
		         (num.size()==0 && data[datanr+len]=='-')) ) {
			num += data[datanr+len];
			len++;
		}

		mpz_class lo = eval(cmd.args[0]);
		mpz_class hi = eval(cmd.args[1]);

//		debug("%s <= %s <= %s",lo.get_str().c_str(),num.c_str(),hi.get_str().c_str());
		if ( cmd.nargs()>=3 ) debug("'%s' = '%s'",cmd.args[2].c_str(),num.c_str());

		if ( num.size()==0 ) error();
		if ( num.size()>=2 && num[0]=='0' ) error("prefix zero(s)");
		if ( num.size()>=1 && num[0]=='-' &&
		     (num.size()==1 || num[1]=='0') ) error("invalid minus sign (-0 not allowed)");

		mpz_class x(num);

		if ( x<lo || x>hi ) error("value out of range");
		if ( cmd.nargs()>=3 ) setvar(cmd.args[2],value_t(x));

		datanr += len;
		charnr += len;
	}

	else if ( cmd.name()=="FLOAT" ) {
		// Accepts format -?[0-9]+(\.[0-9]+)?([eE][+-]?[0-9]+)?
		// where the last optional part, the exponent, is not allowed
		// with the FIXED option and required with the SCIENTIFIC option.

		string float_regex("-?[0-9]+(\\.[0-9]+)?([eE][+-]?[0-9]+)?");
		string fixed_regex("-?[0-9]+(\\.[0-9]+)?");
		string scien_regex("-?[0-9]+(\\.[0-9]+)?[eE][+-]?[0-9]+");

		regex regexstr(float_regex);
		match_results<string::const_iterator> res;
		string matchstr;

		if ( cmd.nargs()>=4 ) {
			if ( cmd.args[3].name()=="SCIENTIFIC" ) regexstr = scien_regex;
			else if ( cmd.args[3].name()=="FIXED" ) regexstr = fixed_regex;
			else {
				cerr << "invalid option in " << program[prognr] << endl;
				exit(exit_failure);
			}
		}

		if ( !regex_search((string::const_iterator)&data[datanr],
		                   (string::const_iterator)data.end(),
		                   res,regexstr,regex_constants::match_continuous) ) {
			error();
		}
		size_t matchend = size_t(res[0].second-data.begin());
		matchstr = string(data.begin()+datanr,data.begin()+matchend);

		mpf_class x(matchstr,4*matchstr.length());

		mpf_class lo = eval(cmd.args[0]);
		mpf_class hi = eval(cmd.args[1]);

		if ( x<lo || x>hi ) error("value out of range");
		if ( cmd.nargs()>=3 ) setvar(cmd.args[2],value_t(x));

		charnr += matchend - datanr;
		datanr = matchend;
	}

	else if ( cmd.name()=="STRING" ) {
		string str = eval(cmd.args[0]).getstr();
		for (size_t i=0; i<str.size(); i++) {
			if ( datanr>=data.size() ) error("premature end of file");
			if ( data[datanr++]!=str[i] ) error();
			charnr++;
			if ( str[i]=='\n' ) linenr++, charnr=0;
		}

		debug("'%s' = '%s'",str.c_str(),cmd.args[0].c_str());
	}

	else if ( cmd.name()=="REGEX" ) {
		string str = eval(cmd.args[0]).getstr();
		regex regexstr(str);
		match_results<string::const_iterator> res;
		string matchstr;

		if ( !regex_search((string::const_iterator)&data[datanr],
		                   (string::const_iterator)data.end(),
		                   res,regexstr,regex_constants::match_continuous) ) {
			error();
		} else {
			size_t matchend = size_t(res[0].second-data.begin());
			matchstr = string(data.begin()+datanr,data.begin()+matchend);
			for (; datanr<matchend; datanr++) {
				charnr++;
				if ( data[datanr]=='\n' ) linenr++, charnr=0;
			}
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
}

// This function processes the outer control structure commands both
// for checking and generating testdata (as determined by the global
// variable 'gendata').
void checktestdata(ostream &datastream)
{
	datastream << setprecision(float_precision);

	while ( true ) {
		command cmd = currcmd = program[prognr];

		if ( cmd.name()=="EOF" ) {
			if ( gendata ) {
				debug("done: EOF found");
				return;
			} else {
				debug("checking EOF");
				if ( datanr++!=data.size() ) error();
				throw eof_found_exception();
			}
		}

		else if ( loop_cmds.count(cmd.name()) ) {

			// Current and maximum loop iterations.
			unsigned long i = 0, times = ULONG_MAX;
			unsigned loopvar = 0; // Optional variable for loop iteration present.

			if ( cmd.name()=="REPI" || cmd.name()=="WHILEI" ) {
				loopvar = 1;
				setvar(cmd.args[0],value_t(mpz_class(i)));
			}

			if ( cmd.name()=="REP" || cmd.name()=="REPI" ) {
				mpz_class n = eval(cmd.args[loopvar]);
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
				if ( loopvar ) setvar(cmd.args[0],value_t(mpz_class(i)));
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

	// Initialize random generators
	struct timeval time;
	unsigned long seed;
	gettimeofday(&time,NULL);
	seed = (time.tv_sec * 1000) + (time.tv_usec % 1000);
	srandom(seed);
	gmp_rnd.seed(seed);

	// Initialize current position in program and data.
	linenr = charnr = 0;
	datanr = prognr = 0;
	extra_ws = 0;
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
	if ( whitespace_ok ) readwhitespace();

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
