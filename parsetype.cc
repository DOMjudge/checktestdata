#include "parsetype.hpp"

std::ostream &operator<<(std::ostream &out, const parse_t &obj)
{
	char op = obj.op;

	// '#' should never be output as operator
	switch ( op ) {
	case 'I':
	case 'F':
	case 'f':
	case ' ': out << obj.val;   op = ','; break;
	case 'n': out << '-';       op = '#'; break;
	case '!': out << '!';       op = '#'; break;
	case '(':                   op = '#'; break;
	case 'E': out << "ISEOF";   op = '#'; break;
	case 'M': out << "MATCH";   op = '#'; break;
	case 'U': out << "UNIQUE";  op = ','; break;
	case 'A': out << "INARRAY"; op = ','; break;
	}

	// Special case quote strings
	if ( op=='S' ) return out << '"' << obj.val << '"';

	// Special case compare operators, as these are not stored in 'op'
	if ( op=='?' ) {
		if ( obj.nargs()!=2 ) return out << "#error in compare#";
		out << obj.args[0] << obj.val << obj.args[1];
		return out;
	}

	// Special case array variable using []
	if ( op=='v' ) {
		out << obj.val;
		if ( obj.nargs()>0 ) {
			out << '[' << obj.args[0];
			for(size_t i=1; i<obj.nargs(); i++) out << ',' << obj.args[i];
			out << ']';
		}
		return out;
	}

	// Special case variable assignment
	if ( op=='a' ) {
		out << obj.args[0] << '=' << obj.args[1];
		return out;
	}

	if ( obj.nargs()>0 ) {
		out << '(' << obj.args[0];
		for(size_t i=1; i<obj.nargs(); i++) out << op << obj.args[i];
		out << ')';
	}
    return out;
}

namespace checktestdata {

std::ostream& operator<<(std::ostream& os, const none_t&) {
	return os << "<no value>";
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

std::ostream& operator <<(std::ostream &os, const value_t &val)
{
	return os << val.val;
}

std::string value_t::getstr() const
{
	return boost::get<std::string>(val);
}

std::string value_t::tostr() const
{
	std::stringstream ss;
	ss << *this;
	return ss.str();
}

} // namespace checktestdata
