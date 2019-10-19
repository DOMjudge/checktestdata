/*
   Libchecktestdata -- check testdata according to specification.

   It's licensed under the 2-clause BSD license, see the file COPYING.

   Input data buffer, wraps a string.
*/

#include <string>
#include <cctype>

namespace checktestdata {

int isspace_notnewline(char c) { return isspace(c) && c!='\n'; }

class databuffer {
private:
	std::string data;
	size_t _pos = 0, _line = 0, _lpos = 0;

public:
	databuffer() {}
	databuffer(const std::string& _data): data(_data) {}
	databuffer(std::string&& _data): data(std::move(_data)) {}

	bool eof() const { return _pos >= data.size(); }

	size_t size() const { return data.size(); }

	size_t pos()  const { return _pos; }
	size_t line() const { return _line; }
	size_t lpos() const { return _lpos; }

	std::string::const_iterator curr()  const { return data.cbegin()+_pos; }
	std::string::const_iterator begin() const { return data.cbegin(); }
	std::string::const_iterator end()   const { return data.cend(); }

	std::string next(size_t length=1) const
	{
		size_t end = std::min(size(),_pos+length);
		return data.substr(_pos,end-_pos);
	}
	std::string prev(size_t length=1) const
	{
		size_t start = std::max(0LL,(long long)_pos-(long long)length);
		return data.substr(start,_pos-start);
	}

	char peek(size_t ahead=0) const
	{
		if ( _pos+ahead>=size() ) return char();
		return data[_pos+ahead];
	}

	char readchar() {
		char c = data[_pos++];
		if ( c=='\n' ) {
			_line++;
			_lpos = 0;
		} else {
			_lpos++;
		}
		return c;
	}

	void readwhitespace()
	{
		while ( !eof() && isspace_notnewline(data[_pos]) ) {
			_pos++;
			_lpos++;
		}
	}
};

} // namespace checktestdata
