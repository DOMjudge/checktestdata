#include <string_view>

inline int get(std::string_view& s) {
  if (s.empty()) {
    return std::char_traits<char>::eof();
  }
  s.remove_prefix(1);
  return s.front();
}

inline int peek(std::string_view& s) {
  if (s.empty()) {
    return std::char_traits<char>::eof();
  }
  return s.front();
}
