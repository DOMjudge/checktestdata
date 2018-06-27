#pragma once
#include <cstdint>
#include <iostream>
#include <variant>

#include <boost/multiprecision/gmp.hpp>

#include "absl/strings/str_cat.h"
#include "checktestdataParser.h"

class Value {
 public:
  using integer = boost::multiprecision::mpz_int;

  template <typename T>
  static constexpr bool is_integral =
      std::is_same_v<T, int64_t> || std::is_same_v<T, integer> ||
      std::is_same_v<T, __int128>;
  template <typename T>
  static constexpr bool is_numeric =
      is_integral<T> || std::is_same_v<T, double>;

 private:
  template <typename T, typename U>
  using result_t =
      std::conditional_t<std::is_same_v<T, double> || std::is_same_v<U, double>,
                         double,
                         std::conditional_t<std::is_same_v<T, integer> ||
                                                std::is_same_v<U, integer>,
                                            integer, int64_t>>;
  template <typename T, typename U>
  using intermediate_t =
      std::conditional_t<std::is_same_v<result_t<T, U>, int64_t>, __int128,
                         result_t<T, U>>;

  template <typename T>
  static Value returnInt(const T& x) {
    if (std::numeric_limits<int64_t>::min() <= x &&
        x <= std::numeric_limits<int64_t>::max()) {
      return Value{(int64_t)x};
    } else if constexpr (std::is_same_v<T, __int128>) {
      integer result_lower = (int64_t)x;
      integer result_upper = (int64_t)(x >> 64);
      integer result = (result_upper << 64) + result_lower;
      return Value{result};
    } else {
      return Value{(integer)x};
    }
  }
  template <typename T, typename U>
  static Value powBinop(const T& a, const U& b) {
    if constexpr (is_integral<T>) {
      if (b >= std::numeric_limits<unsigned>::max() || b < 0) {
        throw std::invalid_argument("value not suitable for pow");
      }
      integer result = pow((integer)(a), (unsigned)b);
      return returnInt(result);
    } else {
      return Value{pow(a, (double)b)};
    }
  }

 public:
  template <int op>
  Value binop(const Value& o) const {
    return visit(
        [&](const auto& a, const auto& b) -> Value {
          using T = std::decay_t<decltype(a)>;
          using U = std::decay_t<decltype(b)>;
          if constexpr ((std::is_same_v<T, U> ||
                         (is_integral<T> && is_integral<U>))) {
            if constexpr (op == checktestdataParser::EQ)
              return Value{a == b};
            else if constexpr (op == checktestdataParser::NE)
              return Value{a != b};
          }
          if constexpr (std::is_same_v<T, bool> && std::is_same_v<U, bool>) {
            if constexpr (op == checktestdataParser::AND)
              return Value{a && b};
            else if constexpr (op == checktestdataParser::OR)
              return Value{a || b};
          }
          if constexpr (!is_numeric<T> || !is_numeric<U>) {
            throw std::invalid_argument("invalid operands");
          } else {
            if constexpr (op == checktestdataParser::EQ) {
              return Value{!(*this < o) && !(o < *this)};
            } else if constexpr (op == checktestdataParser::NE) {
              return Value{*this < o || o < *this};
            } else if constexpr (op == checktestdataParser::LE) {
              return Value{!(o < *this)};
            } else if constexpr (op == checktestdataParser::LT) {
              return Value{*this < o};
            } else if constexpr (op == checktestdataParser::GE) {
              return Value{!(*this < o)};
            } else if constexpr (op == checktestdataParser::GT) {
              return Value{o < *this};
            }
            using i_t = intermediate_t<T, U>;
            i_t result = static_cast<i_t>(a);
            if constexpr (op == checktestdataParser::PLUS) {
              result += (i_t)b;
            } else if constexpr (op == checktestdataParser::MINUS) {
              result -= (i_t)b;
            } else if constexpr (op == checktestdataParser::MULT) {
              result *= (i_t)b;
            } else if constexpr (op == checktestdataParser::DIV) {
              result /= (i_t)b;
            } else if constexpr (op == checktestdataParser::MOD) {
              if constexpr (is_integral<i_t>) {
                result %= (i_t)b;
              } else {
                result = std::fmod(result, (i_t)b);
              }
            } else if constexpr (op == checktestdataParser::POW) {
              return powBinop(a, b);
            } else {
              throw std::logic_error{"invalid binary operator " +
                                     std::to_string(op)};
            }
            if constexpr (is_integral<i_t>) {
              return returnInt(result);
            } else {
              return Value{result};
            }
          }
          return Value{};
        },
        value_, o.value_);
  }
  Value negate() const;
  template <typename T>
  T toInt() const {
    return visit(
        [&](const auto& v) -> T {
          using U = std::decay_t<decltype(v)>;
          if constexpr (std::is_same_v<U, integer>) {
            if (v >= std::numeric_limits<T>::min() &&
                v <= std::numeric_limits<T>::max()) {
              return (T)v;
            }
          } else if constexpr (std::is_same_v<U, int64_t>) {
            if (((std::is_signed_v<T> && v >= std::numeric_limits<T>::min()) ||
                 v >= 0) &&
                v <= std::numeric_limits<T>::max()) {
              return (T)v;
            }
          }
          throw std::invalid_argument(
              absl::StrCat("Value (", typeid(U).name(), ")", str(),
                           " not suitable for loop counter"));
        },
        value_);
  }
  bool operator<(const Value& o) const {
    return visit(
        [](const auto& a, const auto& b) -> bool {
          using T = std::decay_t<decltype(a)>;
          using U = std::decay_t<decltype(b)>;
          if constexpr (std::is_same_v<T, U> ||
                        (is_integral<T> && is_integral<U>)) {
            return a < b;
          } else if constexpr (!is_numeric<T> || !is_numeric<U>) {
            return typeid(T).before(typeid(U));
          } else {
            static_assert(std::is_same_v<T, double> ||
                          std::is_same_v<U, double>);
            if constexpr (std::is_same_v<T, int64_t> ||
                          std::is_same_v<U, int64_t>) {
              static_assert(LDBL_MANT_DIG >= 64,
                            "insufficient long double precision");
              return (long double)a < (long double)b;
            } else if constexpr (std::is_same_v<T, double>) {
              return (boost::multiprecision::mpq_rational)a < b;
            } else if constexpr (std::is_same_v<U, double>) {
              return a < (boost::multiprecision::mpq_rational)b;
            } else {
              throw std::logic_error("programmer error");
            }
          }
        },
        value_, o.value_);
  }
  bool operator==(const Value& o) const { return !(*this < o) && !(o < *this); }
  std::string str() const {
    std::stringstream ss;
    visit([&](const auto& v) { ss << v; }, value_);
    return ss.str();
  }

  struct invalid_value {
    friend std::ostream& operator<<(std::ostream& out, const invalid_value&) {
      out << "invalid value!";
      return out;
    }
    bool operator==(const invalid_value&) const {
      throw std::logic_error{"==: invalid values should never be compared"};
    }
    bool operator!=(const invalid_value&) const {
      throw std::logic_error{"!=: invalid values should never be compared"};
    }
    bool operator<(const invalid_value&) const {
      throw std::logic_error{"<: invalid values should never be compared"};
    }
  };

  bool invalid() { return std::holds_alternative<invalid_value>(value_); }

  using string = std::string_view;

  std::variant<invalid_value, int64_t, string, integer, double, bool> value_;
};
