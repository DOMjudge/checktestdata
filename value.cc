#include "value.h"

Value Value::negate() const {
  return std::visit(
      [](const auto& v) -> Value {
        using T = std::decay_t<decltype(v)>;
        if constexpr (!is_numeric<T>) {
          throw std::logic_error{"negate called on non numeric value"};
        } else if constexpr (is_integral<T>) {
          if (v >= std::numeric_limits<int64_t>::min() + 1 &&
              -v >= std::numeric_limits<int64_t>::min()) {
            return Value{int64_t{T{-v}}};
          } else {
            return Value{-integer{v}};
          }
        } else {
          return Value{-v};
        }
      },
      value_);
}
