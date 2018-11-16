#include "loop.h"

void WhileLoop::run(std::string_view& in) {
  if (next_) {
    int64_t i;
    for (i = 0; std::get<bool>(condition_.eval().value_); ++i) {
      set_i(i);
      if (i)
        separator_->run(in);
      else
        next_->run(in);
    }
    set_i(i);
  }
  after_->run(in);
}

void ForLoop::run(std::string_view& in) {
  if (next_) {
    Value v = count_.eval();
    int64_t count = v.toInt<uint32_t>();
    int64_t i;
    for (i = 0; i < count; ++i) {
      set_i(i);
      if (i)
        separator_->run(in);
      else
        next_->run(in);
    }
    set_i(i);
  }
  after_->run(in);
}
