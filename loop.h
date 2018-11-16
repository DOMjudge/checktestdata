#pragma once

#include <string_view>

#include "command.h"
#include "expression.h"

class Loop : public Command {
 public:
  Command* after_ = nullptr;
  Command* separator_ = nullptr;
  std::optional<Expression> i_;

 protected:
  void set_i(int64_t i) {
    if (i_) i_->assign(Value{i});
  }
};

class WhileLoop : public Loop {
  void run(std::string_view& in) override;

 public:
  Expression condition_;
};

class ForLoop : public Loop {
  void run(std::string_view& in) override;

 public:
  Expression count_;
};
