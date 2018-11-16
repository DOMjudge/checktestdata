#pragma once

#include <any>
#include <boost/iterator/transform_iterator.hpp>
#include <vector>

#include "value.h"
#include "variable.h"

#include "checktestdataLexer.h"

class Expression {
 public:
  const Value& eval() { return *callEval().second; }
  void assign(const Value& v) { (this->*assign_)(v); }
  Expression() {}
  Expression(Value v);
  Expression(Variable* variable, std::vector<Expression> idx);
  Expression(const antlr4::Token* literal);
  Expression(const antlr4::Token* binop, Expression a, Expression b);
  Expression(const antlr4::Token* unop, Expression e);
  Expression(const antlr4::Token* function, std::vector<Expression> arguments);
  template <typename F,
            typename _ = std::enable_if_t<std::is_invocable_v<F>, void>>
  Expression(F f)
      : eval_(&Expression::foldConst<&Expression::backdoorEval<F>>),
        context_(std::move(f)) {}

 private:
  auto idx() {
    return boost::make_transform_iterator(
        children_.begin(), [](Expression& child) { return child.eval(); });
  }
  std::pair<bool, const Value*> callEval();
  std::pair<bool, const Value*> invalid();
  std::pair<bool, const Value*> constEval();
  using EvalFn = decltype(&Expression::constEval);
  template <EvalFn inner>
  std::pair<bool, const Value*> foldConst() {
    auto result = (this->*inner)();
    if (result.first) {
      *this = Expression{std::move(*result.second)};
    } else {
      eval_ = inner;
    }
    return callEval();
  }
  template <int op>
  std::pair<bool, const Value*> binopEval();
  std::pair<bool, const Value*> unaryMinusEval();
  std::pair<bool, const Value*> unaryNotEval();
  void invalidAssignment(const Value& value);
  using AssignFn = decltype(&Expression::invalidAssignment);
  void variableAssignment(const Value& value);
  std::pair<bool, const Value*> variableEval();
  std::pair<bool, const Value*> uniqueEval();
  std::pair<bool, const Value*> inarrayEval();
  std::pair<bool, const Value*> strlenEval();
  template <typename F>
  std::pair<bool, const Value*> backdoorEval() {
    return (*std::any_cast<F>(&context_))();
  }
  template <size_t size>
  void variableArrayAssignment(const Value& value) {
    variable_->set<size>(idx(), value);
  }
  template <size_t size>
  std::pair<bool, const Value*> variableArrayEval() {
    return {false, &variable_->get<size>(idx())};
  }

  template <size_t size>
  EvalFn getVariableEval(size_t i);

  template <size_t size>
  AssignFn getVariableAssignment(size_t i);

  Value value_;
  Variable* variable_;
  std::vector<const Variable*> variables_;
  EvalFn eval_ = &Expression::invalid;
  AssignFn assign_ = &Expression::invalidAssignment;
  std::vector<Expression> children_;
  std::any context_;
};
