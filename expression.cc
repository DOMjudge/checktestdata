#include "expression.h"

#include <boost/preprocessor/list/for_each.hpp>
#include <cstdlib>
#include <exception>
#include <mutex>

#include "absl/strings/str_cat.h"

namespace {

Value parseInteger(const std::string& literal) {
  try {
    return Value{int64_t{stoll(literal)}};
  } catch (...) {
    return Value{boost::multiprecision::mpz_int{literal}};
  }
}

Value parseFloat(const std::string& literal) {
  return Value{double{stod(literal)}};
}

// "Leaks" all string constants contained in input programs. That is not a
// beautiful thing to do, but doesn't matter in practise, and because strings
// only ever come from input or the program, this enables all of them to be a
// string_view.
std::mutex mutex;
std::set<std::string> global_string_pool;

Value parseString(const std::string& literal) {
  std::string result;
  for (int i = 1; i < (int)literal.size() - 1; ++i) {
    char c = literal[i];
    if (c != '\\') {
      result += c;
    } else {
      i += 1;
      char c2 = literal[i];
      if (c2 >= '0' && c2 <= '9') {
        i++;
        char c3 = literal[i];
        i++;
        char c4 = literal[i];
        result += static_cast<char>(c4 + 8 * (c3 + 8 * c2));
      } else {
        switch (c2) {
          case '\\':
            result += '\\';
            break;
          case 'n':
            result += '\n';
            break;
          case 't':
            result += '\t';
            break;
          case 'r':
            result += '\r';
            break;
          case 'b':
            result += '\b';
            break;
          case '\n':
            break;
          default:
            throw std::logic_error(literal);
        }
      }
    }
  }
  std::lock_guard guard(mutex);
  return Value{*global_string_pool.emplace(std::move(result)).first};
}

Value parseToken(const antlr4::Token* literal) {
  if (literal->getType() == checktestdataParser::INTEGER_LITERAL) {
    return parseInteger(literal->getText());
  } else if (literal->getType() == checktestdataParser::FLOAT_LITERAL) {
    return parseFloat(literal->getText());
  } else if (literal->getType() == checktestdataParser::STRING_LITERAL) {
    return parseString(literal->getText());
  }
  throw std::logic_error(literal->toString());
}
}  // namespace

Expression::Expression(Value v)
    : value_(std::move(v)), eval_(&Expression::constEval) {}

Expression::Expression(const antlr4::Token* literal)
    : Expression(parseToken(literal)) {}

Expression::Expression(const antlr4::Token* binop, Expression a, Expression b)
    : children_({std::move(a), std::move(b)}) {
#define LOOP_BODY(r, data, elem)                            \
  else if (binop->getType() == checktestdataParser::elem) { \
    eval_ = &Expression::foldConst<                         \
        &Expression::binopEval<checktestdataParser::elem>>; \
  }
  if (false) {
  }
  BOOST_PP_LIST_FOR_EACH(
      LOOP_BODY, _,
      (PLUS,
       (MINUS,
        (MULT,
         (DIV,
          (MOD,
           (POW,
            (EQ, (NE, (LT, (GT, (GE, (LE, (AND, (OR, BOOST_PP_NIL)))))))))))))))
  else {
    throw std::invalid_argument(
        absl::StrCat(binop->getText(), " (", binop->getType(),
                     ") is not a valid binary operator"));
  }
#undef LOOP_BODY
}

Expression::Expression(const antlr4::Token* unop, Expression e) {
  std::string op = unop->getText();
  if (op == "(") {
    *this = std::move(e);
  } else if (op == "-" || op == "!") {
    children_ = {std::move(e)};
    eval_ = (op == "-") ? &Expression::foldConst<&Expression::unaryMinusEval>
                        : &Expression::foldConst<&Expression::unaryNotEval>;
  } else {
    throw std::invalid_argument(op + " is not a valid unary operator");
  }
}

Expression::Expression(const antlr4::Token* function,
                       std::vector<Expression> arguments) {
  switch (function->getType()) {
    case checktestdataParser::UNIQUE:
      for (const auto& expression : arguments) {
        variables_.push_back(expression.variable_);
      }
      eval_ = &Expression::uniqueEval;
      break;
    case checktestdataParser::INARRAY:
      variable_ = arguments[0].variable_;
      children_ = {arguments[1]};
      eval_ = &Expression::inarrayEval;
      break;
    case checktestdataParser::STRLEN:
      children_ = {arguments[0]};
      eval_ = &Expression::foldConst<&Expression::strlenEval>;
      break;
    default:
      throw std::logic_error{"unimplemented function " + function->getText()};
  }
}

std::pair<bool, const Value*> Expression::strlenEval() {
  auto res = children_[0].callEval();
  value_ = Value{
      static_cast<int64_t>(std::get<Value::string>(res.second->value_).size())};
  return {res.first, &value_};
}

std::pair<bool, const Value*> Expression::inarrayEval() {
  Value v = children_[0].eval();
  value_ = Value{variable_->inArray(v)};
  return {false, &value_};
}

std::pair<bool, const Value*> Expression::uniqueEval() {
  value_ = Value{Variable::UniqueVars(variables_)};
  return {false, &value_};
}

template <size_t size>
Expression::EvalFn Expression::getVariableEval(size_t i) {
  if (i == size) return &Expression::variableArrayEval<size>;
  return getVariableEval<size - 1>(i);
}

template <>
Expression::EvalFn Expression::getVariableEval<0>(size_t) {
  throw std::runtime_error("Array dimensions greater 10 are not supported");
}

template <size_t size>
Expression::AssignFn Expression::getVariableAssignment(size_t i) {
  if (i == size) return &Expression::variableArrayAssignment<size>;
  return getVariableAssignment<size - 1>(i);
}

template <>
Expression::AssignFn Expression::getVariableAssignment<0>(size_t) {
  throw std::runtime_error("Array dimensions greater 10 are not supported");
}

Expression::Expression(Variable* v, std::vector<Expression> idx)
    : variable_(v),
      eval_(idx.empty() ? &Expression::variableEval
                        : getVariableEval<Variable::kMaxDim>(idx.size())),
      assign_(idx.empty()
                  ? &Expression::variableAssignment
                  : getVariableAssignment<Variable::kMaxDim>(idx.size())),
      children_(std::move(idx)) {}

std::pair<bool, const Value*> Expression::callEval() {
  return (this->*eval_)();
}
std::pair<bool, const Value*> Expression::invalid() {
  throw std::logic_error("invalid expression state");
}
std::pair<bool, const Value*> Expression::constEval() {
  return {true, &value_};
}
template <int op>
std::pair<bool, const Value*> Expression::binopEval() {
  auto e1 = children_[0].callEval();
  if ((op == checktestdataParser::OR && std::get<bool>(e1.second->value_)) ||
      (op == checktestdataParser::AND && !std::get<bool>(e1.second->value_)))
    return e1;
  auto e2 = children_[1].callEval();
  // std::cerr << "binop: " << e1.second->str() << ' ' << op << ' '
  //          << e2.second->str() << '\n';
  constexpr auto binop = &Value::binop<op>;
  value_ = (e1.second->*binop)(*e2.second);
  // std::cerr << "binop " << op << " returned " << value_.str() << '\n';
  return {e1.first && e2.first, &value_};
}
std::pair<bool, const Value*> Expression::unaryMinusEval() {
  auto result = children_[0].callEval();
  value_ = result.second->negate();
  // std::cerr << "minus returned " << value_.str() << '\n';
  return {result.first, &value_};
}
std::pair<bool, const Value*> Expression::unaryNotEval() {
  auto result = children_[0].callEval();
  value_ = Value{!std::get<bool>(result.second->value_)};
  return {result.first, &value_};
}
void Expression::invalidAssignment(const Value&) {
  throw std::logic_error("invalid assignment, this should not be possible");
}

void Expression::variableAssignment(const Value& value) {
  variable_->set(value);
}
std::pair<bool, const Value*> Expression::variableEval() {
  return {false, &variable_->get()};
}
