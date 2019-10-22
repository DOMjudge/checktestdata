#include <cstdio>
#include <iomanip>
#include <iostream>
#include <iterator>
#include <map>
#include <memory>
#include <optional>
#include <thread>

#include "absl/strings/numbers.h"
#include "re2/re2.h"

#include "command.h"
#include "expression.h"
#include "loop.h"
#include "stream.h"
#include "variable.h"

#include "checktestdataBaseListener.h"
#include "checktestdataLexer.h"
#include "checktestdataParser.h"

using namespace antlr4;

class InputMismatch : public std::exception {
 public:
  InputMismatch() = default;
  InputMismatch(const std::string& reason) : reason_(reason) {}
  const char* what() const noexcept override {
    if (reason_.empty()) return "Input mismatch";
    return reason_.c_str();
  }

 private:
  std::string reason_;
};

const static std::map<int, int> token_to_char{
    {checktestdataParser::EOF_COMMAND, std::char_traits<char>::eof()},
    {checktestdataParser::NEWLINE, '\n'},
    {checktestdataParser::SPACE, ' '},
};
const static std::map<int, std::string> print_token{
    {std::char_traits<char>::eof(), "EOF"},
    {'\n', "NEWLINE"},
    {' ', "SPACE"},
};

class SimpleCommand : public Command {
 public:
  SimpleCommand(int token) : expected_(token_to_char.at(token)) {}
  void run(std::string_view& in) override {
    if (peek(in) == expected_) {
      get(in);
    } else {
      throw std::logic_error{"expected " + print_token.at(expected_)};
    }
    next_->run(in);
  }

 private:
  int expected_;
};

class SuccessCommand : public Command {
  void run(std::string_view&) {}
};

class Reader : public Command {
 public:
  Reader(checktestdataParser::ReadContext* ctx, std::optional<Expression> min,
         std::optional<Expression> max, std::optional<Expression> mindecimals,
         std::optional<Expression> maxdecimals,
         std::optional<Expression> variable, std::optional<Expression> value,
         checktestdataParser::FloatoptContext* floatopt)
      : min_(min),
        max_(max),
        mindecimals_(mindecimals),
        maxdecimals_(maxdecimals),
        variable_(variable) {
    if (floatopt)
      scientific_ =
          floatopt->format->getType() == checktestdataParser::SCIENTIFIC;
    switch (ctx->type->getType()) {
      case checktestdataParser::INT:
        read_ = &Reader::readInt;
        break;
      case checktestdataParser::FLOAT:
      case checktestdataParser::FLOATP:
        read_ = &Reader::readFloat;
        break;
      case checktestdataParser::STRING:
        string_ = std::move(*value);
        read_ = &Reader::readString;
        break;
      case checktestdataParser::REGEX:
        string_ = std::move(*value);
        read_ = &Reader::readRegex;
        break;
      default:
        throw std::logic_error("unimplemented read statement " +
                               std::to_string(ctx->type->getType()));
    }
  }

  void run(std::string_view& in) override {
    (this->*read_)(in);
    next_->run(in);
  }

 private:
  Expression string_;
  Value::string last_string_value_;
  std::optional<RE2> regex_;

  void readString(std::string_view& in) {
    const auto& s = std::get<Value::string>(string_.eval().value_);
    if (in.substr(0, s.size()) == s) {
      in.remove_prefix(s.size());
    }
    if (variable_) {
      variable_->assign(Value{s});
    }
  }

  void readRegex(std::string_view& in) {
    const auto& s = std::get<Value::string>(string_.eval().value_);
    if (!regex_ || s != last_string_value_) {
      last_string_value_ = s;
      regex_.emplace(s);
    }
    const char* before = in.data();
    if (!RE2::Consume(&in, *regex_)) {
      throw InputMismatch{absl::StrCat("did not match regex ",
                                       regex_->pattern(), " on \"", in, "\"")};
    }
    if (variable_) {
      variable_->assign(Value{Value::string(before, in.data() - before)});
    }
  }

  void readInt(std::string_view& in) {
    const char* before = in.data();
    // static const RE2 regex(R"(0|-?[1-9][0-9]*)");
    bool fail = true;
    if (peek(in) == '0') {
      fail = false;
      get(in);
    } else {
      if (peek(in) == '-') get(in);
      fail = true;
      if (peek(in) >= '1' && peek(in) <= '9') {
        fail = false;
        get(in);
      }
      while (peek(in) >= '0' && peek(in) <= '9') {
        get(in);
      }
    }
    if (fail) throw InputMismatch{};
    std::string_view result(before, in.data() - before);
    Value value;
    if (int64_t small_value; absl::SimpleAtoi(result, &small_value)) {
      value = Value{small_value};
    } else {
      value = Value{Value::integer{(std::string)result}};
    }
    if (value < min_->eval() || max_->eval() < value) {
      std::cerr << (value < min_->eval()) << ' ' << (max_->eval() < value)
                << '\n';
      throw InputMismatch{};
    }
    if (variable_) variable_->assign(value);
  }

  void readFloat(std::string_view& in) {
    const char* before = in.data();
    // static const RE2 regex(R"(-?[0-9]+(\.[0-9]*)?([eE][+-]?[0-9]+)?)");
    if (peek(in) == '-') get(in);
    bool fail = true;
    int decimals = 0;
    int digits = 0;
    bool scientific = false;
    while (peek(in) >= '0' && peek(in) <= '9') {
      fail = false;
      get(in);
      ++digits;
    }
    if (!fail && peek(in) == '.') {
      get(in);
      while (peek(in) >= '0' && peek(in) <= '9') {
        get(in);
        ++decimals;
      }
    }
    if (!fail && (peek(in) == 'e' || peek(in) == 'E')) {
      scientific = true;
      get(in);
      if (peek(in) == '+' || peek(in) == '-') get(in);
      fail = true;
      while (peek(in) >= '0' && peek(in) <= '9') {
        fail = false;
        get(in);
      }
    }
    if (mindecimals_) {
      int64_t mindecimals = mindecimals_->eval().toInt<uint32_t>();
      int64_t maxdecimals = maxdecimals_->eval().toInt<uint32_t>();
      if (scientific && (digits != 1 || before[0] == '0')) {
        throw std::logic_error{
            "In the FLOATP case a floating point number in scientific notation "
            "must have exactly one nonzero digit before the decimal point."};
      }
      if (mindecimals > decimals || maxdecimals < decimals) {
        throw std::logic_error{"wrong number of decimal places in input"};
      }
    }
    if (scientific_ && *scientific_ != scientific) {
      throw std::logic_error{"wrong float format"};
    }
    if (fail) throw InputMismatch{};
    std::string_view result(before, in.data() - before);
    Value value;
    if (double double_value; absl::SimpleAtod(result, &double_value)) {
      value = Value{double_value};
    } else {
      throw InputMismatch{};
    }
    if (value < min_->eval() || max_->eval() < value) {
      std::cerr << (value < min_->eval()) << ' ' << (max_->eval() < value)
                << '\n';
      throw InputMismatch{};
    }
    if (variable_) variable_->assign(value);
  }

  using ReadFn = decltype(&Reader::readFloat);

  ReadFn read_ = nullptr;
  std::optional<Expression> min_;
  std::optional<Expression> max_;
  std::optional<Expression> mindecimals_;
  std::optional<Expression> maxdecimals_;
  std::optional<Expression> variable_;
  std::optional<bool> scientific_;
};

class AssertCommand : public Command {
 public:
  AssertCommand(Expression expr) : expr_(std::move(expr)) {}
  void run(std::string_view& in) override {
    if (!std::get<bool>(expr_.eval().value_)) {
      throw InputMismatch{};
    }
    next_->run(in);
  }

 private:
  Expression expr_;
};

class Assignment : public Command {
 public:
  Assignment(Expression lvalue, Expression rvalue)
      : lvalue_(std::move(lvalue)), rvalue_(std::move(rvalue)) {}
  void run(std::string_view& in) override {
    lvalue_.assign(rvalue_.eval());
    next_->run(in);
  }

 private:
  Expression lvalue_, rvalue_;
};

class Unassignment : public Command {
 public:
  Unassignment(Variable* var) : var_(var) {}
  void run(std::string_view& in) override {
    var_->reset();
    next_->run(in);
  }

 private:
  Variable* var_;
};

class If : public Command {
 public:
  Command* then_;
  Command* otherwise_;
  Expression condition_;
  void run(std::string_view& in) {
    if (std::get<bool>(condition_.eval().value_))
      then_->run(in);
    else
      otherwise_->run(in);
    next_->run(in);
  }
};

class IsEOF : public Command {
 public:
  IsEOF(Variable* var) : var_(var) {}
  void run(std::string_view& in) {
    var_->set(Value{in.empty()});
    next_->run(in);
  }

 private:
  Variable* var_;
};

class Listener : public checktestdataBaseListener {
 public:
  Listener() {
    program_.emplace_back(new Command);
    current_ = &program_[0]->next_;
  }

  void run(std::string_view& in) {
    input_ = &in;
    program_[0]->run(in);
  }

 private:
  std::vector<If*> ifstack_;
  void enterIf_(checktestdataParser::If_Context*) override {
    ifstack_.push_back(&append<If>());
    current_ = &ifstack_.back()->then_;
  }

  void enterElse_(checktestdataParser::Else_Context*) override {
    append<SuccessCommand>();
    current_ = &ifstack_.back()->otherwise_;
  }

  void exitElse_(checktestdataParser::Else_Context*) override {
    append<SuccessCommand>();
    ifstack_.back()->condition_ = pop_expr();
    current_ = &ifstack_.back()->next_;
    ifstack_.pop_back();
  }

  void enterSimpleCommand(
      checktestdataParser::SimpleCommandContext* ctx) override {
    append<SimpleCommand>(ctx->getStart()->getType());
  }

  void exitAssignment(checktestdataParser::AssignmentContext*) override {
    Expression rvalue = pop_expr();
    Expression lvalue = pop_expr();
    append<Assignment>(lvalue, rvalue);
  }

  void enterUnassignment(
      checktestdataParser::UnassignmentContext* ctx) override {
    append<Unassignment>(&variables_[ctx->VARNAME()->getText()]);
  }

  void exitRead(checktestdataParser::ReadContext* ctx) override {
    std::optional<Expression> variable;
    if (ctx->varopt()) variable = pop_expr();
    std::optional<Expression> maxdecimals;
    if (ctx->maxdecimals) maxdecimals = pop_expr();
    std::optional<Expression> mindecimals;
    if (ctx->mindecimals) mindecimals = pop_expr();
    std::optional<Expression> max;
    if (ctx->max) max = pop_expr();
    std::optional<Expression> min;
    if (ctx->min) min = pop_expr();
    std::optional<Expression> value;
    if (ctx->value) value = pop_expr();
    append<Reader>(ctx, min, max, mindecimals, maxdecimals, variable, value,
                   ctx->floatopt());
  }

  void exitMain(checktestdataParser::MainContext*) override {
    append<SimpleCommand>(checktestdataParser::EOF_COMMAND);
    append<SuccessCommand>();
  }

  void exitAssrt(checktestdataParser::AssrtContext* ctx) override {
    auto expr = pop_expr();
    append<AssertCommand>(std::move(expr));
  }

  void exitExpr(checktestdataParser::ExprContext* ctx) override {
    if (ctx->binop) {
      auto e2 = pop_expr();
      auto e1 = pop_expr();
      expr_stack_.emplace_back(ctx->binop, std::move(e1), std::move(e2));
    } else if (ctx->unop) {
      auto e = pop_expr();
      expr_stack_.emplace_back(ctx->unop, std::move(e));
    } else if (ctx->literal) {
      expr_stack_.emplace_back(ctx->literal);
    } else if (ctx->variable()) {
      // handled in exitVariable
    } else if (ctx->function()) {
      switch (ctx->function()->getStart()->getType()) {
        case checktestdataParser::ISEOF: {
          expr_stack_.emplace_back(&input_);
          break;
        }
        case checktestdataParser::MATCH: {
          expr_stack_.emplace_back(&input_, pop_expr());
          break;
        }
        default: {
          std::vector<Expression> arguments;
          for (auto* token : ctx->function()->VARNAME()) {
            Variable* variable = &variables_[token->getText()];
            arguments.emplace_back(variable, std::vector<Expression>{});
          }
          if (ctx->function()->expr()) arguments.push_back(pop_expr());
          expr_stack_.emplace_back(ctx->function()->getStart(), arguments);
        }
      }
    } else {
      throw std::logic_error(ctx->toStringTree());
    }
  }

  void exitVariable(checktestdataParser::VariableContext* ctx) override {
    std::vector<Expression> idx;
    for (int i = 0; i < (int)ctx->expr().size(); ++i) {
      idx.push_back(pop_expr());
    }
    std::reverse(begin(idx), end(idx));
    Variable* variable = &variables_[ctx->VARNAME()->getText()];
    expr_stack_.emplace_back(variable, idx);
  }

  void enterLoop(checktestdataParser::LoopContext* ctx) override {
    if (ctx->count) {
      loop_stack_.push_back(&append<ForLoop>());
    }
    if (ctx->condition) {
      loop_stack_.push_back(&append<WhileLoop>());
    }
  }
  void exitLoop(checktestdataParser::LoopContext* ctx) override {
    Loop* loop = loop_stack_.back();
    loop_stack_.pop_back();
    append<SuccessCommand>();
    current_ = &loop->after_;
    *(loop->separator_ ? &loop->separator_->next_ : &loop->separator_) =
        loop->next_;
    if (ctx->count) {
      static_cast<ForLoop*>(loop)->count_ = pop_expr();
    }
    if (ctx->condition) {
      static_cast<WhileLoop*>(loop)->condition_ = pop_expr();
    }
    if (ctx->i) {
      loop->i_ = pop_expr();
    }
  }
  void enterLoopsep(checktestdataParser::LoopsepContext*) override {
    current_ = &loop_stack_.back()->separator_;
  }
  void exitLoopsep(checktestdataParser::LoopsepContext*) override {
    current_ = &loop_stack_.back()->next_;
  }
  template <typename T, typename... Args>
  T& append(Args&&... args) {
    T* result = static_cast<T*>(
        program_.emplace_back(new T(std::forward<Args>(args)...)).get());
    *current_ = result;
    current_ = &result->next_;
    return *result;
  }
  std::vector<std::unique_ptr<Command>> program_;
  std::vector<Expression> expr_stack_;
  std::vector<Loop*> loop_stack_;
  Expression pop_expr() {
    auto res = std::move(expr_stack_.back());
    expr_stack_.pop_back();
    return res;
  }
  std::unordered_map<std::string, Variable> variables_;
  std::string_view* input_;
  Command** current_;
};

int main(int argc, char** argv) {
  std::ios::sync_with_stdio(false);
  std::cin.tie(nullptr);
  if (argc < 2) {
    throw std::runtime_error{
        absl::StrCat("Too few input arguments to ", argv[0])};
  }
  std::ifstream program_input(argv[1]);
  if (!program_input) {
    throw std::runtime_error{
	    absl::StrCat("Error reading program file '", argv[1], "'.")
    };
  }
  ANTLRInputStream input(program_input);
  checktestdataLexer lexer(&input);
  CommonTokenStream tokens(&lexer);
  checktestdataParser parser(&tokens);
  tree::ParseTree* tree = parser.main();
  if (!lexer.hitEOF || parser.getNumberOfSyntaxErrors()) {
    throw std::runtime_error{absl::StrCat("Failed to compile program file ",
                                          lexer.hitEOF, " ",
                                          parser.getNumberOfSyntaxErrors())};
  }
  // std::cerr << tree->toStringTree(&parser) << std::endl;
  std::vector<std::thread> threads;
  for (int i = 2; i == 2 || i < argc; ++i) {
    threads.emplace_back([tree, i, argc, argv]() {
      Listener listener;
      tree::ParseTreeWalker treeWalker;
      treeWalker.walk(&listener, tree);
      std::string s;
      if (i >= argc || argv[i] == std::string("-")) {
        s = std::string(std::istreambuf_iterator<char>(std::cin), {});
      } else {
        std::ifstream input_file(argv[i]);
        if (!input_file) {
          throw std::runtime_error{
	    absl::StrCat("Error reading input file '", argv[i], "'.")
	  };
        }
        s = std::string(std::istreambuf_iterator<char>(input_file), {});
      }
      std::string_view stream(s);
      try {
        listener.run(stream);
      } catch (const std::exception& e) {
        std::cerr << e.what() << '\n';
        std::quick_exit(1);
      }
    });
  }
  for (auto& thread : threads) thread.join();

  std::quick_exit(0);
}
