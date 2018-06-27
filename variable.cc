#include "variable.h"

namespace {
template <typename T, typename _ = void> struct Varsize_t {
  static int64_t get(const T &) { return 1; }
  constexpr static bool constant = true;
};

template <typename T> struct Varsize_t<T, std::void_t<decltype(&T::size)>> {
  static int64_t get(const T &c) { return c.size(); }
  constexpr static bool constant = false;
};

template <typename T> int64_t Varsize(const T &c) {
  return Varsize_t<T>::get(c);
}

template <typename... T> struct is_same;

template <typename T> struct is_same<T> { static constexpr bool value = true; };

template <typename T, typename U, typename... V> struct is_same<T, U, V...> {
  static constexpr bool value = std::is_same_v<T, U> && is_same<T, V...>::value;
};

std::string keystr(int64_t x) { return std::to_string(x); }

std::string keystr(std::monostate x) { return "invalid value!"; }

std::string keystr(const Value &v) { return v.str(); }

template <std::size_t size, typename T>
std::string keystr(const std::array<T, size> &v) {
  std::string res;
  res += "(";
  for (int i = 0; i < (int)size; ++i) {
    if (i)
      res += ", ";
    res += keystr(v[i]);
  }
  res += ")";
  return res;
}

} // namespace

std::string Variable::str() const {
  auto fn = [&](const auto &value) {
    std::string res;
    if constexpr (!Varsize_t<std::decay_t<decltype(value)>>::constant) {
      res += "[";
      for (const auto &entry : value) {
        res += " " + keystr(entry.first) + " -> ";
        res += keystr(entry.second);
      }
      res += " ]";
    } else {
      res += keystr(value);
    }
    return res;
  };
  std::string forward = std::visit(fn, contents_);
  if (reverse_)
    return "forward: " + forward + ", reverse: " + fn(*reverse_);
  else
    return forward;
}

Variable::~Variable() {}

bool Variable::inArray(const Value &v) {
  if (!reverse_) {
    std::visit(
        [&](const auto &map) {
          reverse_.emplace();
          if constexpr (!Varsize_t<std::decay_t<decltype(map)>>::constant) {
            for (const auto &entry : map) {
              if (++(*reverse_)[entry.second] > 1)
                non_unique_++;
            }
          } else {
            throw std::logic_error{"called INARRAY on a singular value"};
          }
        },
        contents_);
  }
  auto it = reverse_->find(v);
  return it != reverse_->end() && it->second;
}

const Value &Variable::get() const { return std::get<Value>(contents_); }
void Variable::set(Value value) { contents_ = std::move(value); }

int64_t Variable::size() const {
  return std::visit([](const auto &v) { return Varsize(v); }, contents_);
}

template <size_t... I>
struct Variable::UniqueVisitor<std::index_sequence<I...>> {
  struct visit {
    int64_t size;
    const Variable *const *vars;
    template <typename T> bool operator()(const T &x) {
      if constexpr (!Varsize_t<T>::constant) {
        std::vector<std::array<Value, sizeof...(I)>> elements(size);
        using It = typename T::const_iterator;
        for (const T *variable_contents :
             {std::get_if<T>(&vars[I]->contents_)...})
          if (!variable_contents)
            return false;
        It iterators[] = {std::get<T>(vars[I]->contents_).begin()...};
        for (int64_t i = 0; i < size; ++i) {
          const auto &index = iterators[0]->first;
          for (int j : {I...}) {
            auto it = iterators[j]++;
            if (it->first != index) {
              return false;
            }
            elements[i][j] = it->second;
          }
        }
        std::sort(begin(elements), end(elements));
        auto it = std::unique(begin(elements), end(elements));
        if (it != end(elements)) {
          return false;
        }
        return true;
      } else {
        throw std::logic_error{"UNIQUE on non array"};
      }
    }
  };
  bool operator()(const Variable *const *vars) const {
    for (int i = 0; i < (int)sizeof...(I); ++i) {
      if (vars[i]->contents_.index() != vars[0]->contents_.index()) {
        throw std::logic_error{"UNIQUE on arrays of different dimensions"};
      }
    }
    // std::cerr << vars[0]->str() << '\n';
    return std::visit(visit{vars[0]->size(), vars}, vars[0]->contents_);
  }
};

template <size_t csize>
bool Variable::UniqueVarsDispatch(const std::vector<const Variable *> &vars) {
  if constexpr (csize <= kMaxDim) {
    if (csize == vars.size()) {
      std::array<const Variable *, csize> arg;
      for (int i = 0; i < (int)csize; ++i)
        arg[i] = vars[i];
      return UniqueVisitor<std::make_index_sequence<csize>>{}(vars.data());
    }
    return UniqueVarsDispatch<csize + 1>(vars);
  }
  throw std::logic_error{"too many arguments to UNIQUE, unimplemented"};
}

bool Variable::UniqueVars(const std::vector<const Variable *> &vars) {
  if (vars.size() == 1 && vars[0]->reverse_)
    return vars[0]->non_unique_ == 0;
  auto expected_size = vars.at(0)->size();
  for (int i = 1; i < (int)vars.size(); ++i) {
    if (vars[i]->size() != expected_size)
      throw std::logic_error{"UNIQUE with mismatching array sizes"};
  }
  return UniqueVarsDispatch<1>(vars);
}
