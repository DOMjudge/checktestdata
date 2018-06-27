#pragma once
#include <map>
#include <variant>
#include <vector>

#include "btree_map.h"

#include "value.h"

#include "checktestdataParser.h"

class Variable {
 public:
  static bool UniqueVars(const std::vector<const Variable*>& vars);
  void reset() {
    contents_ = std::monostate{};
    reverse_.reset();
    last_access_ = nullptr;
  }
  int64_t size() const;
  const Value& get() const;
  void set(Value value);
  template <int size, typename IdxIt>
  static std::variant<std::array<int64_t, size>, std::array<Value, size>>
  getIndexArray(IdxIt it, bool array) {
    std::array<int64_t, size> int_idx;
    std::array<Value, size> val_idx;
    int i = 0;
    for (; array && i < size; ++i) {
      Value v = *(it + i);
      int64_t* j = std::get_if<int64_t>(&v.value_);
      if (j) {
        int_idx[i] = *j;
      } else {
        val_idx[i] = std::move(v);
        break;
      }
    }
    if (i == size) return int_idx;
    for (int j = 0; j < i; ++j) val_idx[j] = Value{int_idx[j]};
    for (; i < size; ++i) val_idx[i] = *(it + i);
    return val_idx;
  }
  template <int size, typename IdxIt>
  const Value& get(IdxIt it) const {
    if (const auto* array = std::get_if<array_t<size>>(&contents_)) {
      auto idx = std::get<0>(getIndexArray<size>(it, true));
      return at(*array, idx);
    } else {
      const map_t<size>& map = std::get<map_t<size>>(contents_);
      auto idx = std::get<1>(getIndexArray<size>(it, false));
      return at(map, idx);
    }
  }
  template <int size, typename IdxIt>
  void set(IdxIt it, Value value) {
    if (std::get_if<std::monostate>(&contents_)) {
      contents_ = array_t<size>{};
    }
    std::optional<decltype(getIndexArray<size>(it, true))> idx;
    if (auto* array = std::get_if<array_t<size>>(&contents_)) {
      auto idx = getIndexArray<size>(it, true);
      if (const auto* int_idx = std::get_if<0>(&idx)) {
        setMap(array, *int_idx, std::move(value));
        return;
      }
      map_t<size> replacement;
      for (const auto& entry : *array) {
        std::array<Value, size> idx;
        for (int i = 0; i < size; ++i) {
          idx[i] = Value{entry.first[i]};
        }
        replacement[idx] = std::move(entry.second);
      }
      contents_ = std::move(replacement);
      reverse_.reset();
      last_access_ = nullptr;
    }
    map_t<size>& map = std::get<map_t<size>>(contents_);
    if (!idx) idx = getIndexArray<size>(it, false);
    setMap(&map, std::get<1>(*idx), std::move(value));
  }
  bool inArray(const Value& v);

  static constexpr size_t kMaxDim = 4;
  std::string str() const;

  ~Variable();

 private:
  template <typename M, typename I>
  void setMap(M* map, const I& idx, Value v) {
    typename M::value_type* last_access =
        static_cast<typename M::value_type*>(last_access_);
    if (!last_access || last_access->first != idx) {
      auto inserted =
          map->insert(std::make_pair(idx, Value{Value::invalid_value{}}));
      last_access_ = last_access = &*inserted.first;
    }
    Value& entry = last_access->second;
    if (reverse_ && !entry.invalid()) {
      if (--(*reverse_)[entry] == 1) non_unique_--;
    }
    entry = std::move(v);
    if (reverse_) {
      if ((*reverse_)[entry]++ == 1) non_unique_++;
    }
  }
  template <typename T, typename I>
  auto& at(T& c, const I& i) const {
    const typename T::value_type* last_access =
        static_cast<typename T::value_type*>(last_access_);
    if (!last_access || last_access->first != i) {
      auto it = c.find(i);
      if (it == c.end()) {
        throw std::out_of_range("map access");
      }
      last_access = &*it;
      last_access_ = const_cast<void*>(static_cast<const void*>(last_access));
    }
    return last_access->second;
  }
  template <typename T>
  struct UniqueVisitor;
  template <size_t csize>
  static bool UniqueVarsDispatch(const std::vector<const Variable*>& vars);

  template <typename T, typename V>
  using btree = btree::btree_map<T, V, std::less<T>,
                                 std::allocator<std::pair<const T, V>>, 4096>;
  template <size_t size>
  using map_t = btree<std::array<Value, size>, Value>;
  template <size_t size>
  using array_t = btree<std::array<int64_t, size>, Value>;

  template <typename S>
  struct variant_helper;
  template <size_t... D>
  struct variant_helper<std::index_sequence<D...>> {
    using type =
        std::variant<std::monostate, Value, map_t<D + 1>..., array_t<D + 1>...>;
  };

  variant_helper<std::make_index_sequence<kMaxDim>>::type contents_;
  mutable void* last_access_ = nullptr;
  std::optional<btree<Value, int64_t>> reverse_;
  int64_t non_unique_;
};
