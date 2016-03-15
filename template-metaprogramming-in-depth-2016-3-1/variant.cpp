#include "boost/variant.hpp"
#include <algorithm>
#include <string>
#include <stack>
#include <iostream>

template <typename... Args> struct overload_set : public Args... {
  overload_set(Args... args) : Args(args)...
  {
  }
};

template <typename... Args>
inline overload_set<Args...> overload(Args &&... args)
{
  return overload_set<Args...>(args...);
}

template <typename T, typename... Args>
struct variant_overload_set : public boost::variant<T>, Args... {
  const overload_set<Args...> over_set;
  variant_overload_set(Args... args) : Args(args)..., over_set(args...)
  {
  }
  template <typename U> inline T operator()(U val)
  {
    return static_cast<T>(over_set(val));
  }
};

template <typename T, typename... Args>
inline variant_overload_set<T, Args...> variant_overload(Args &&... args)
{
  return variant_overload_set<T, Args...>(args...);
}

int main()
{
  auto overloaded_funs = overload(
      []() { return 0; }, [](int i) { return i + 1; },
      [](const char * s) { return strlen(s); },
      /* can't be stack<T>! must be concrete type */
      [](std::stack<int> s) { return s.size(); }, [](auto) { return 34; });
  std::stack<int> s;
  s.push(3);
  std::vector<int> results;
  results.push_back(overloaded_funs());
  results.push_back(overloaded_funs(3));
  results.push_back(overloaded_funs("wow"));
  results.push_back(overloaded_funs(s));

  std::for_each(results.begin(), results.end(),
                [](auto i) { std::cout << i << std::endl; });

  auto overload_v =
      variant_overload<int>([]() { return 0; }, [](int i) { return i + 1; },
                            [](const char * s) { return strlen(s); },
                            [](auto a) { return a.size(); });

  boost::variant<int, std::string> v("hello");
  int res = boost::apply_visitor(overload_v, v);
  std::cout << res << std::endl;
}
