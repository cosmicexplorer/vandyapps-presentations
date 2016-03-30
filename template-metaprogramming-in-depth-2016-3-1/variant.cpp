#include "boost/variant.hpp"
#include <algorithm>
#include <string>
#include <stack>
#include <iostream>

template <typename... Args>
struct overload_set : public Args... {
  overload_set(Args... args) : Args(args)...
  {
  }
};

template <typename... Args>
inline overload_set<Args...> overload(Args... args)
{
  return overload_set<Args...>(args...);
}

template <typename T, typename... Args>
struct variant_overload_set : public boost::static_visitor<T>, Args... {
  const overload_set<Args...> over_set;
  variant_overload_set(Args... args) : Args(args)..., over_set(args...)
  {
  }
  template <typename... VisitorArgs>
  inline T operator()(VisitorArgs... args)
  {
    return static_cast<T>(over_set(args...));
  }
};

template <typename T, typename... Args>
inline variant_overload_set<T, Args...> variant_overload(Args... args)
{
  return variant_overload_set<T, Args...>(args...);
}

struct normal_visitor : public boost::variant<int> {
  int operator()() const
  {
    return 0;
  }

  int operator()(int i) const
  {
    return i + 1;
  }

  int operator()(const char * s) const
  {
    return strlen(s);
  }

  /* we can do this here, but not with lambdas */
  template <typename T>
  int operator()(std::stack<T> s) const
  {
    return s.size();
  }

  template <typename T>
  int operator()(T) const
  {
    return 34;
  }
};

int main()
{
  auto overloaded_funs =
      overload([]() { return 0; }, [](int i) { return i + 1; },
               [](const char * s) { return strlen(s); },
               /* can't be stack<T>! must be concrete type */
               [](std::stack<int> s) { return s.size(); },
               /* can't have multiple "auto" matchers, the standard says no */
               [](auto) { return 34; });
  /* normal_visitor overloaded_funs; */
  std::stack<int> s;
  s.push(3);
  std::vector<int> results;
  results.push_back(overloaded_funs());
  results.push_back(overloaded_funs(3));
  results.push_back(overloaded_funs("wow"));
  results.push_back(overloaded_funs(s));

  /* std::for_each(results.begin(), results.end(), */
  /*               [](auto i) { std::cout << i << std::endl; }); */

  auto overload_v = variant_overload<int>([](int i) { return i + 1; },
                                          [](auto el) { return el.size(); });
  auto overload_v2 =
      variant_overload<int>([](int i, std::string s) { return i + s.size(); },
                            [](auto, auto) { return 3; });

  boost::variant<int, std::string> v(std::string("hello world!"));
  /* boost::variant<int, std::string> v(int(4)); */
  boost::variant<int, std::string> other(int(2));
  int res  = boost::apply_visitor(overload_v, v);
  int res2 = boost::apply_visitor(overload_v2, other, v);
  /* int res = boost::apply_visitor(normal_visitor(), v); */
  std::cout << res << std::endl;
  /* std::cout << res2 << std::endl; */
}
