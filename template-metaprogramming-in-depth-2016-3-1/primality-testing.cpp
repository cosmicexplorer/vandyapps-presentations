#include <iostream>

/* can use std::true_type and std::false_type from <type_traits> */
struct false_type {
  typedef false_type type;
  static const int value = 0;
};

struct true_type {
  typedef true_type type;
  static const int value = 1;
};

/* can use std::conditional */
/* defaults to "returning" second class */
template <bool condition, class T, class U> struct if_ {
  typedef U type;
};

/* but if first argument is true, specializes to "returning" first class */
template <class T, class U> struct if_<true, T, U> {
  typedef T type;
};

/* use recursion since looping makes no sense */
template <size_t N, size_t c> struct is_prime_impl {
  /* declare typename "type" to be the result of complex recursive expression */
  typedef typename if_<(c * c > N),
                       true_type,
                       typename if_<(N % c == 0),
                                    false_type,
                                    is_prime_impl<N, c + 1>>::type>::type type;
  static const int value = type::value;
};

template <size_t N> struct is_prime {
  static const int value = is_prime_impl<N, 2>::type::value;
};

template <> struct is_prime<0> {
  static const int value = 0;
};

template <> struct is_prime<1> {
  static const int value = 0;
};

/* can check with static_assert, since this is determined at compile time */
static_assert(is_prime<3>::value == 1, "duh");
static_assert(is_prime<4>::value == 0, "duh");

int main()
{
  std::cout << is_prime<3>::value << std::endl;
  std::cout << is_prime<4>::value << std::endl;
}
