#include <iostream>

template <unsigned int n> struct factorial {
  static const unsigned int value = n * factorial<n - 1>::value;
};

template <> struct factorial<0> {
  static const unsigned int value = 1;
};

int main()
{
  std::cout << factorial<0>::value << std::endl;
  std::cout << factorial<1>::value << std::endl;
  std::cout << factorial<5>::value << std::endl;
}
