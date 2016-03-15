#include <iostream>

/* use recursion since mutating variables not possible within constexpr */
constexpr bool is_prime_helper(size_t n, size_t cur_fac)
{
  if (cur_fac * cur_fac > n) {
    return true;
  }
  if (n % cur_fac == 0) {
    return false;
  }
  return is_prime_helper(n, cur_fac + 1);
}

constexpr bool is_prime(size_t n)
{
  return is_prime_helper(n, 2);
}

int main()
{
  std::cout << is_prime(3) << std::endl;
  std::cout << is_prime(4) << std::endl;
}
