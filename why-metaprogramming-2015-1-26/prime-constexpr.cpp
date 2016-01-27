#include <iostream>
#include <limits>

constexpr size_t mid(size_t low, size_t high) { return (low + high) / 2; }

// precondition: low*low <= n, high*high > n.
constexpr size_t ceilsqrt(size_t n, size_t low, size_t high) {
  return low + 1 >= high ? high : (mid(low, high) * mid(low, high) == n)
                                      ? mid(low, high)
                                      : (mid(low, high) * mid(low, high) < n)
                                            ? ceilsqrt(n, mid(low, high), high)
                                            : ceilsqrt(n, low, mid(low, high));
}

// returns ceiling(sqrt(n))
constexpr size_t ceilsqrt(size_t n) {
  return n < 3 ? n : ceilsqrt(n, 1,
                              size_t(1)
                                  << (std::numeric_limits<size_t>::digits / 2));
}

// returns true if n is divisible by an odd integer in
// [2 * low + 1, 2 * high + 1).
constexpr bool find_factor(size_t n, size_t low, size_t high) {
  return low + 1 >= high ? (n % (2 * low + 1)) == 0
                         : (find_factor(n, low, mid(low, high)) ||
                            find_factor(n, mid(low, high), high));
}

constexpr bool is_prime(std::size_t n) {
  return n > 1 &&
         (n == 2 || (n % 2 == 1 &&
                     (n == 3 || !find_factor(n, 1, (ceilsqrt(n) + 1) / 2))));
}

int main() {
  if (is_prime(4)) {
    std::cout << "hey" << std::endl;
  }
}
