#include <boost/type_traits.hpp>
#include <iostream>
#include <cstring>

template <typename I1, typename I2, bool b>
inline I2 copy_imp(I1 first,
                   I1 last,
                   I2 out,
                   const boost::integral_constant<bool, b> &)
{
  std::cout << "slow was chosen!" << std::endl;
  while (first != last) {
    *out = *first;
    ++out;
    ++first;
  }
  return out;
}

template <typename T>
inline T *
    copy_imp(const T * first, const T * last, T * out, const boost::true_type &)
{
  std::cout << "fast was chosen" << std::endl;
  memcpy(out, first, (last - first) * sizeof(T));
  return out + (last - first);
}

template <typename I1, typename I2>
inline I2 our_copy(I1 first, I1 last, I2 out)
{
  return copy_imp(first, last, out,
                  boost::has_trivial_assign<
                      typename std::iterator_traits<I1>::value_type>());
}

#include <vector>

int main()
{
  std::vector<int> a{1, 1, 1, 1, 45234, 1};
  std::vector<int> b(a.size());
  our_copy(a.begin(), a.end(), b.begin());
  for (int el : b) {
    std::cout << el << std::endl;
  }
  int a_arr[] = {1, 2, 3};
  int b_arr[3];
  const int * a_ptr     = a_arr;
  const int * a_end_ptr = a_arr + 3;
  int * b_ptr = b_arr;
  our_copy(a_ptr, a_end_ptr, b_ptr);
  for (int el : b_arr) {
    std::cout << el << std::endl;
  }
}
