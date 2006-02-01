#if !STD_VECTOR
#define Array flower_vector
#endif
#include "std-vector.hh"

#include <iostream>

#include <boost/test/auto_unit_test.hpp>
#include <boost/test/floating_point_comparison.hpp>

using boost::unit_test::test_suite;

template<typename T>
void
print (vector<T> v)
{
  for (vsize i = 0; i < v.size (); i++)
    cout << "v[" << i << "] = " << v[i] << endl;
}

BOOST_AUTO_UNIT_TEST (vector_erase)
{
  vector<int> v;
  v.push_back (0);
  v.push_back (1);
  BOOST_CHECK_EQUAL (v.size (), 2u);
  v.erase (v.begin () + 1);
  BOOST_CHECK_EQUAL (v.size (), 1u);
  BOOST_CHECK_EQUAL (v.back (), 0);

  v.push_back (1);
  BOOST_CHECK_EQUAL (v.size (), 2u);
  v.erase (v.begin () + 0);
  BOOST_CHECK_EQUAL (v.size (), 1u);
  BOOST_CHECK_EQUAL (v.back (), 1);
}


test_suite*
init_unit_test_suite (int, char**)
{
  test_suite *test = BOOST_TEST_SUITE("std::Flower");
  test->add (BOOST_TEST_CASE (vector_erase));
  return test;
}
