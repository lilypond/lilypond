#if !STD_VECTOR
#define Array flower_vector
#endif
#include "std-vector.hh"

#include <iostream>

#include <boost/test/auto_unit_test.hpp>
#include <boost/test/floating_point_comparison.hpp>

using boost::unit_test::test_suite;

#if !STD_VECTOR
#define vector flower_vector
#endif

template<typename T>
void
print (vector<T> v)
{
  for (vsize i = 0; i < v.size (); i++)
    cout << "v[" << i << "] = " << v[i] << endl;
  cout << endl;
}

BOOST_AUTO_UNIT_TEST (vector_erase)
{
  vector<int> v;
  v.push_back (0);
  v.push_back (1);
  BOOST_CHECK_EQUAL (v.size (), vsize (2));
  v.erase (v.begin () + 1);
  BOOST_CHECK_EQUAL (v.size (), vsize (1));
  BOOST_CHECK_EQUAL (v.back (), 0);

  v.push_back (1);
  BOOST_CHECK_EQUAL (v.size (), vsize (2));
  v.erase (v.begin () + 0);
  BOOST_CHECK_EQUAL (v.size (), vsize (1));
  BOOST_CHECK_EQUAL (v.back (), 1);
}

BOOST_AUTO_UNIT_TEST (vector_slice)
{
  vector<int> v;
  v.push_back (0);
  v.push_back (1);
  v.push_back (2);
  v.push_back (3);
#if VECTOR_SLICE
  BOOST_CHECK_EQUAL (v.slice (0, 0).size (), vsize (0));
  BOOST_CHECK_EQUAL (v.slice (0, v.size ()).size (), v.size ());
  BOOST_CHECK_EQUAL (v.slice (1, 2).size (), vsize (1));
#else
  BOOST_CHECK_EQUAL (vector<int> (v.begin (), v.begin ()).size (), vsize (0));
  BOOST_CHECK_EQUAL (vector<int> (v.begin (), v.end ()).size (), v.size ());
  BOOST_CHECK_EQUAL (vector<int> (v.begin () + 1, v.begin () + 2).size (),
		     vsize (1));
#endif
}

BOOST_AUTO_UNIT_TEST (vector_sorting)
{
  vector<int> v;
  v.push_back (2);
  v.push_back (1);
  v.push_back (0);
#if VECTOR_SORT
  v.sort (default_compare);
#else
  vector_sort (v, default_compare);
#endif
  print (v);
  BOOST_CHECK_EQUAL (v[0], 0);
  BOOST_CHECK_EQUAL (v[1], 1);
  BOOST_CHECK_EQUAL (v[2], 2);
}

test_suite*
init_unit_test_suite (int, char**)
{
  test_suite *test = BOOST_TEST_SUITE("std::Flower");
  test->add (BOOST_TEST_CASE (vector_erase));
  test->add (BOOST_TEST_CASE (vector_slice));
  test->add (BOOST_TEST_CASE (vector_sorting));
  return test;
}
