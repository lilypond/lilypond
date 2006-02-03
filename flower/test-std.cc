

#if !STD_VECTOR
#define Array flower_vector
#endif
#define HAVE_BOOST_LAMBDA 1
#include "std-vector.hh"

#include <iostream>

#include <boost/test/auto_unit_test.hpp>
#include <boost/test/floating_point_comparison.hpp>

using boost::unit_test::test_suite;

#if !STD_VECTOR
#define vector flower_vector
#endif

using namespace std;

template<typename T>
void
print (vector<T> v)
{
  for (vsize i = 0; i < v.size (); i++)
    cout << "v[" << i << "] = " << v[i] << endl;
  cout << endl;
}

#if !STD_VECTOR
template<typename T>
void
print (Link_array<T> v)
{
  for (vsize i = 0; i < v.size (); i++)
    cout << "v[" << i << "] = " << *v[i] << endl;
  cout << endl;
}
#endif

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
  BOOST_CHECK_EQUAL (v[0], 0);
  BOOST_CHECK_EQUAL (v[1], 1);
  BOOST_CHECK_EQUAL (v[2], 2);
}

BOOST_AUTO_UNIT_TEST (vector_insert)
{
  vector<int> v;
  v.push_back (0);
#if VECTOR_INSERT
  v.insert (1, 0);
#else
  v.insert (v.begin (), 1);
#endif  
  BOOST_CHECK_EQUAL (v[0], 1);
#if VECTOR_INSERT
  v.insert (2, v.size ());
#else
  v.insert (v.end (), 2);
#endif  
  BOOST_CHECK_EQUAL (v.back (), 2);
  vector<int> u;
  u.insert (u.begin (), v.begin (), v.end ());
  BOOST_CHECK_EQUAL (u.size (), v.size ());
  u.clear ();
  u.insert (u.end (), v.begin (), v.end ());
  BOOST_CHECK_EQUAL (u.size (), v.size ());
  u.clear ();
}

BOOST_AUTO_UNIT_TEST (parray_concat)
{
#if !STD_VECTOR
  Link_array<int> u, v;
#else
  vector<int*> u, v;
#endif  
  int a[5] = { 0, 1, 2, 3, 4 };
  u.push_back (&a[0]);
  u.push_back (&a[1]);
  u.push_back (&a[2]);
  v.push_back (&a[3]);
  v.push_back (&a[4]);
  concat (u, v);
  BOOST_CHECK_EQUAL (u[0], &a[0]);
  BOOST_CHECK_EQUAL (u[1], &a[1]);
  BOOST_CHECK_EQUAL (u[2], &a[2]);
  BOOST_CHECK_EQUAL (u[3], &a[3]);
  BOOST_CHECK_EQUAL (u[4], &a[4]);
  BOOST_CHECK_EQUAL (u.size (), vsize (5));
  concat (u, v);
  BOOST_CHECK_EQUAL (u.size (), vsize (7));

  u.clear ();
  v.clear ();
  v.push_back (&a[0]);
  v.push_back (&a[1]);
  v.push_back (&a[2]);
  v.push_back (&a[3]);
  v.push_back (&a[4]);
  concat (u, v);
  BOOST_CHECK_EQUAL (u[0], &a[0]);
  BOOST_CHECK_EQUAL (u[1], &a[1]);
  BOOST_CHECK_EQUAL (u[2], &a[2]);
  BOOST_CHECK_EQUAL (u[3], &a[3]);
  BOOST_CHECK_EQUAL (u[4], &a[4]);
  BOOST_CHECK_EQUAL (u.size (), vsize (5));
}

BOOST_AUTO_UNIT_TEST (parray_uniq)
{
  vector<int> v;
  v.push_back (0);
  v.push_back (1);
  v.push_back (0);
  vector_sort (v, default_compare);
  uniq (v);
  BOOST_CHECK_EQUAL (v.size (), vsize (2));
}

BOOST_AUTO_UNIT_TEST (vector_search)
{
  vector<int> v;
  v.push_back (0);
  v.push_back (1);
  v.push_back (2);
  vsize i = binary_search (v, 1, &default_compare);
  BOOST_CHECK_EQUAL (i, vsize (1));
}

test_suite*
init_unit_test_suite (int, char**)
{
  vsize i = 0;
  vsize j = 0;
  vector<int> v;
  binary_search_bounds (v, 1, &default_compare, &i, &j);
  //binary_search_bounds (v, 1, &default_compare, 0, 0);
  
  //Link_array<char> w;
  vector<char*> w;
  binary_search_bounds (w, (char*)1, &default_compare, &i, &j);
  
  test_suite *test = BOOST_TEST_SUITE("std::Flower");
  test->add (BOOST_TEST_CASE (vector_erase));
  test->add (BOOST_TEST_CASE (vector_slice));
  test->add (BOOST_TEST_CASE (vector_sorting));
  test->add (BOOST_TEST_CASE (vector_insert));
  test->add (BOOST_TEST_CASE (parray_concat));
  test->add (BOOST_TEST_CASE (parray_uniq));
  test->add (BOOST_TEST_CASE (vector_search));
  return test;
}
