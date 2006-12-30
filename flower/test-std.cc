#define STD_VECTOR 1

#if !STD_VECTOR
#define Array flower_vector
#endif

#define HAVE_BOOST_LAMBDA 1
#include "std-vector.hh"

#include <iostream>

#define YAFFUT_MAIN
#include "yaffut.h"

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

FUNC (vector_erase)
{
  vector<int> v;
  v.push_back (0);
  v.push_back (1);
  EQUAL (v.size (), vsize (2));
  v.erase (v.begin () + 1);
  EQUAL (v.size (), vsize (1));
  EQUAL (v.back (), 0);

  v.push_back (1);
  EQUAL (v.size (), vsize (2));
  v.erase (v.begin () + 0);
  EQUAL (v.size (), vsize (1));
  EQUAL (v.back (), 1);
}

FUNC (vector_slice)
{
  vector<int> v;
  v.push_back (0);
  v.push_back (1);
  v.push_back (2);
  v.push_back (3);
#if VECTOR_SLICE
  EQUAL (v.slice (0, 0).size (), vsize (0));
  EQUAL (v.slice (0, v.size ()).size (), v.size ());
  EQUAL (v.slice (1, 2).size (), vsize (1));
#else
  EQUAL (vector<int> (v.begin (), v.begin ()).size (), vsize (0));
  EQUAL (vector<int> (v.begin (), v.end ()).size (), v.size ());
  EQUAL (vector<int> (v.begin () + 1, v.begin () + 2).size (),
		     vsize (1));
#endif
}

FUNC (vector_sorting)
{
  vector<int> v;
  v.push_back (2);
  v.push_back (1);
  v.push_back (0);
#if VECTOR_SORT
  v.sort (default_compare);
#else
  //sort (v.begin (), v.end ());
  vector_sort (v, less<int> ());
#endif
  EQUAL (v[0], 0);
  EQUAL (v[1], 1);
  EQUAL (v[2], 2);
}

FUNC (vector_insert)
{
  vector<int> v;
  v.push_back (0);
#if VECTOR_INSERT
  v.insert (1, 0);
#else
  v.insert (v.begin (), 1);
#endif  
  EQUAL (v[0], 1);
#if VECTOR_INSERT
  v.insert (2, v.size ());
#else
  v.insert (v.end (), 2);
#endif  
  EQUAL (v.back (), 2);
  vector<int> u;
  u.insert (u.begin (), v.begin (), v.end ());
  EQUAL (u.size (), v.size ());
  u.clear ();
  u.insert (u.end (), v.begin (), v.end ());
  EQUAL (u.size (), v.size ());
  u.clear ();
}

FUNC (parray_concat)
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
  EQUAL (u[0], &a[0]);
  EQUAL (u[1], &a[1]);
  EQUAL (u[2], &a[2]);
  EQUAL (u[3], &a[3]);
  EQUAL (u[4], &a[4]);
  EQUAL (u.size (), vsize (5));
  concat (u, v);
  EQUAL (u.size (), vsize (7));

  u.clear ();
  v.clear ();
  v.push_back (&a[0]);
  v.push_back (&a[1]);
  v.push_back (&a[2]);
  v.push_back (&a[3]);
  v.push_back (&a[4]);
  concat (u, v);
  EQUAL (u[0], &a[0]);
  EQUAL (u[1], &a[1]);
  EQUAL (u[2], &a[2]);
  EQUAL (u[3], &a[3]);
  EQUAL (u[4], &a[4]);
  EQUAL (u.size (), vsize (5));
}

FUNC (parray_uniq)
{
  vector<int> v;
  v.push_back (0);
  v.push_back (1);
  v.push_back (0);
  vector_sort (v, less<int> ());
  uniq (v);
  EQUAL (v.size (), vsize (2));
}

FUNC (vector_search)
{
  vector<int> v;
  v.push_back (0);
  v.push_back (1);
  v.push_back (2);
  vsize i = binary_search (v, 1, less<int> ());
  EQUAL (i, vsize (1));
}
