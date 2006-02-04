/*
  std-vector.hh -- declare std::vector

  source file of the GNU LilyPond music typesetter

  (c) 2006 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef STD_VECTOR_HH
#define STD_VECTOR_HH

#include <algorithm>   /* find, reverse, sort */
#include <functional>  /* unary_function */
#include <cassert>

#if HAVE_BOOST_LAMBDA
#include <boost/lambda/lambda.hpp>
#endif

template<typename T>
int default_compare (T const &a, T const &b)
{
   if (a < b)
     return -1;
   else if (a > b)
     return 1;
   else
     return 0;
}

template<typename T>
int default_compare (T *const &a, T *const &b)
{
  if (a < b)
    return -1;
  else if (a > b)
    return 1;
  else
    return 0;
}

#if !STD_VECTOR
/* Also declare vector, in the wrong way.  */
#include <iostream>
#include <sstream>
#endif

#include "compare.hh"

#if STD_VECTOR

#include "config.hh"

#if HAVE_STL_DATA_METHOD
#include <vector>
#else /* !HAVE_STL_DATA_METHOD */
#define vector __vector
#include <vector>
#undef vector
namespace std {
  /* Interface without pointer arithmetic (iterator) semantics.  */
  template<typename T>
  class vector : public __vector<T>
  {
  public:
    typedef typename __vector<T>::iterator iterator;
    typedef typename __vector<T>::const_iterator const_iterator;
    
    vector<T> () : __vector<T> ()
    {
    }
    
    vector<T> (const_iterator b, const_iterator e) : __vector<T> (b, e)
    {
    }
    
    T*
    data ()
    {
      return &(*this)[0];
    }
    
    T const*
    data () const
    {
      return &(*this)[0];
    }
  };
}
#endif /* !HAVE_STL_DATA_METHOD */

namespace std {

  #ifndef VSIZE
  #define VSIZE
  typedef size_t vsize;
  #define VPOS UINT_MAX
  #endif

  template<typename T>
  T const &
  boundary (vector<T> const &v, int dir, vsize i)
  {
    assert (dir);
    return v[dir == -1 ? i : v.size () - 1 - i];
  }

  template<typename T>
  T &
  boundary (vector<T> &v, int dir, vsize i)
  {
    assert (dir);
    return v[dir == -1 ? i : v.size () - 1 - i];
  }

  template<typename T>
  T const &
  back (vector<T> const &v, vsize i)
  {
    return v[v.size () - i - 1];
  }

  template<typename T>
  T&
  back (vector<T> &v, vsize i)
  {
    return v[v.size () - i - 1];
  }

  template<typename T>
  void
  concat (vector<T> &v, vector<T> const& w)
  {
    v.insert (v.end (), w.begin (), w.end ());
  }
  
  template<class T>
  void
  binary_search_bounds (vector<T> const &table,
			T const &key, int (*compare) (T const &, T const &),
			vsize *lo,
			vsize *hi)
  {
    if (*lo >= *hi)
      return;

    int cmp;
    int result;

    /* binary search */
    do
      {
	cmp = (*lo + *hi) / 2;

	result = (*compare) (key, table[cmp]);

	if (result < 0)
	  *hi = cmp;
	else
	  *lo = cmp;
      }
    while (*hi - *lo > 1);
  }

#if 0
  template<typename T>
  vsize
  //  binary_search (std::vector<T> const &v,
  binary_search (vector<T> const &v,
		 T const &key, int (*compare) (T const &, T const &),
		 vsize b=0, vsize e=VPOS)
  {
    //(void) compare;
    if (e == VPOS)
      e = v.size ();
    typename vector<T>::const_iterator i = find (v.begin () + b,
						 v.begin () + e, key);
    if (i != v.end ())
      return i - v.begin ();
    return VPOS;
  }
#else // c&p from array.icc; cannot easily use stl_algo:find b.o. compare func.
  template<class T>
  vsize
  binary_search (vector<T> const &table,
		 T const &key,
		 int (*compare) (T const &, T const &),
		 vsize lo=0,
		 vsize hi=VPOS)
  {
    if (hi == VPOS)
      hi = table.size ();

    if (lo >= hi)
      return VPOS;

    binary_search_bounds (table, key, compare, &lo, &hi);

    if (! (*compare) (key, table[lo]))
      return lo;

    /* not found */
    return VPOS;
  }


#endif


#if 0
  /* FIXME: the COMPARE functionality is broken?  */
  template<typename T>
  void
  vector_sort (vector<T> &v, int (*compare) (T const &, T const &),
	       vsize lower=VPOS, vsize upper=VPOS)
  {
    typename vector<T>::iterator b = v.begin ();
    typename vector<T>::iterator e = v.begin ();
    if (lower == VPOS)
      {
	lower = 0;
	upper = v.size ();
      }
    ::std::sort (b + lower, e + upper, compare);
  }
#else

  // ugh, c&p
  template<typename T> void
  vector_sort (vector<T> &v, int (*compare) (T const &, T const &),
	       vsize lower=VPOS, vsize upper=VPOS)
  {
    if (lower == VPOS)
      {
	lower = 0;
	upper = v.size () - 1;
    }
    if (upper == VPOS || lower >= upper)
      return;
    swap (v[lower], v[(lower + upper) / 2]);
    vsize last = lower;
    for (vsize i = lower +1; i <= upper; i++)
      if (compare (v[i], v[lower]) < 0)
	swap (v[++last], v[i]);
    swap (v[lower], v[last]);
    vector_sort (v, compare, lower, last - 1);
    vector_sort (v, compare, last + 1, upper);
  }
#endif
  
  template<typename T>
  void
  reverse (vector<T> &v)
  {
    // CHECKME: for a simple vector, like vector<int>, this should
    // expand to memrev.
    ::std::reverse (v.begin (), v.end ());
  }

  template<typename T>
  void
  uniq (vector<T> &v)
  {
    v.erase (unique (v.begin (), v.end ()), v.end ());
  }

  template<typename T>
  typename vector<T>::const_iterator
  find (vector<T> const &v, T const &key)
  {
    return ::std::find (v.begin (), v.end (), key);
  }

#if HAVE_BOOST_LAMBDA
#include <boost/lambda/lambda.hpp>
  using namespace boost::lambda;
  template<typename T>
  void
  junk_pointers (vector<T> &v)
  {
    for_each (v.begin (), v.end (), (delete _1, _1 = 0));
    v.clear ();
  }
#else

  template<typename T> struct del : public unary_function<T, void>
  {
    void operator() (T x)
    {
      delete x;
      x = 0;
    }
  };

  template<typename T>
  void
  junk_pointers (vector<T> &v)
  {
    // Hmm.
    ::std::for_each (v.begin (), v.end (), del<T> ());
    v.clear ();
  }
#endif /* HAVE_BOOST_LAMBDA */

}

#else /* ! STD_VECTOR */

namespace std {

#ifndef Array  
#define vector Array
#endif

  using namespace std;
  
#ifndef VSIZE
#define VSIZE
  typedef int vsize;
#define VPOS -1
#endif

}

#include "array.hh"
#include "parray.hh"

#endif /* !STD_VECTOR */

using namespace std;

#endif /* STD_VECTOR_HH */
