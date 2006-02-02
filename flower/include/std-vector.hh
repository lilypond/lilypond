/*
  std-vector.hh -- declare std::vector

  source file of the GNU LilyPond music typesetter

  (c) 2006 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef STD_VECTOR_HH
#define STD_VECTOR_HH

#include <algorithm> // reverse, sort

#if !STD_VECTOR
/* Also declare vector, in the wrong way.  */
#include <algorithm>
#include <iostream>
#include <sstream>
#endif


#include "compare.hh"

#if STD_VECTOR

#define vector __vector
#include <vector>
#undef vector

namespace std {

  #ifndef VSIZE
  #define VSIZE
  typedef size_t vsize;
  #define VPOS UINT_MAX
  #endif

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

    /* Flower-Array compatibility.  */
    void
    concat (vector<T> const &v)
    {
      __vector<T>::insert (this->end (), v.begin (), v.end ());
    }

    T const &
    boundary (int dir, vsize i) const
    {
      assert (dir);
      if (dir == 1)
	return this->top (i);
      else
	return this->at (i);
    }

    T &
    boundary (int dir, vsize i)
    {
      assert (dir);
      if (dir == 1)
	return this->top (i);
      else
	return this->at (i);
    }

    T *
    remove_array ()
    {
      T *p = &(*this)[0];
      /* FIXME: forget array? */
      /* this->resize (0); */
      return p;
    }

    void
    reverse ()
    {
      // CHECKME: for a simple vector, like vector<int>, this should
      // expand to memrev.
      ::std::reverse (this->begin (), this->end ());
    }

    void swap (vsize i, vsize j)
    {
      T t ((*this)[i]);
      (*this)[i] = (*this)[j];
      (*this)[j] = t;
    }

    T const &
    top (vsize i) const
    {
      return (*this)[this->size () - i - 1];
    }

    T&
    top (vsize i)
    {
      return (*this)[this->size () - i - 1];
    }
  };
  
#if 0
  template<typename T>
  vsize
  //  binary_search (std::vector<T> const &v,
  binary_search (vector<T> const &v,
		 T const &key, int (*compare) (T const &, T const &),
		 vsize b=0, vsize e=VPOS)
  {
    //(void) compare;
    typename vector<T>::const_iterator i = find (v.iter (b), v.iter (e), key);
    if (i != v.end ())
      return i - v.begin ();
    return VPOS;
  }
#else // c&p from array.icc; cannot easily use stl_algo:find b.o. compare func.
  template<class T>
  void
  binary_search_bounds (vector<T> const &table,
			T const &key, int (*compare) (T const &, T const &),
			vsize *lo,
			vsize *hi)
  {
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

  template<class T>
  vsize
  binary_search (vector<T> const &table,
		 T const &key, int (*compare) (T const &, T const &),
		 vsize lo=0,
		 vsize hi=VPOS)
  {
    if (hi == VPOS)
      hi = table.size ();

    binary_search_bounds (table, key, compare, &lo, &hi);

    if (! (*compare) (key, table[lo]))
      return lo;

    /* not found */
    return VPOS;
  }
#endif


#if 0
  /* FIXME: the simple test works, but lily barfs.  */
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
  v.swap (lower, (lower + upper) / 2);
  vsize last = lower;
  for (vsize i = lower +1; i <= upper; i++)
    if (compare (v[i], v[lower]) < 0)
      v.swap (++last, i);
  v.swap (lower, last);
  vector_sort (v, compare, lower, last - 1);
  vector_sort (v, compare, last + 1, upper);
}
  
#endif

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


#endif /* STD_VECTOR */

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
 

#include "array.hh"

#endif /* STD_VECTOR_HH */
