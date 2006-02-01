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

    iterator
    iter (vsize n)
    {
      if (n == VPOS)
	return this->end ();
      return __vector<T>::begin () + n;
    }

    const_iterator
    iter (vsize n) const
    {
      if (n == VPOS)
	return this->end ();
      return __vector<T>::begin () + n;
    }
    
    void
    insert (T k, vsize i)
    {
      __vector<T>::insert (this->iter (i), k);
    }

    void
    insert (vector<T> &v, vsize i)
    {
      __vector<T>::insert (iter (i), v.begin (), v.end ());
    }

    void
    concat (vector<T> const &v)
    {
      __vector<T>::insert (this->end (), v.begin (), v.end ());
    }

    /* Flower-Array compatibility.  */
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

    T const &
    elem (vsize i) const
    {
      return this->at (i);
    }

    T &
    elem (vsize i)
    {
      return this->at (i);
    }

#if 1 // FIXME, silly, but keep for s/r
    T const &
    elem_ref (vsize i) const
    {
      return elem (i);
    }

    T &
    elem_ref (vsize i)
    {
      return elem (i);
    }
#endif

#if 0
    T *
    remove_array ()
    {
      // FIXME, no algorithm for this?
      T *p = new T[this->size ()];
      for (vsize i = 0; i < this->size (); i++)
	p[i] = (*this)[i];
      this->clear ();
      return p;
    }
#else
    T *
    remove_array ()
    {
      T *p = &(*this)[0];
      // forget array?
      //this->resize (0);
      return p;
    }
#endif

    void
    reverse ()
    {
      // CHECKME: for a simple vector, like vector<int>, this should
      // expand to memrev.
      ::std::reverse (this->begin (), this->end ());
    }

    void
    sort (int (*compare) (T const &, T const &), vsize b=0, int e=VPOS)
    {
      ::std::sort (iter (b), iter(e), compare);
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

#include "array.hh"

#endif /* STD_VECTOR_HH */
