/*
  (c) 1995--2006 Han-Wen Nienhuys

  Distributed under GNU GPL
*/
#ifndef STD_VECTOR_HH
#error array.hh is obsolete, use std-vector.hh
#endif

#ifndef ARRAY_H
#define ARRAY_H

#include <cassert>
using namespace std;

#ifndef INLINE
#define INLINE inline
#endif

#if !STD_VECTOR
#define index_assert(i) (assert (i >= 0 && i < size_));
#else
#define index_assert(i) (assert (i != VPOS && i < size_));
#endif

namespace std {
/// copy a bare (C-)array from #src# to #dest# sized  #count#
template<class T> void arrcpy (T *dest, T const *src, vsize count);

/**
   Scaleable array/stack template, for a type T with default constructor.


   This template implements a scaleable vector. With (or without) range
   checking. The type T should have a default constructor. It is
   best suited for simple types, such as int, double or String, it
   provides a paranoidly safe replacement for the new T[int] construct.

   You should \bf{never} store pointers to objects in an Array (since
   the array may be relocated without the pointer knowing it).

   It uses stack terminology, (push, pop, top), and  can be used as a stack.
*/
template<class T>
class Array
{
public:

  /// maximum length of array.
  vsize max_;

  /// the data itself
  T *array_;

  /// stretch or shrink  array.
  void remax (vsize newmax)
  {
    T *newarr = new T[newmax];
    size_ = (newmax < size_) ? newmax : size_;
    arrcpy (newarr, array_, size_);

    delete[] array_;
    array_ = newarr;
    max_ = newmax;
  }
  vsize size_;

public:
  /* std::vector interface */
  typedef T* iterator;
  typedef T const* const_iterator;

  Array ()
  {
    array_ = 0;
    max_ = 0;
    size_ = 0;
  }

  Array (Array const &src)
  {
    array_ = src.copys ();
    max_ = size_ = src.size_;
  }

  Array (const_iterator begin, const_iterator end);
    
  T const &back () const
  {
    return (*this)[size_ - 1];
  }

  T &back ()
  {
    return (*this)[size_ - 1];
  }

  bool empty () const
  {
    return !size_;
  }

  void pop_back ()
  {
    assert (!empty ());
    resize (size () - 1);
  }

  vsize size () const
  {
    return size_;
  }

  /** set the size_ to #s#.
      POST: size () == s.
      Warning: contents are unspecified */
  void resize (vsize s)
  {
    if (s > max_)
      remax (s);
    size_ = s;
  }

  T*
  data ()
  {
    return array_;
  }

  T const*
  data () const
  {
    return array_;
  }

  iterator
  begin ()
  {
    return data ();
  }

  const_iterator
  begin () const
  {
    return data ();
  }
  
  iterator
  end ()
  {
    return data () + size_;
  }

  const_iterator
  end () const
  {
    return data () + size_;
  }

  void clear ()
  {
    //resize (0);
    size_ = 0;
  }

  T &
  at (vsize i)
  {
    return (*this)[i];
  }

  T const &
  at (vsize i) const
  {
    return (*this)[i];
  }

  T &operator [] (vsize i)
  {
    return array_[i];
  }

  T const &operator [] (vsize i) const
  {
    return array_[i];
  }

  iterator
  erase (iterator p)
  {
    vsize i = p - data ();
    index_assert (i);
    arrcpy (array_ + i, array_ + i + 1, size_ - i - 1);
    size_--;
    return p;
  }

  void
  insert (iterator b, T k)
  {
    vsize j = b - array_;
    resize (size_ + 1);
    index_assert (j);
    for (vsize i = size_ - 1; i > j; i--)
      array_[i] = array_[i - 1];
    array_[j] = k;
  }

  void
  insert (iterator pos, const_iterator b, const_iterator e)
  {
    vsize j = pos - array_;
    vsize k = e - b;
    resize (size_ + k);
    for (vsize i = size_ - 1; i > j + k; i--)
      array_[i] = array_[i - k];
    for (vsize i = j; i < j + k; i++)
      array_[i] = b[i - j];
  }

  /// add to the end of array
  void push_back (T x)
  {
    if (size_ == max_)
      remax (2 * max_ + 1);

    // T::operator= (T &) is called here. Safe to use with automatic
    // vars
    array_[size_++] = x;
  }


  /* Flower intererface */

  
  /// check invariants
  void OK () const;
  /** report the size_.
      @see
      {setsize_}
  */

  Array (T *tp, vsize n)
  {
    array_ = new T[n];
    max_ = size_ = n;
    arrcpy (array_, tp, n);
  }

  // ugh, get around gcc 2.8.1 ice; see bezier.cc
  Array (vsize i)
  {
    max_ = size_ = i;
    array_ = new T[i];
  }

  /// tighten array size_.
  void tighten_maxsize ()
  {
    remax (size_);
  }

  ~Array ()
  {
    delete[] array_;
  }

  /// return a  "new"ed copy of array 
  T *copys () const
  {
    T *Tarray = new T[size_];
    arrcpy (Tarray, array_, size_);
    return Tarray;
  }

  void operator = (Array const &src)
  {
    resize (src.size_);
    arrcpy (array_, src.array_, size_);
  }

  void unordered_del (vsize i)
  {
    at (i) = back ();
    resize (size () -1);
  }
};

  template<typename T>
  T const &
  back (Array<T> const &v, vsize i)
  {
    return v[v.size () - i - 1];
  }

  template<typename T>
  T&
  back (Array<T> &v, vsize i)
  {
    return v[v.size () - i - 1];
  }

  template<typename T>
  T const &
  boundary (Array<T> const &v, int dir, vsize i)
  {
    assert (dir);
    return v[dir == -1 ? i : v.size () - 1 - i];
  }

  template<typename T>
  T &
  boundary (Array<T> &v, int dir, vsize i)
  {
    assert (dir);
    return v[dir == -1 ? i : v.size () - 1 - i];
  }

  template<class T>
  void
  reverse (Array<T> &v)
  {
    vsize h = v.size () / 2;
    for (vsize i = 0, j = v.size () - 1; i < h; i++, j--)
      swap (v[i], v[j]);
  }

  template<typename T>
  void
  concat (Array<T> &v, Array<T> const& w)
  {
    v.insert (v.end (), w.begin (), w.end ());
  }

  template<typename T>
  void
  vector_sort (Array<T> &v, int (*compare) (T const &, T const &),
	       vsize lower=-1, vsize upper=-1)
  {
    if (lower < 0)
      {
	lower = 0;
	upper = v.size () - 1;
      }
    if (lower >= upper)
      return;
    swap (v[lower], v[(lower + upper) / 2]);
    vsize last = lower;
    for (vsize i = lower +1; i <= upper; i++)
      if (compare (v.array_[i], v.array_[lower]) < 0)
	swap (v[++last], v[i]);
    swap (v[lower], v[last]);
    vector_sort (v, compare, lower, last - 1);
    vector_sort (v, compare, last + 1, upper);
  }

#include "array.icc"

}

#endif
