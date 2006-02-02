/*
  (c) 1995--2006 Han-Wen Nienhuys

  Distributed under GNU GPL
*/
#ifndef ARRAY_H
#define ARRAY_H

#ifndef STD_VECTOR_HH
#error array.hh is obsolete, use std-vector.hh
#endif

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
template<class T> void arrcpy (T *dest, T const *src, int count);

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

  T const *accesses () const
  {
    return array_;
  }
  void operator = (Array const &src)
  {
    resize (src.size_);
    arrcpy (array_, src.array_, size_);
  }

  T *remove_array ();

  /// access last entry
  T &top (vsize j)
  {
    return (*this)[size_ - j - 1];
  }
  /// return last entry
  T top (vsize j) const
  {
    return (*this)[size_ - j - 1];
  }

  T &boundary (int dir, vsize idx)
  {
    assert (dir);
    if (dir == 1)
      return top (idx);
    else
      return at (idx);
  }
  T boundary (int dir, vsize idx) const
  {
    assert (dir);
    if (dir == 1)
      return top (idx);
    else
      return at (idx);
  }
  void swap (vsize i, vsize j)
  {
    T t ((*this)[i]);
    (*this)[i] = (*this)[j];
    (*this)[j] = t;
  }

  void insert (iterator j, T k);

  void unordered_del (vsize i)
  {
    at (i) = back ();
    resize (size () -1);
  }
  void concat (Array<T> const &src)
  {
    vsize s = size_;
    resize (size_ + src.size_);
    arrcpy (array_ + s, src.array_, src.size_);
  }
  void reverse ();
};

#include "array.icc"

}

#endif
