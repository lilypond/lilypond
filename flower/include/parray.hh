/*
  parray.hh -- declare Pointer_array

  source file of the Flower Library

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef PARRAY_HH
#define PARRAY_HH

#include "array.hh"

/**
  an array of pointers.

  TODO
  should init to 0.
 */
template<class T>
class Link_array : private Array<void *>
{
  static int default_compare (T *const& p1, T  *const&p2) {
    /* can't do p1 -p2, since T might be an incomplete type */
    if (p1 < p2)
      return -1 ;
    if (p2 < p1)
      return 1;
    return 0;
  }
  Link_array (Array<void*> v)
    :Array<void*> (v)
    {
    }
public:
  Link_array()
    {}

  Link_array(T * const *tp, int n)
    : Array<void*> ((void **)tp, n)
    {
    }

  Link_array (Link_array<T> const &src)
    : Array<void*> (src)
    {
    }
  /// access element
  T *elem (int i) const 
    {
      return elem_ref (i);
    }
  T *&elem_ref  (int i) const
    {
      return (T*&) Array<void*>::elem_ref (i);
    }

  /// access element
  T* &operator[] (int i)  
    {
      return (T*&) Array<void*>::elem_ref (i);
    }
  /// access element
  T *const operator[] (int i) const 
    {
      return (T *const) Array<void*>::elem (i);
    }
  T *pop ()
    {
      return (T*) Array<void*>::pop ();
    }
  void insert (T *t, int i)
    {
      Array<void*>::insert (t, i);
    }
  void push (T* t)
    {
      Array<void*>::push (t);
    }
  /// return last entry
  T* top (int j=0) const 
    {
      return (T*) Array<void*>::top (j);
    }
  T *& top (int i=0)
    {
      return (T*&) Array<void*>::top (i);
    }
  void substitute (T *old, T*new_l)
    {
      int i;
      while ((i = find_i (old)) >=0) 
	if (new_l)
	  elem_ref (i) =new_l;
	else
	  del (i);
    }
  void unordered_substitute (T* old, T * new_l)
    {
      int i;
      while ((i = find_i (old)) >=0) 
	if (new_l)
	  elem_ref (i) =new_l;
	else {
	  unordered_del (i);
	}
    
    }
  void default_sort() {
    sort (default_compare);
  }
  // quicksort.
  void sort (int (*compare)(T *const&,T *const&),
	     int lower = -1, int upper = -1);
  
  void uniq() {
    Link_array<T> l_arr;
    for (int i=0; i < size(); i++) 
      if (!i || elem (i-1) != elem (i))
	l_arr.push (elem (i)); 
    *this = l_arr;
  }
  Array<void*>::del;
  Array<void*>::unordered_del;  
  Array<void*>::size;
  Array<void*>::clear;
  Array<void*>::set_size;
  Array<void*>::empty;
  Array<void*>::reverse;
  Array<void*>::tighten_maxsize;
  T ** access_array () const
    {
      return (T**) Array<void*>::access_array();
    }
  T * get (int i)
    {
      return (T*) Array<void*>::get (i);
    }
  Link_array<T>
  slice(int l,int u)
    {
      return Array<void*>::slice (l,u);
    }
  void concat (Link_array<T> const &a2)
    {
      Array<void*>::concat (a2);
    }
  int find_i (T const * t) const {
    for (int i=0; i < size(); i++)
      if (elem (i) == t)
	return i;
    return -1;
  }
  T *find_l (T const *t) const
    {
      int i = find_i (t);
      if (i >= 0)
	return elem (i);
      else
	return 0;
    }
  
};

template<class T, class V>
Link_array<T>
typecast_array (Link_array<V> const &a, T * /* dummy */ )
{
  Link_array<T> ret;
  for (int i=a.size (); i-- ; )
	ret.push (dynamic_cast<T*> (a[i]));	// ugh?
  return ret;
}



template<class T> inline void
Link_array<T>::sort (int (*compare)(T *const&,T *const&),
		int lower = -1, int upper = -1) 
{
  if (lower < 0) 
    {
      lower = 0 ;
      upper = size () - 1;
    }
  if (lower >= upper)
    return;
  swap (lower, (lower+upper)/2);
  int last = lower;
  for (int i= lower +1; i <= upper; i++)
    if (compare (elem (i), elem(lower)) < 0)
      swap (++last,i);
  swap (lower, last);
  sort (compare, lower, last-1);
  sort (compare, last+1, upper);
}

template<class T>
void
junk_pointer_array (Link_array<T> &a)
{
  for (int i=0; i < a.size ();  i++)
    {
      delete a[i];
    }
  a.clear ();
}

#endif // PARRAY_HH

