/*
  parray.hh -- declare Pointer_array

  source file of the Flower Library

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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

  Link_array (Array<void*> v)
    :Array<void*> (v)
    {
    }
public:
  Link_array ()
    {}

  static int default_compare (T *const& p1, T  *const&p2)
  {
    /* can't do p1 -p2, since T might be an incomplete type */
    if (p1 < p2)
      return -1 ;
    if (p2 < p1)
      return 1;
    return 0;
  }
  Link_array (T * const *tp, int n)
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
  T *&elem_ref (int i) const
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
  void substitute (T *old, T*new_p)
    {
      int i;
      while ((i = find_index (old)) >=0) 
	if (new_p)
	  elem_ref (i) =new_p;
	else
	  del (i);
    }
  void unordered_substitute (T* old, T * new_p)
    {
      int i;
      while ((i = find_index (old)) >=0) 
	if (new_p)
	  elem_ref (i) =new_p;
	else {
	  unordered_del (i);
	}
    
    }
  void default_sort () {
    sort (default_compare);
  }
  // quicksort.
  void sort (int (*compare) (T *const&,T *const&),
	     int lower = -1, int upper = -1);
  
  void uniq () {
    Link_array<T> ls;
    for (int i=0; i < size (); i++) 
      if (!i || elem (i-1) != elem (i))
	ls.push (elem (i)); 
    *this = ls;
  }
  Array<void*>::del;
  Array<void*>::unordered_del;  
  Array<void*>::size;
  Array<void*>::clear;
  Array<void*>::set_size;
  Array<void*>::empty;
  Array<void*>::reverse;
  Array<void*>::tighten_maxsize;

  T *& boundary (int d, int i)
  {
    assert (d);
    if (d == 1)
      return top (i);
    else
      return elem_ref (i);
  }
  T * boundary (int d, int i)const
  {
    assert (d);
    if (d == 1)
      return top (i);
    else
      return elem_ref (i);
  }

  
  T ** accesses () const
    {
      return (T**) Array<void*>::accesses ();
    }
  T * get (int i)
    {
      return (T*) Array<void*>::get (i);
    }
  Link_array<T>
  slice (int l,int u)
    {
      return Array<void*>::slice (l,u);
    }
  void concat (Link_array<T> const &a2)
    {
      Array<void*>::concat (a2);
    }
  int find_index (T const * t) const {
    for (int i=0; i < size (); i++)
      if (elem (i) == t)
	return i;
    return -1;
  }
  T *find (T const *t) const
    {
      int i = find_index (t);
      if (i >= 0)
	return elem (i);
      else
	return 0;
    }
};

template<class T, class V>
Link_array<T>
typecasts (Link_array<V> const &a, T * /* dummy */ )
{
  Link_array<T> ret;
  for (int i=a.size (); i-- ; )
	ret.push (dynamic_cast<T*> (a[i]));	// ugh?
  return ret;
}



template<class T> inline void
Link_array<T>::sort (int (*compare)(T *const&,T *const&), int lower, int upper)
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
    if (compare (elem (i), elem (lower)) < 0)
      swap (++last,i);
  swap (lower, last);
  sort (compare, lower, last-1);
  sort (compare, last+1, upper);
}

template<class T>
void
junk_pointers (Link_array<T> &a)
{
  for (int i=0; i < a.size ();  i++)
    {
      delete a[i];
    }
  a.clear ();
}



/*
  lookup with binsearch, return tokencode.
*/
template<class T>
int
binsearchs (Array<T> const &arr, T t, int (*compare) (T const&,T const&))
{
  int lo;
  int hi;
  int cmp;
  int result;
  lo = 0;
  hi = maxkey;

  /* binary search */
  do
  {
      cmp = (lo + hi) / 2;

      result = compare (t, arr[cmp]);

      if (result < 0)
          hi = cmp;
      else
          lo = cmp;
    }
  while (hi - lo > 1);
  if (!compare (t, arr[lo]))
    return lo;
  else
    return -1;              /* not found */
}


template<class T>
int
binsearch_links (Link_array<T> const &arr, T *t,
		      int (*compare) (T *const&,T *const&),
		      int lo = 0, int hi = -1 )
{
  int cmp;
  int result;
  if (hi< 0)
    hi = arr.size ();

  if (hi == 0)
    return -1;
  
  /* binary search */
  do
  {
      cmp = (lo + hi) / 2;

      result = compare (t, arr[cmp]);

      if (result < 0)
          hi = cmp;
      else
          lo = cmp;
    }
  while (hi - lo > 1);
  if (!compare (t, arr[lo]))
    return lo;
  else
    return -1;              /* not found */
}


template<class T>
void
binary_search_bounds (Link_array<T> const &table,
		      T const *key, int (*compare) (T * const& , T *const &),
		      int *lo,
		      int *hi)
{
  int cmp;
  int result;

  /* binary search */
  do
  {
      cmp = (*lo + *hi) / 2;

      result = (*compare)  ((T*) key, table[cmp]);

      if (result < 0)
          *hi = cmp;
      else
          *lo = cmp;
    }
  while (*hi - *lo > 1);
}

#endif // PARRAY_HH

