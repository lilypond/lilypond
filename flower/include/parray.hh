/*
  parray.hh -- declare Pointer_array

  source file of the Flower Library

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef PARRAY_HH
#define PARRAY_HH

#include "std-vector.hh"

/**
   an array of pointers.

   TODO
   should init to 0.
*/
template<class T>
class Link_array : private Array<void *>
{

  Link_array (Array<void *> const &v)
    :Array<void *> (v)
  {
  }

public:
  Link_array ()
  {
  }

  T *const &back() const
  {
    return (T * const &) Array<void *>::back();
  }

  T *&back ()
  {
    return (T *&) Array<void *>::back ();
  }

  Array<void *>::begin;
  Array<void *>::end;
  Array<void *>::clear;
  Array<void *>::erase;
  Array<void *>::resize;
  Array<void *>::size;
  Array<void *>::empty;
  Array<void *>::pop_back;


  /* Flower compat */
  Array<void *>::unordered_del;
  Array<void *>::reverse;
  Array<void *>::tighten_maxsize;

  static int default_compare (T *const &p1, T *const &p2)
  {
    /* can't do p1 -p2, since T might be an incomplete type */
    if (p1 < p2)
      return -1;
    if (p2 < p1)
      return 1;
    return 0;
  }
  Link_array (T *const *tp, int n)
    : Array<void *> ((void **)tp, n)
  {
  }

  Link_array (Link_array<T> const &src)
    : Array<void *> (src)
  {
  }

  /// access element
  T *elem (int i) const
  {
    return elem_ref (i);
  }
  T *&elem_ref (int i) const
  {
    return (T *&) Array<void *>::elem_ref (i);
  }

  /// access element
  T *&operator [] (int i)
  {
    return (T *&) Array<void *>::elem_ref (i);
  }
  /// access element
  T *const operator [] (int i) const
  {
    return (T *const) Array<void *>::elem (i);
  }
  T *pop ()
  {
    T* t = (T *) Array<void *>::back ();
    pop_back ();
    return t;
  }
  void insert (iterator b, T *t)
  {
    Array<void *>::insert (b, t);
  }
  void push_back (T *t)
  {
    Array<void *>::push_back (t);
  }

  /// return last entry
  T *top (int j) const
  {
    return (T *) Array<void *>::top (j);
  }
  T *& top (int i)
  {
    return (T *&) Array<void *>::top (i);
  }

  void substitute (T *old, T *new_p)
  {
    int i;
    while ((i = find_index (old)) >= 0)
      if (new_p)
	elem_ref (i) = new_p;
      else
	erase (begin () + i);
  }
  void unordered_substitute (T *old, T *new_p)
  {
    int i;
    while ((i = find_index (old)) >= 0)
      if (new_p)
	elem_ref (i) = new_p;
      else
	unordered_del (i);
  }
  void default_sort ()
  {
    sort (default_compare);
  }

  // quicksort.
  void sort (int (*compare) (T *const &, T *const &),
	     int lower = -1, int upper = -1);

  void uniq ()
  {
    Link_array<T> ls;
    for (vsize i = 0; i < size (); i++)
      if (!i || elem (i - 1) != elem (i))
	ls.push_back (elem (i));
    *this = ls;
  }

  T *& boundary (int d, int i)
  {
    assert (d);
    if (d == 1)
      return top (i);
    else
      return elem_ref (i);
  }
  T *boundary (int d, int i)const
  {
    assert (d);
    if (d == 1)
      return top (i);
    else
      return elem_ref (i);
  }

  T ** accesses () const
  {
    return (T **) Array<void *>::accesses ();
  }
  /**
     remove  i-th element, and return it.
  */
  T *get (vsize i)
  {
    T *t = elem (i);
    Array<void*>::erase (Array<void*>::begin () + i);
    return t;
  }
  Link_array<T>
  slice (int l, int u) const
  {
    return Array<void *>::Array (begin () + l, begin () + u);
  }
  void concat (Link_array<T> const &a2)
  {
    Array<void *>::concat (a2);
  }
  int find_index (T const *t) const
  {
    for (vsize i = 0; i < size (); i++)
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
typecasts (Link_array<V> const &a, T * /* dummy */)
{
  Link_array<T> ret;
  for (vsize i = a.size (); i--;)
    ret.push_back (dynamic_cast<T *> (a[i]));	// ugh?
  return ret;
}

template<class T> inline void
Link_array<T>::sort (int (*compare) (T *const &, T *const &), int lower, int upper)
{
  if (lower < 0)
    {
      lower = 0;
      upper = size () - 1;
    }
  if (lower >= upper)
    return;
  swap (lower, (lower + upper) / 2);
  int last = lower;
  for (int i = lower +1; i <= upper; i++)
    if (compare (elem (i), elem (lower)) < 0)
      swap (++last, i);
  swap (lower, last);
  sort (compare, lower, last - 1);
  sort (compare, last + 1, upper);
}

template<class T>
void
junk_pointers (Link_array<T> &a)
{
  for (vsize i = 0; i < a.size (); i++)
    delete a[i];
  a.clear ();
}

/*
  lookup with binsearch, return tokencode.
*/
template<class T>
int
binsearch (Array<T> const &arr, T t, int (*compare) (T const &, T const &))
{
  int cmp;
  int result;
  int lo = 0;
  int hi = arr.size ();

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
  /* not found */
  return -1;
}

template<class T>
int
binsearch_links (Link_array<T> const &arr, T *t,
		 int (*compare) (T *const &, T *const &))
{
  int cmp;
  int result;
  int lo = 0;
  int hi = arr.size ();

  if (hi == 0)
    return -1;

  /* binary search */
  do
    {
      cmp = (lo + hi) / 2;

      result = (*compare) (t, arr[cmp]);

      if (result < 0)
	hi = cmp;
      else
	lo = cmp;
    }
  while (hi - lo > 1);

  if (!compare (t, arr[lo]))
    return lo;
  /* not found */
  return -1;
}

template<class T>
void
binary_search_bounds (Link_array<T> const &table,
		      T const *key, int (*compare) (T *const &, T *const &),
		      int *lo,
		      int *hi)
{
  int cmp;
  int result;

  /* binary search */
  do
    {
      cmp = (*lo + *hi) / 2;

      result = (*compare) ((T *) key, table[cmp]);

      if (result < 0)
	*hi = cmp;
      else
	*lo = cmp;
    }
  while (*hi - *lo > 1);
}

#endif // PARRAY_HH

