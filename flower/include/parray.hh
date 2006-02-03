/*
  parray.hh -- declare Pointer_array

  source file of the Flower Library

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef PARRAY_HH
#define PARRAY_HH

#ifndef STD_VECTOR_HH
#error array.hh is obsolete, use std-vector.hh
#endif

using namespace std;

namespace std {
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
  Array<void *>::data;
  Array<void *>::end;
  Array<void *>::clear;
  Array<void *>::erase;
  Array<void *>::resize;
  Array<void *>::size;
  Array<void *>::empty;
  Array<void *>::pop_back;


  /* Flower compat */
  Array<void *>::unordered_del;
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

  /* std::vector interface */
  //typedef T** iterator;
  //typedef T* const* iterator_const;

  Link_array (const_iterator begin, const_iterator end)
    : Array<void *> (begin, end)
  {
  }

  T *at (int i)
  {
    return (T *) Array<void *>::at (i);
  }
  T const *at (int i) const
  {
    return (T const *) Array<void *>::at (i);
  }

  /// access element
  T *&operator [] (int i)
  {
    return (T *&) Array<void *>::at (i);
  }
  /// access element
  T *const operator [] (int i) const
  {
    return (T *const) Array<void *>::at (i);
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
  void insert (iterator pos, const_iterator b, const_iterator e)
  {
    Array<void *>::insert (pos, b, e);
  }
  void push_back (T *t)
  {
    Array<void *>::push_back (t);
  }

  T *top (vsize j)
  {
    return (*this)[size_ - j - 1];
  }
  T *& top (vsize j) const
  {
    return (*this)[size_ - j - 1];
  }

  void substitute (T *old, T *new_p)
  {
    int i;
    while ((i = find_index (old)) >= 0)
      if (new_p)
	at (i) = new_p;
      else
	erase (begin () + i);
  }
  void unordered_substitute (T *old, T *new_p)
  {
    int i;
    while ((i = find_index (old)) >= 0)
      if (new_p)
	at (i) = new_p;
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
      if (!i || at (i - 1) != at (i))
	ls.push_back (at (i));
    *this = ls;
  }

  T *& boundary (int d, int i)
  {
    assert (d);
    if (d == 1)
      return top (i);
    else
      return at (i);
  }
  T *boundary (int d, int i)const
  {
    assert (d);
    if (d == 1)
      return top (i);
    else
      return at (i);
  }

  T **
  data ()
  {
    return (T**) Array<void *>::data ();
  }

  T * const*
  data () const
  {
    return (T**) Array<void *>::data ();
  }

  /**
     remove  i-th element, and return it.
  */
  T *get (vsize i)
  {
    T *t = at (i);
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
    Array<void *>::insert (end (), a2.begin (), a2.end ());
  }
  int find_index (T const *t) const
  {
    for (vsize i = 0; i < size (); i++)
      if (at (i) == t)
	return i;
    return -1;
  }
  T const *find (T const *t) const
  {
    int i = find_index (t);
    if (i >= 0)
      return at (i);
    else
      return 0;
  }

  void swap (vsize i, vsize j)
  {
    T *t ((*this)[i]);
    (*this)[i] = (*this)[j];
    (*this)[j] = t;
  }
  void
  reverse ()
  {
    vsize h = size () / 2;
    for (vsize i = 0, j = size () - 1; i < h; i++, j--)
      swap (i, j);
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
    if (compare (at (i), at (lower)) < 0)
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

template<class T>
int
binary_search (Link_array<T> const &arr, T *t,
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

template<class T>
void
reverse (Link_array<T> &v)
{
  vsize h = v.size () / 2;
  for (vsize i = 0, j = v.size () - 1; i < h; i++, j--)
    swap (v[i], v[j]);
}

template<typename T>
void
concat (Link_array<T> &v, Link_array<T> const& w)
{
  v.insert (v.end (), w.begin (), w.end ());
}

template<typename T>
void
vector_sort (Link_array<T> &v, int (*compare) (T *const &, T * const &),
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
    if (compare (v[i], v[lower]) < 0)
      swap (v[++last], v[i]);
  swap (v[lower], v[last]);
  vector_sort (v, compare, lower, last - 1);
  vector_sort (v, compare, last + 1, upper);
}

template<typename T>
void
uniq (Link_array<T> &v)
{
  v.uniq ();
}

template<typename T>
typename Array<void *>::const_iterator
find (Link_array<T> const &v, T * const& key)
{
  return v.begin () + v.find_index (key);
}

}

#endif // PARRAY_HH

