/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef PQUEUE_HH
#define PQUEUE_HH

#include <cassert>
#include <vector>

template <class K, class T>
struct PQueue_ent
{
  T val;
  K key;
};

template <class K, class T>
int
compare (PQueue_ent<K, T> const &e1, PQueue_ent<K, T> const &e2)
{
  return compare (e1.key, e2.key);
}

/**
   Priority queue using a (variable size) in-situ heap.

   Hungarian postfix pq

   TODO: add increase/decrease operations,
   add max () operation
*/
template <class T>
class PQueue
{
  using Vector = std::vector<T>;
  using size_type = typename Vector::size_type;

  Vector heap_array_;
  T &elt (size_type i) { return heap_array_[i - 1]; }
  T const &elt (size_type i) const { return heap_array_[i - 1]; }

public:
  /** acces an heap element.  Careful with this, as changing the
      priority might fuck up the invariants

      @param 1 <= i < size () */
  T &operator[] (size_type i) { return heap_array_[i]; }
  T operator[] (size_type i) const { return heap_array_[i]; }
  void OK () const
  {
#ifdef DEBUG
    for (size_type i = 2; i <= size (); i++)
      assert (compare (elt (i / 2), elt (i)) <= 0);
#endif
  }
  T front () const
  {
    return elt (1);
  }
  size_type size () const
  {
    return heap_array_.size ();
  }
  void insert (T v)
  {
    heap_array_.push_back (v);
    size_type i = heap_array_.size ();
    size_type j = i / 2;
    while (j)
      {
        if (compare (elt (j), v) > 0)
          {
            elt (i) = elt (j);
            i = j;
            j = i / 2;
          }
        else
          break;
      }
    elt (i) = v;
    OK ();
  }
  T max () const
  {
    //int first_leaf_i = size ();
    T max_t;
    return max_t;
  }
  void delmin ()
  {
    assert (size ());
    T last = heap_array_.back ();

    size_type mini = 2;
    size_type lasti = 1;

    while (mini < size ())
      {
        if (compare (elt (mini + 1), elt (mini)) < 0)
          mini++;
        if (compare (last, elt (mini)) < 0)
          break;
        elt (lasti) = elt (mini);
        lasti = mini;
        mini *= 2;
      }
    elt (lasti) = last;
    heap_array_.pop_back ();
    OK ();
  }
  T get ()
  {
    T t = front ();
    delmin ();
    return t;
  }
};

#endif // PQUEUE_HH
