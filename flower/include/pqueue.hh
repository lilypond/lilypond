/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2023 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include <algorithm>
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

  static bool greater (T const &e1, T const &e2)
  {
    return compare (e1, e2) > 0;
  }

public:
  /** acces an heap element.  Careful with this, as changing the
      priority might fuck up the invariants

      @param 1 <= i < size () */
  T &operator[] (size_type i) { return heap_array_[i]; }
  T const &operator[] (size_type i) const { return heap_array_[i]; }
  T const &front () const { return heap_array_.front (); }
  size_type size () const { return heap_array_.size (); }
  void insert (T v)
  {
    heap_array_.push_back (v);
    std::push_heap (heap_array_.begin (), heap_array_.end (), greater);
  }
  void delmin ()
  {
    assert (size ());
    std::pop_heap (heap_array_.begin (), heap_array_.end (), greater);
    heap_array_.pop_back ();
  }
  T get ()
  {
    assert (size ());
    std::pop_heap (heap_array_.begin (), heap_array_.end (), greater);
    T t = heap_array_.back ();
    heap_array_.pop_back ();
    return t;
  }
};

#endif // PQUEUE_HH
