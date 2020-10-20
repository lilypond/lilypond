/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2020 Jan Nieuwenhuizen <janneke@gnu.org>

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

#ifndef STD_VECTOR_HH
#define STD_VECTOR_HH

#if 0

/*
  leads to dubious crashes - libstdc++  bug?
*/
#ifdef DEBUG
#define _GLIBCXX_DEBUG 1
#endif
#endif

#include <algorithm>   /* find, reverse, sort */
#include <functional>  /* unary_function */
#include <cassert>
#include <string>

#include "compare.hh"

#ifndef VSIZE
#define VSIZE
typedef size_t vsize;
#define VPOS ((vsize) -1)
#endif

#include <vector>

template<typename T>
T const &
boundary (std::vector<T> const &v, int dir, vsize i)
{
  assert (dir);
  return v[dir == -1 ? i : v.size () - 1 - i];
}

template<typename T>
T &
boundary (std::vector<T> &v, int dir, vsize i)
{
  assert (dir);
  return v[dir == -1 ? i : v.size () - 1 - i];
}

template<typename T>
T const &
back (std::vector<T> const &v, vsize i)
{
  return v[v.size () - i - 1];
}

template<typename T>
T &
back (std::vector<T> &v, vsize i)
{
  return v[v.size () - i - 1];
}

template<typename T>
void
concat (std::vector<T> &v, std::vector<T> const &w)
{
  v.insert (v.end (), w.begin (), w.end ());
}

template<typename T, typename Compare>
vsize
lower_bound (std::vector<T> const &v,
             T const &key,
             Compare less,
             vsize b = 0, vsize e = VPOS)
{
  if (e == VPOS)
    e = v.size ();
  typename std::vector<T>::const_iterator i = lower_bound (v.begin () + b,
                                                           v.begin () + e,
                                                           key,
                                                           less);

  return i - v.begin ();
}

template<typename T, typename Compare>
vsize
upper_bound (std::vector<T> const &v,
             T const &key,
             Compare less,
             vsize b = 0, vsize e = VPOS)
{
  if (e == VPOS)
    e = v.size ();

  typename std::vector<T>::const_iterator i = upper_bound (v.begin () + b,
                                                           v.begin () + e,
                                                           key,
                                                           less);

  return i - v.begin ();
}

template<typename T, typename Compare>
vsize
binary_search (std::vector<T> const &v,
               T const &key,
               Compare less,
               vsize b = 0, vsize e = VPOS)
{
  vsize lb = lower_bound (v, key, less, b, e);

  if (lb == v.size () || less (key, v[lb]))
    return VPOS;
  return lb;
}

template<typename T, typename Compare>
void
vector_sort (std::vector<T> &v,
             Compare less,
             vsize b = 0, vsize e = VPOS)
{
  if (e == VPOS)
    e = v.size ();

  sort (v.begin () + b, v.begin () + e, less);
}

template<typename T, typename Compare>
void
vector_stable_sort (std::vector<T> &v,
                    Compare less,
                    vsize b = 0, vsize e = VPOS)
{
  if (e == VPOS)
    e = v.size ();

  stable_sort (v.begin () + b, v.begin () + e, less);
}

template<typename T>
void
reverse (std::vector<T> &v)
{
  // CHECKME: for a simple vector, like std::vector<int>, this should
  // expand to memrev.
  reverse (v.begin (), v.end ());
}

template<typename T>
void
uniq (std::vector<T> &v)
{
  v.erase (unique (v.begin (), v.end ()), v.end ());
}

template<typename T>
typename std::vector<T>::const_iterator
find (std::vector<T> const &v, T const &key)
{
  return find (v.begin (), v.end (), key);
}

template<typename T> struct del : public std::unary_function<T, void>
{
  void operator () (T x)
  {
    delete x;
    x = 0;
  }
};

template<typename T>
void
junk_pointers (std::vector<T> &v)
{
  // Hmm.
  for_each (v.begin (), v.end (), del<T> ());
  v.clear ();
}

std::vector<std::string> string_split (std::string str, char c);
std::string string_join (std::vector<std::string> const &strs, const std::string &infix);

#endif /* STD_VECTOR_HH */
