/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2020 Han-Wen Nienhuys <hanwen@lilypond.org>

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

#ifndef TUPLE_HH
#define TUPLE_HH

template <class T, int N> struct Tuple
{
  T t_array[N];
  Tuple () {}

  Tuple (T const *src)
  {
    for (int i = 0; i < N; i++)
      t_array[i] = src[i];
  }
};

template <class K> struct Tuple2 : public Tuple<K, 2>
{
  Tuple2 () {}

  Tuple2 (K a, K b)
  {
    Tuple<K, 2> *p (this); //  ugr.

    p->t_array[0] = a;
    p->t_array[1] = b;
  }
};

template <class T, int N>
inline bool
operator< (Tuple<T, N> const &t1, Tuple<T, N> const &t2)
{
  for (int i = 0; i < N; i++)
    {
      if (t1.t_array[i] > t2.t_array[i])
        return false;
      if (t1.t_array[i] < t2.t_array[i])
        return true;
    }

  return false;
}

#endif /* TUPLE_HH */
