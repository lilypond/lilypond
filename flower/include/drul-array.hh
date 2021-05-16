/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2021 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef DRUL_ARRAY_HH
#define DRUL_ARRAY_HH

#include "direction.hh"
#include "real.hh"

#include <cassert>

/**
   Left/right or Up/down arrays. Drul is nicer sounding than udlr
*/
template<class T>
struct Drul_array
{
  T array_[2];
  T &at (Direction d)
  {
    assert (d == 1 || d == -1);
    return array_[d > 0];
  }
  T const &at (Direction d) const
  {
    assert (d == 1 || d == -1);
    return array_[d > 0];
  }
  T &operator [] (Direction d)
  {
    return at (d);
  }
  T const &operator [] (Direction d) const
  {
    return at (d);
  }
  Drul_array ()
  {
  }
  Drul_array (T const &t1, T const &t2)
  {
    set (t1, t2);
  }
  void set (T const &t1, T const &t2)
  {
    array_[0] = t1;
    array_[1] = t2;
  }
  T delta () const
  {
    return at (RIGHT) - at (LEFT);
  }
  Real linear_combination (Real x) const;
};

template<class T>
void
scale_drul (Drul_array<T> *dr, T x)
{
  dr->at (LEFT) *= x;
  dr->at (RIGHT) *= x;
}

template <>
inline Real
Drul_array<Real>::linear_combination (Real x) const
{
  return ((1.0 - x) * at (LEFT)
          + (x + 1.0) * at (RIGHT)) * 0.5;
}

#endif /* DRUL_ARRAY_HH */
