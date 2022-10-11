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

#ifndef DRUL_ARRAY_HH
#define DRUL_ARRAY_HH

#include "direction.hh"
#include "real.hh"

#include <cassert>

/**
   Left/right or Up/down arrays. Drul is nicer sounding than udlr
*/
template <class T>
struct Drul_array
{
private:
  T array_[2];

public:
  // By default, value-initialize both elements.
  constexpr Drul_array ()
    : array_ {}
  {
  }
  constexpr Drul_array (T const &t1, T const &t2)
    : array_ {t1, t2}
  {
  }
  constexpr Drul_array (const Drul_array &) = default;
  constexpr Drul_array (Drul_array &&) = default;
  ~Drul_array () = default; // N.B. non-virtual

  Drul_array &operator= (const Drul_array &) = default;
  Drul_array &operator= (Drul_array &&) = default;

  T &at (Direction d)
  {
    assert (d); // constexpr in C++17
    return array_[d > CENTER];
  }

  T const &at (Direction d) const
  {
    assert (d); // constexpr in C++17
    return array_[d > CENTER];
  }

  T &operator[] (Direction d) { return at (d); }

  constexpr T const &operator[] (Direction d) const { return at (d); }

  constexpr T &front () // at (Direction::negative ())
  {
    return array_[0];
  }

  constexpr T const &front () const // at (Direction::negative ())
  {
    return array_[0];
  }

  constexpr T &back () // at (Direction::positive ())
  {
    return array_[1];
  }

  constexpr T const &back () const // at (Direction::positive ())
  {
    return array_[1];
  }
};

template <class T1, class T2>
constexpr void
scale_drul (Drul_array<T1> *dr, T2 x)
{
  dr->front () *= x;
  dr->back () *= x;
}

#endif /* DRUL_ARRAY_HH */
