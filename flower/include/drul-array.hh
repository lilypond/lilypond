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

#include "assert.hh"
#include "direction.hh"
#include "real.hh"

/**
   Left/right or Up/down arrays. Drul is nicer sounding than udlr
*/
template<class T>
struct Drul_array
{
private:
  T array_[2];

public:
  // By default, value-initialize both elements.
  constexpr Drul_array () : array_ {} {}
  constexpr Drul_array (T const &t1, T const &t2) : array_ {t1, t2} {}
  constexpr Drul_array (const Drul_array &) = default;
  constexpr Drul_array (Drul_array &&) = default;
  ~Drul_array () = default; // N.B. non-virtual

  Drul_array &operator = (const Drul_array &) = default;
  Drul_array &operator = (Drul_array &&) = default;

  T &at (Direction d)
  {
    return constexpr_assert (d), array_[d > CENTER];
  }

  constexpr T const &at (Direction d) const
  {
    return constexpr_assert (d), array_[d > CENTER];
  }

  T &operator [] (Direction d)
  {
    return at (d);
  }

  constexpr T const &operator [] (Direction d) const
  {
    return at (d);
  }

  void set (T const &t1, T const &t2)
  {
    array_[0] = t1;
    array_[1] = t2;
  }

  // Compute the average of the elements.  Requires T to be divisible by int.
  constexpr T average () const
  {
    return (at (RIGHT) + at (LEFT)) / 2;
  }

  constexpr T delta () const
  {
    return at (RIGHT) - at (LEFT);
  }
};

template<class T1, class T2>
void
scale_drul (Drul_array<T1> *dr, T2 x)
{
  dr->at (LEFT) *= x;
  dr->at (RIGHT) *= x;
}

#endif /* DRUL_ARRAY_HH */
