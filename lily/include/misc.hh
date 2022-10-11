/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef MISC_HH
#define MISC_HH

#include <cstdlib>

#include "interval.hh"

double log_2 (double x);

/*
  Return the 2-log, rounded down
*/
template <class T>
int
intlog2 (T d)
{
  // TODO: To support user-defined types, use namespace std and call
  // unqualified to_string().
  if (d <= 0)
    error ("intlog2 with negative argument: " + std::to_string (d));
  int i = 0;
  while ((d != 1))
    {
      d /= 2;
      i++;
    }

  assert (!(d / 2));
  return i;
}

inline int
sign (int i)
{
  if (i < 0)
    return -1;
  else if (i)
    return 1;
  else
    return 0;
}

// Shift value left by shiftamount; if shiftamount is negative, shift right
// instead.
template <class T>
constexpr T
shift_left (T value, int shiftamount)
{
  return (shiftamount >= 0) ? (value << shiftamount) : (value >> -shiftamount);
}

inline Real
linear_interpolate (Real x, Real x1, Real x2, Real y1, Real y2)
{
  return (x2 - x) / (x2 - x1) * y1 + (x - x1) / (x2 - x1) * y2;
}

inline Real
normalize (Real x, Real x1, Real x2)
{
  return (x - x1) / (x2 - x1);
}

Real directed_round (Real f, Direction d);

Real peak_around (Real epsilon, Real threshold, Real x);
Real convex_amplifier (Real standard_x, Real increase_factor, Real x);
std::string camel_case_to_lisp_identifier (const std::string &in);

#endif
