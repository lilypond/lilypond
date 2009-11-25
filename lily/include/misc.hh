/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
using namespace std;

#include "std-vector.hh"
#include "interval.hh"

double log_2 (double x);
int intlog2 (int d);

inline int
sign (int i)
{
  if (i < 0)
    return -1;
  else if (i)
    return 1;
  else return 0;
}

inline int
shift_left (int value, int shiftamount)
{
 if (shiftamount < 0) return (value >> -shiftamount); 
  else return (value << shiftamount);
}

inline Real
linear_interpolate (Real x, Real x1, Real x2, Real y1, Real y2)
{
  return (x2 - x) / (x2 - x1) * y1
    + (x - x1) / (x2 - x1) * y2;
}

Real directed_round (Real f, Direction d);

Real peak_around (Real epsilon,  Real threshold, Real x);
Real convex_amplifier (Real standard_x, Real increase_factor, Real x);
string camel_case_to_lisp_identifier (string in);
vsize utf8_char_len (char);

#endif

