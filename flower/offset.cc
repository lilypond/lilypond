/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "offset.hh"

#ifndef STANDALONE
string
Offset::to_string () const
{
  string s;
  s = string (" (") + ::to_string (coordinate_a_[X_AXIS]) + ", "
    + ::to_string (coordinate_a_[Y_AXIS]) + ")";
  return s;
}
#endif

/*
  free bsd fix by John Galbraith
*/

Offset
complex_multiply (Offset z1, Offset z2)
{
  Offset z;
  if (!isinf (z2[Y_AXIS]))
    {
      z[X_AXIS] = z1[X_AXIS] * z2[X_AXIS] - z1[Y_AXIS] * z2[Y_AXIS];
      z[Y_AXIS] = z1[X_AXIS] * z2[Y_AXIS] + z1[Y_AXIS] * z2[X_AXIS];
    }
  return z;
}

Offset
complex_conjugate (Offset o)
{
  o[Y_AXIS] = -o[Y_AXIS];
  return o;
}

Offset
complex_divide (Offset z1, Offset z2)
{
  z2 = complex_conjugate (z2);
  Offset z = complex_multiply (z1, z2);
  z *= 1 / z2.length ();
  return z;
}

Offset
complex_exp (Offset o)
{
  Real s = sin (o[Y_AXIS]);
  Real c = cos (o[Y_AXIS]);

  Real r = exp (o[X_AXIS]);

  return Offset (r * c, r * s);
}

Real
Offset::arg () const
{
  return atan2 (coordinate_a_[Y_AXIS], coordinate_a_[X_AXIS]);
}

Real
Offset::angle_degrees () const
{
  return arg () * 180 / M_PI;
}
/**
   euclidian vector length / complex modulus
*/
Real
Offset::length () const
{
  return sqrt (sqr (coordinate_a_[X_AXIS])
		    + sqr (coordinate_a_[Y_AXIS]));
}

bool
Offset::is_sane () const
{
  return !isnan (coordinate_a_[X_AXIS])
    && !isnan (coordinate_a_ [Y_AXIS])
    && !isinf (coordinate_a_[X_AXIS]) 
    && !isinf (coordinate_a_[Y_AXIS]);
}

Offset
Offset::direction () const
{
  Offset d = *this;
  d /= length (); 
  return d;
}

Offset
Offset::swapped () const
{
  return Offset (coordinate_a_[Y_AXIS], coordinate_a_[X_AXIS]);
}
