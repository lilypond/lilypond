/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "least-squares.hh"

#include "warn.hh"

using std::vector;

void
minimise_least_squares (Real *coef, Real *offset, vector<Offset> const &input)
{
  Real sx = 0.0;
  Real sy = 0.0;
  Real sqx = 0.0;
  Real sxy = 0.0;

  for (const auto &offset : input)
    {
      Real x = offset[X_AXIS];
      Real y = offset[Y_AXIS];
      sx += x;
      sy += y;
      sqx += sqr (x);
      sxy += x * y;
    }

  const auto count = static_cast<Real> (input.size ());

  *coef = 0.0;
  *offset = 0.;

  Real den = (count * sqx - sqr (sx));
  if (!count || !den)
    {
      programming_error ("minimise_least_squares ():  Nothing to minimise\n"
                         "This means that vertical spacing is triggered\n"
                         "before line breaking\n");
      *coef = 0.0;
      *offset = count ? sy / count : 0.0;
    }
  else
    {
      *coef = (count * sxy - sx * sy) / den;
      *offset = (sy - (*coef) * sx) / count;
    }
}
