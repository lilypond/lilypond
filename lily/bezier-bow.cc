/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2022 Jan Nieuwenhuizen <janneke@gnu.org>

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

// Needed because of extension definitions for POSIX functions.
#include "config.hh"

#include "misc.hh"
#include "bezier.hh"

#include <cmath>

static Real
F0_1 (Real x)
{
  return 2 / M_PI * atan (M_PI * x / 2);
}

Real
slur_height (Real width, Real h_inf, Real r_0)
{
  return F0_1 (width * r_0 / h_inf) * h_inf;
}

/*
  ^                x                    x
  |
  height   <indent>
  |
  v      x                                       x



  For small w, the height should be proportional to w, for w ->
  infinity, the height should rise to a limit asymptotically.

  Hence we take F (x) such that
  F (0) = 0 , F' (0) = 1, and F (infty) = 1

  and use

  h = h_infinity * F (x * r_0 / h_infinity)


  Examples:

  * F (x) = 2/pi * atan (pi x/2)

  * F (x) = 1/alpha * x^alpha / (1 + x^alpha)

  * (etc.)

  [with the 2nd recipe you can determine how quickly the conversion from
  `small' slurs to `big' slurs occurs.]

  Although this might seem cand_idates to SCM-ify, it is not all clear
  which parameters (ie. h_inf, r_0, F (.)) should be candidates for
  this.  At present h_inf and r_0 come from layout settings, but we did
  no experiments for determining the best combinations of F, h_inf and
  r_0.


  The indent is proportional to the height of the slur for small
  slurs.  For large slurs, this gives a certain hookiness at the end,
  so we increase the indent.

  indent = G (w)

  w -> 0,  G (w) -> .33 w


  (due to derivative constraints, we cannot have indent > len/3)

  w -> inf, G (w) -> 2*h_inf

  i.e.


  G (0) = 0 , G'(0) 1/3, G (infty) = 2h_inf

  solve from

  G (w) = r  + p/(w+q)

  yields

  G (w) = 2 h_inf - max_fraction * q^2/ (w + q)

  with q = 2 h_inf
*/

void
get_slur_indent_height (Real *indent, Real *height, Real width, Real h_inf,
                        Real r_0)
{
  Real max_fraction = 1.0 / 3.1;
  *height = slur_height (width, h_inf, r_0);

  Real q = 2 * h_inf / max_fraction;
  *indent = 2 * h_inf - sqr (q) * max_fraction / (width + q);
}

Bezier
slur_shape (Real width, Real h_inf, Real r_0)
{
  Real indent;
  Real height;

  get_slur_indent_height (&indent, &height, width, h_inf, r_0);

  Bezier curve;
  curve.control_[0] = Offset (0, 0);
  curve.control_[1] = Offset (indent, height);
  curve.control_[2] = Offset (width - indent, height);
  curve.control_[3] = Offset (width, 0);
  return curve;
}
