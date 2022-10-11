/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2022 Jean Abou Samra <jean@abou-samra.fr>

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

#include "bezier.hh"
#include "lily-guile.hh"

LY_DEFINE (ly_bezier_extent, "ly:bezier-extent", 2, 0, 0,
           (SCM control_points, SCM axis),
           R"(
Compute the extent of the Bézier curve defined by @var{control-points}
along @var{axis}.
           )")
{
  LY_ASSERT_TYPE (is_scm<Bezier>, control_points, 1);
  LY_ASSERT_TYPE (is_scm<Axis>, axis, 2);
  Bezier b = from_scm<Bezier> (control_points);
  Axis a = from_scm<Axis> (axis);
  Interval extent = b.extent (a);
  return to_scm (extent);
}

LY_DEFINE (ly_bezier_extract, "ly:bezier-extract", 3, 0, 0,
           (SCM control_points, SCM t_min, SCM t_max),
           R"(
Return a sub-curve of the Bézier curve defined by
@var{control-points}.  The sub-curve is delimited by the curve points
indexed by @var{t-min} and @var{t-max} (between 0 and 1, 0 = first
control point, 1 = last control point).  A sub-curve of a Bézier curve
is in turn a Bézier curve.
           )")
{
  LY_ASSERT_TYPE (is_scm<Bezier>, control_points, 1);
  LY_ASSERT_TYPE (is_scm<Real>, t_min, 2);
  LY_ASSERT_TYPE (is_scm<Real>, t_max, 3);
  Bezier b = from_scm<Bezier> (control_points);
  Real t_min_cpp = from_scm<Real> (t_min);
  Real t_max_cpp = from_scm<Real> (t_max);
  return to_scm (b.extract (t_min_cpp, t_max_cpp));
}
