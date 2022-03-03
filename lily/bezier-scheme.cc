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

LY_DEFINE (ly_bezier_extent, "ly:bezier-extent",
           2, 0, 0, (SCM control_points, SCM axis),
           R"(
Compute the extent of the Bézier curve defined by @var{control-points}
along @var{axis}.
           )")
{
  LY_ASSERT_TYPE (is_scm<Bezier>, control_points, 1);
  LY_ASSERT_TYPE (is_scm<Axis>, axis, 2);
  Bezier b = from_scm<Bezier> (control_points);
  Axis a = from_scm<Axis> (axis);
  Interval extent = b.extent(a);
  return to_scm (extent);
}
