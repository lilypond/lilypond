/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2018--2020  David Kastrup <dak@gnu.org>

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

/*
 * This file provides a hook into Pango's transform matrices, reusing
 * them as a smobbable data structure for affine transforms.  There is
 * a very important difference as opposed to Pango's coordinate
 * system: LilyPond's y-axis value increase _upwards_ while Pango's
 * y-axis values increase _downwards.  This means that the notions of
 * "counterclockwise" and "clockwise" in the respective documentation
 * are opposite and rotation functions interpret the angle sign
 * differently.
 *
 * When applied to a point, the basic transformation can be written as
 *
 *    [ [ xx, xy, x0 ]     [ [ x ]
 *      [ yx, yy, y0 ]   *   [ y ]
 *      [ 0,  0,  1  ] ]     [ 1 ] ]
 *
 * The last row of both transform matrix and coordinates is not stored
 * but merely implied.
 */

#include "transform.hh"
#include "offset.hh"

const char *const Transform::type_p_name_ = "ly:transform?";

const Transform Transform::identity;

Transform::Transform (Real angle, Offset center)
{
  // Don't use pango_matrix_rotate since it does not bother
  // maintaining sane behavior at multiples of 45 degrees
  Offset d = offset_directed (angle);
  xx = d[X_AXIS];
  xy = -d[Y_AXIS];
  yx = d[Y_AXIS];
  yy = d[X_AXIS];
  x0 = center[X_AXIS];
  y0 = center[Y_AXIS];
  d = (*this) (-center);
  x0 = d[X_AXIS];
  y0 = d[Y_AXIS];
}

Transform &
Transform::concat (const Transform &t)
{
  pango_matrix_concat (this, &t);
  return *this;
}

Transform &
Transform::translate (Offset p)
{
  pango_matrix_translate (this, p[X_AXIS], p[Y_AXIS]);
  return *this;
}

Transform &
Transform::rotate (Real angle, Offset center)
{
  Transform tmp (angle, center);
  return concat (tmp);
}

Transform &
Transform::scale (Real xscale, Real yscale)
{
  pango_matrix_scale (this, xscale, yscale);
  return *this;
}

Offset
Transform::operator() (Offset point) const
{
  pango_matrix_transform_point (this, &point[X_AXIS], &point[Y_AXIS]);
  return point;
}

Transform
Transform::operator() (const Transform &t) const
{
  // This looks dangerous regarding garbage collection.  However,
  // there is no allocation happening inside of this routine, so the
  // lack of garbage protection for the incoming argument should not
  // prove an issue.
  return Transform (*this).concat (t);
}

Offset
scm_transform (SCM trans, Offset p)
{
  if (Transform *tp = unsmob<Transform> (trans))
    return (*tp) (p);
  return p;
}

Transform
scm_transform (SCM trans, const Transform &t)
{
  if (Transform *tp = unsmob<Transform> (trans))
    return (*tp) (t);
  return t;
}
