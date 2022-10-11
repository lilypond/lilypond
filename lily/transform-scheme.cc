/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2018--2022  David Kastrup <dak@gnu.org>

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

#include "lily-guile.hh"
#include "offset.hh"
#include "transform.hh"

const char *const Transform::type_p_name_ = "ly:transform?";

SCM
Transform::call (SCM arg)
{
  if (Transform *tr = unsmob<Transform> (arg))
    {
      return (*this) (*tr).smobbed_copy ();
    }
  if (scm_is_number (arg))
    {
      Offset res
        = (*this) (Offset (scm_c_real_part (arg), scm_c_imag_part (arg)));
      return scm_c_make_rectangular (res[X_AXIS], res[Y_AXIS]);
    }
  LY_ASSERT_TYPE (is_scm<Offset>, arg, 1);
  return to_scm<Offset> ((*this) (from_scm<Offset> (arg)));
}

LY_DEFINE (ly_make_transform, "ly:make-transform", 0, 6, 0,
           (SCM xx, SCM yx, SCM xy, SCM yy, SCM x0, SCM y0),
           R"(
Create a transform.  Without options, it is the identity transform.  Given four
arguments @var{xx}, @var{yx}, @var{xy}, and @var{yy}, it is a linear transform.
Given six arguments (with @var{x0} and @var{y0} last), it is an affine
transform.

Transforms can be called as functions on other transforms (concatening them) or
on points given either as complex number or real number pair.  See also
@code{ly:make-rotation}, @code{ly:make-scaling}, and
@code{ly:make-translation}.
           )")
{
  if (SCM_UNBNDP (xx))
    return Transform ().smobbed_copy ();
  LY_ASSERT_TYPE (scm_is_real, xx, 1);
  LY_ASSERT_TYPE (scm_is_real, yx, 2);
  LY_ASSERT_TYPE (scm_is_real, xy, 3);
  LY_ASSERT_TYPE (scm_is_real, yy, 4);
  if (SCM_UNBNDP (x0))
    {
      // Constructor argument order follows that of Pango
      return Transform (from_scm<Real> (xx), from_scm<Real> (xy),
                        from_scm<Real> (yx), from_scm<Real> (yy), 0, 0)
        .smobbed_copy ();
    }
  LY_ASSERT_TYPE (scm_is_real, x0, 5);
  LY_ASSERT_TYPE (scm_is_real, y0, 6);
  return Transform (from_scm<Real> (xx), from_scm<Real> (xy),
                    from_scm<Real> (yx), from_scm<Real> (yy),
                    from_scm<Real> (x0), from_scm<Real> (y0))
    .smobbed_copy ();
}

LY_DEFINE (ly_make_scaling, "ly:make-scaling", 1, 1, 0, (SCM scale, SCM scaley),
           R"(
Create a scaling transform from argument @var{scale} and optionally
@var{scaley}.  When both arguments are given, they must be real and give the
scale in x and y@tie{}direction.  If only @var{scale} is given, it may also be
complex to indicate a scaled rotation in the manner of complex number
rotations, or a pair of reals for specifying different scales in x and
y@tie{}direction like with the first calling convention.
           )")
{
  if (SCM_UNBNDP (scaley))
    {
      if (scm_is_pair (scale))
        {
          LY_ASSERT_TYPE (is_scm<Offset>, scale, 1);
          Offset xy = from_scm<Offset> (scale);
          return Transform (xy[X_AXIS], 0.0, 0.0, xy[Y_AXIS], 0.0, 0.0)
            .smobbed_copy ();
        }
      LY_ASSERT_TYPE (scm_is_number, scale, 1);
      Real re = scm_c_real_part (scale);
      Real im = scm_c_imag_part (scale);
      // Constructor argument order follows that of Pango
      // 0.0-im avoids -0.0
      return Transform (re, im, 0.0 - im, re, 0.0, 0.0).smobbed_copy ();
    }
  LY_ASSERT_TYPE (scm_is_real, scale, 1);
  LY_ASSERT_TYPE (scm_is_real, scaley, 2);
  return Transform (from_scm<Real> (scale), 0.0, 0.0, from_scm<Real> (scaley),
                    0.0, 0.0)
    .smobbed_copy ();
}

LY_DEFINE (ly_make_rotation, "ly:make-rotation", 1, 1, 0,
           (SCM angle, SCM center),
           R"(
Make a transform rotating by @var{angle} in degrees.  If @var{center} is given
as a pair of coordinates, it is the center of the rotation, otherwise the
rotation is around @w{(0, 0)}.
           )")
{
  LY_ASSERT_TYPE (scm_is_real, angle, 1);
  if (!SCM_UNBNDP (center))
    LY_ASSERT_TYPE (is_scm<Offset>, center, 2);
  return Transform (from_scm<Real> (angle), from_scm (center, Offset (0, 0)))
    .smobbed_copy ();
}

LY_DEFINE (ly_make_translation, "ly:make-translation", 1, 1, 0, (SCM x, SCM y),
           R"(
Make a transform translating by @var{x} and @var{y}. If only @var{x} is given,
it can also be a complex number or a pair of numbers indicating the offset to
use.
           )")
{
  if (!SCM_UNBNDP (y))
    {
      LY_ASSERT_TYPE (scm_is_real, x, 1);
      LY_ASSERT_TYPE (scm_is_real, y, 2);
      return Transform (Offset (from_scm<Real> (x), from_scm<Real> (y)))
        .smobbed_copy ();
    }
  if (scm_is_number (x))
    {
      return Transform (Offset (scm_c_real_part (x), scm_c_imag_part (x)))
        .smobbed_copy ();
    }
  LY_ASSERT_TYPE (is_scm<Offset>, x, 1);
  return Transform (from_scm<Offset> (x)).smobbed_copy ();
}

LY_DEFINE (ly_transform_2_list, "ly:transform->list", 1, 0, 0, (SCM transform),
           R"(
Convert a transform matrix to a list of six values.  Values are @var{xx},
@var{yx}, @var{xy}, @var{yy}, @var{x0}, @var{y0}.
           )")
{
  auto *const tr = LY_ASSERT_SMOB (Transform, transform, 1);
  SCM res = ly_list (to_scm (tr->get_xx ()), to_scm (tr->get_yx ()),
                     to_scm (tr->get_xy ()), to_scm (tr->get_yy ()),
                     to_scm (tr->get_x0 ()), to_scm (tr->get_y0 ()));
  scm_remember_upto_here_1 (transform);
  return res;
}
