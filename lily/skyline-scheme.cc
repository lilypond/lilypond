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

#include "lazy-skyline-pair.hh"
#include "ly-scm-list.hh"
#include "lily-guile.hh"
#include "skyline.hh"
#include "stencil.hh"

LY_DEFINE (ly_skyline_touching_point, "ly:skyline-touching-point", 2, 1, 0,
           (SCM skyline, SCM other_skyline, SCM horizon_padding),
           R"(
Get the point where @var{skyline} and @var{other-skyline} (having
opposite directions) reach their minimum distance.  If
@var{horizon-padding} is provided, one skyline is padded with it
first.
           )")
{
  auto *const sky = LY_ASSERT_SMOB (Skyline, skyline, 1);
  auto *const other = LY_ASSERT_SMOB (Skyline, other_skyline, 2);
  Real hp = 0;
  if (!SCM_UNBNDP (horizon_padding))
    {
      LY_ASSERT_TYPE (scm_is_real, horizon_padding, 3);
      hp = from_scm<Real> (horizon_padding);
    }
  return to_scm (sky->touching_point (*other, hp));
}

LY_DEFINE (ly_skyline_distance, "ly:skyline-distance", 2, 1, 0,
           (SCM skyline, SCM other_skyline, SCM horizon_padding),
           R"(
Compute the distance between the two skylines, padding by
@var{horizon-padding} if provided.
           )")
{
  auto *const sky = LY_ASSERT_SMOB (Skyline, skyline, 1);
  auto *const other = LY_ASSERT_SMOB (Skyline, other_skyline, 2);
  Real hp = 0;
  if (!SCM_UNBNDP (horizon_padding))
    {
      LY_ASSERT_TYPE (scm_is_real, horizon_padding, 3);
      hp = from_scm<Real> (horizon_padding);
    }
  return to_scm (sky->distance (*other, hp));
}

LY_DEFINE (ly_skyline_max_height, "ly:skyline-max-height", 1, 0, 0,
           (SCM skyline),
           R"(
Return the maximum height found in @var{skyline}.
           )")
{
  auto *const sky = LY_ASSERT_SMOB (Skyline, skyline, 1);
  return to_scm (sky->max_height ());
}

LY_DEFINE (ly_skyline_max_height_position, "ly:skyline-max-height-position", 1,
           0, 0, (SCM skyline),
           R"(
Return the position at which @var{skyline} reaches its maximum height.
           )")
{
  auto *const sky = LY_ASSERT_SMOB (Skyline, skyline, 1);
  return to_scm (sky->max_height_position ());
}

LY_DEFINE (ly_skyline_height, "ly:skyline-height", 2, 0, 0,
           (SCM skyline, SCM x),
           R"(
Return the height of @var{skyline} at point @var{x}.
           )")
{
  auto *const sky = LY_ASSERT_SMOB (Skyline, skyline, 1);
  LY_ASSERT_TYPE (scm_is_real, x, 2);
  Real x_cpp = from_scm<Real> (x);
  return to_scm (sky->height (x_cpp));
}

LY_DEFINE (ly_skyline_empty_p, "ly:skyline-empty?", 1, 0, 0, (SCM sky),
           R"(
Return whether skyline @var{sky} is empty.
           )")
{
  auto *const s = LY_ASSERT_SMOB (Skyline, sky, 1);
  return to_scm (s->is_empty ());
}

LY_DEFINE (ly_skylines_for_stencil, "ly:skylines-for-stencil", 2, 0, 0,
           (SCM stencil, SCM axis),
           R"(
Return a pair of skylines representing the outline of @var{stencil}.
@var{axis} is the @q{horizon axis} (i.e., this function gives skylines
suitable for the @code{vertical-@/skylines} property if @var{axis}
is@tie{}@code{X}, and for @code{horizontal-@/skylines} if @var{axis}
is@tie{}@code{Y}).
           )")
{
  LY_ASSERT_SMOB (const Stencil, stencil, 1);
  LY_ASSERT_TYPE (is_scm<Axis>, axis, 2);
  Axis a = from_scm<Axis> (axis);
  return to_scm (skylines_from_stencil (stencil, SCM_EOL, a));
}

LY_DEFINE (ly_skyline_pad, "ly:skyline-pad", 2, 0, 0,
           (SCM skyline, SCM horizon_padding),
           R"(
Return a version of @var{skyline} padded by @var{horizon-padding}
along the horizon.
           )")
{
  auto *const sky = LY_ASSERT_SMOB (Skyline, skyline, 1);
  LY_ASSERT_TYPE (scm_is_real, horizon_padding, 2);
  Real hp = from_scm<Real> (horizon_padding);
  return sky->padded (hp).smobbed_copy ();
}

LY_DEFINE (ly_make_skyline, "ly:make-skyline", 3, 0, 0,
           (SCM segments, SCM axis, SCM direction),
           R"(
Create a new skyline from a list of segments.  A skyline is an object
representing an outline along a @q{horizon axis}, much like a city skyline.
The argument @var{segments} is a list of segments.  A segment has the
form @code{'((x1 . y1) . (x2 . y2))}.  The resulting skyline, viewed on
the given @var{axis}, has a builing joining these two points for each segment.
@var{x1}, @var{y1}, @var{x2}, @var{y2} may be infinite.  The buildings
can be given in any order, and overlap.
           )")
{
  std::vector<Drul_array<Offset>> offs;
  LY_ASSERT_TYPE (ly_is_list, segments, 1);
  for (SCM segment : as_ly_scm_list (segments))
    {
      if (!(ly_is_list (segment)
            && from_scm<vsize> (scm_length (segment)) == 4))
        {
          scm_wrong_type_arg_msg ("ly:make-skyline", 0, segment,
                                  "list of 4 numbers");
        }
      for (SCM x : as_ly_scm_list (segment))
        {
          if (!is_scm<Real> (x))
            {
              scm_wrong_type_arg_msg ("ly:make-skyline", 0, x, "real number");
            }
        }
      Real x1 = from_scm<Real> (scm_car (segment));
      Real y1 = from_scm<Real> (scm_cadr (segment));
      Real x2 = from_scm<Real> (scm_caddr (segment));
      Real y2 = from_scm<Real> (scm_cadddr (segment));
      if ((std::isinf (x1) || std::isinf (x2)) && (y1 != y2))
        {
          scm_misc_error ("ly:make-skyline",
                          "building with infinite bound must be horizontal",
                          SCM_EOL);
        }
      offs.push_back (Drul_array<Offset> (Offset (x1, y1), Offset (x2, y2)));
    }
  LY_ASSERT_TYPE (is_scm<Axis>, axis, 2);
  Axis a = from_scm<Axis> (axis);
  LY_ASSERT_TYPE (is_scm<Direction>, direction, 3);
  Direction d = from_scm<Direction> (direction);
  return Skyline (offs, a, d).smobbed_copy ();
}

LY_DEFINE (ly_skyline_2_points, "ly:skyline->points", 2, 0, 0,
           (SCM skyline, SCM horizon_axis),
           R"(
Return a list of points from the given skyline, if viewed with
@var{horizon-@/axis} as @q{horizon axis}.  Joining the points with a
line draws the outline of the skyline.
           )")
{
  Skyline *sky = LY_ASSERT_SMOB (Skyline, skyline, 1);
  LY_ASSERT_TYPE (is_scm<Axis>, horizon_axis, 2);
  Axis ha = from_scm<Axis> (horizon_axis);
  const std::vector<Offset> &points = sky->to_points (ha);
  scm_remember_upto_here (skyline);
  return to_scm_list (points);
}

LY_DEFINE (ly_skyline_merge, "ly:skyline-merge", 2, 0, 0,
           (SCM skyline1, SCM skyline2),
           R"(
Merge the two given skylines.
           )")
{
  Skyline *sky1 = LY_ASSERT_SMOB (Skyline, skyline1, 1);
  Skyline *sky2 = LY_ASSERT_SMOB (Skyline, skyline2, 2);
  if (sky1->sky () != sky2->sky ())
    {
      scm_misc_error ("ly:skyline-merge",
                      "expecting skylines with the same direction", SCM_EOL);
    }
  Skyline merged (*sky1);
  merged.merge (*sky2);
  scm_remember_upto_here (skyline1);
  scm_remember_upto_here (skyline2);
  return merged.smobbed_copy ();
}
