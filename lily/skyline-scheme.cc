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

#include "lily-guile.hh"
#include "skyline.hh"

LY_DEFINE (ly_skyline_touching_point, "ly:skyline-touching-point",
           2, 1, 0, (SCM skyline, SCM other_skyline, SCM horizon_padding),
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

LY_DEFINE (ly_skyline_distance, "ly:skyline-distance",
           2, 1, 0, (SCM skyline, SCM other_skyline, SCM horizon_padding),
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

LY_DEFINE (ly_skyline_max_height, "ly:skyline-max-height",
           1, 0, 0, (SCM skyline),
           R"(
Return the maximum height found in @var{skyline}.
           )")
{
  auto *const sky = LY_ASSERT_SMOB (Skyline, skyline, 1);
  return to_scm (sky->max_height ());
}

LY_DEFINE (ly_skyline_max_height_position, "ly:skyline-max-height-position",
           1, 0, 0, (SCM skyline),
           R"(
Return the position at which @var{skyline} reaches its maximum height.
           )")
{
  auto *const sky = LY_ASSERT_SMOB (Skyline, skyline, 1);
  return to_scm (sky->max_height_position ());
}

LY_DEFINE (ly_skyline_height, "ly:skyline-height",
           2, 0, 0, (SCM skyline, SCM x),
           R"(
Return the height of @var{skyline} at point @var{x}.
           )")
{
  auto *const sky = LY_ASSERT_SMOB (Skyline, skyline, 1);
  LY_ASSERT_TYPE (scm_is_real, x, 2);
  Real x_cpp = from_scm<Real> (x);
  return to_scm (sky->height (x_cpp));
}

LY_DEFINE (ly_skyline_empty_p, "ly:skyline-empty?",
           1, 0, 0, (SCM sky),
           R"(
Return whether skyline @var{sky} is empty.
           )")
{
  auto *const s = LY_ASSERT_SMOB (Skyline, sky, 1);
  return to_scm (s->is_empty ());
}
