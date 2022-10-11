/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "axis-group-interface.hh"
#include "lily-guile.hh"
#include "grob.hh"
#include "grob-array.hh"

using std::vector;

LY_DEFINE (ly_relative_group_extent, "ly:relative-group-extent", 3, 0, 0,
           (SCM elements, SCM common, SCM axis),
           R"(
Determine the extent of @var{elements} relative to @var{common} in the
@var{axis} direction.
           )")
{
  Grob_array *ga = unsmob<Grob_array> (elements);

  SCM_ASSERT_TYPE (ga || ly_is_list (elements), elements, SCM_ARG1,
                   __FUNCTION__, "list or Grob_array");
  auto *const com = LY_ASSERT_SMOB (Grob, common, 2);
  LY_ASSERT_TYPE (is_scm<Axis>, axis, 3);
  Axis a = from_scm<Axis> (axis);
  if (!ga)
    ga = unsmob<Grob_array> (grob_list_to_grob_array (elements));
  Interval ext
    = Axis_group_interface::relative_group_extent (ga->array (), com, a);
  return to_scm (ext);
}

LY_DEFINE (ly_generic_bound_extent, "ly:generic-bound-extent", 2, 0, 0,
           (SCM grob, SCM common),
           R"(
Determine the extent of @var{grob} relative to @var{common} along the
x@tie{}axis, finding its extent as a bound when it a has
@code{bound-alignment-interfaces} property list set and otherwise the full
extent.
           )")
{
  auto *const g = LY_ASSERT_SMOB (Grob, grob, 1);
  auto *const com = LY_ASSERT_SMOB (Grob, common, 2);

  Interval ext = Axis_group_interface::generic_bound_extent (g, com, X_AXIS);
  return to_scm (ext);
}

LY_DEFINE (ly_axis_group_interface__add_element,
           "ly:axis-group-interface::add-element", 2, 0, 0,
           (SCM grob, SCM grob_element),
           R"(
Add @var{grob-element} to the axis group @var{grob}.  In particular, @var{grob}
becomes parent to @var{grob-element} on all axes supported by @var{grob},
unless the parents are already set.
           )")
{
  auto *const g = LY_ASSERT_SMOB (Grob, grob, 1);
  auto *const elem = LY_ASSERT_SMOB (Grob, grob_element, 2);
  Axis_group_interface::add_element (g, elem);
  return SCM_UNSPECIFIED;
}
