/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "grob-array.hh"
#include "grob.hh"
#include "lily-guile.hh"

using std::vector;

LY_DEFINE (
    ly_relative_group_extent, "ly:relative-group-extent", 3, 0, 0,
    (SCM elements, SCM common, SCM axis),
    "Determine the extent of @var{elements} relative to @var{common} in the"
    " @var{axis} direction.")
{
  Grob_array *ga = unsmob<Grob_array> (elements);

  SCM_ASSERT_TYPE (ga || scm_is_pair (elements), elements, SCM_ARG1,
                   __FUNCTION__, "list or Grob_array");
  LY_ASSERT_SMOB (Grob, common, 2);
  LY_ASSERT_TYPE (is_axis, axis, 3);

  vector<Grob *> elts;
  if (!ga)
    {
      for (SCM s = elements; scm_is_pair (s); s = scm_cdr (s))
        elts.push_back (unsmob<Grob> (scm_car (s)));
    }

  Interval ext = Axis_group_interface::relative_group_extent (
      ga ? ga->array () : elts, unsmob<Grob> (common), (Axis)scm_to_int (axis));
  return ly_interval2scm (ext);
}

LY_DEFINE (ly_generic_bound_extent, "ly:generic-bound-extent", 2, 0, 0,
           (SCM grob, SCM common),
           "Determine the extent of @var{grob} relative to @var{common} along"
           " the X axis, finding its extent as a bound when it a has"
           " @code{bound-alignment-interfaces} property list set and"
           " otherwise the full extent.")
{
  LY_ASSERT_SMOB (Grob, grob, 1);
  LY_ASSERT_SMOB (Grob, common, 2);

  Interval ext = Axis_group_interface::generic_bound_extent (
      unsmob<Grob> (grob), unsmob<Grob> (common), X_AXIS);
  return ly_interval2scm (ext);
}

LY_DEFINE (ly_axis_group_interface__add_element,
           "ly:axis-group-interface::add-element", 2, 0, 0,
           (SCM grob, SCM grob_element),
           "Set @var{grob} the parent of @var{grob-element} on all axes of"
           " @var{grob}.")
{
  LY_ASSERT_SMOB (Grob, grob, 1);
  LY_ASSERT_SMOB (Grob, grob_element, 2);
  Axis_group_interface::add_element (unsmob<Grob> (grob),
                                     unsmob<Grob> (grob_element));
  return SCM_UNSPECIFIED;
}
