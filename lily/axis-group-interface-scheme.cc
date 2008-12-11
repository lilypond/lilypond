/*
  axis-group-interface-scheme.cc -- implement Axis_group_interface bindings

  source file of the GNU LilyPond music typesetter

  (c) 2005--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "axis-group-interface.hh"
#include "lily-guile.hh"
#include "grob.hh"
#include "grob-array.hh"

LY_DEFINE (ly_relative_group_extent, "ly:relative-group-extent",
	   3, 0, 0, (SCM elements, SCM common, SCM axis),
	   "Determine the extent of @var{elements} relative to @var{common} in the"
	   " @var{axis} direction.")
{
  Grob_array *ga = unsmob_grob_array (elements);

  SCM_ASSERT_TYPE (ga || scm_is_pair (elements), elements, SCM_ARG1, __FUNCTION__, "list or Grob_array");
  LY_ASSERT_SMOB (Grob, common, 2);
  LY_ASSERT_TYPE (is_axis, axis, 3);

  vector<Grob*> elts;
  if (!ga)
    {
      for (SCM s = elements; scm_is_pair (s); s = scm_cdr (s))
	elts.push_back (unsmob_grob (scm_car (s)));
    }

  Interval ext = Axis_group_interface::relative_group_extent (ga ? ga->array () : elts,
							      unsmob_grob (common),
							      (Axis) scm_to_int (axis));
  return ly_interval2scm (ext);
}

