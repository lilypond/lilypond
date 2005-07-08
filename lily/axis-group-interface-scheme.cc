/*
  axis-group-interface-scheme.cc -- implement Axis_group_interface

  source file of the GNU LilyPond music typesetter

  (c) 2005 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "axis-group-interface.hh"
#include "lily-guile.hh"

LY_DEFINE(ly_relative_group_extent, "ly:relative-group-extent",
	  3, 0, 0, (SCM elements, SCM common, SCM axis),
	  "Determine the extent of @var{elements} relative to @var{common} in the "
	  "@var{axis} direction.")
{
  SCM_ASSERT_TYPE(scm_is_pair (elements), elements, SCM_ARG1, __FUNCTION__, "list");
  SCM_ASSERT_TYPE(unsmob_grob (common), common, SCM_ARG2, __FUNCTION__, "grob");
  SCM_ASSERT_TYPE(is_axis (axis), axis, SCM_ARG3, __FUNCTION__, "axis");


  Interval ext = Axis_group_interface::relative_group_extent (elements,
							      unsmob_grob (common),
							      (Axis) scm_to_int (axis));
  return ly_interval2scm (ext);
}
	  
	  
