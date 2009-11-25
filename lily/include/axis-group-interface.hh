/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef AXIS_GROUP_INTERFACE_HH
#define AXIS_GROUP_INTERFACE_HH

#include "std-vector.hh"
#include "lily-proto.hh"
#include "grob-interface.hh"
#include "skyline.hh"

struct Axis_group_interface
{
  static SCM generic_group_extent (Grob *me, Axis a);
  static Interval pure_group_height (Grob *me, int start, int end);
  DECLARE_SCHEME_CALLBACK (width, (SCM smob));
  DECLARE_SCHEME_CALLBACK (calc_x_common, (SCM smob));
  DECLARE_SCHEME_CALLBACK (calc_y_common, (SCM smob));
  DECLARE_SCHEME_CALLBACK (height, (SCM smob));
  DECLARE_SCHEME_CALLBACK (pure_height, (SCM smob, SCM start, SCM end));
  DECLARE_SCHEME_CALLBACK (calc_skylines, (SCM smob));
  DECLARE_SCHEME_CALLBACK (combine_skylines, (SCM smob));
  DECLARE_SCHEME_CALLBACK (print, (SCM smob));
  DECLARE_SCHEME_CALLBACK (adjacent_pure_heights, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_next_staff_spacing, (SCM));
  static Interval relative_group_extent (vector<Grob*> const &list,
					 Grob *common, Axis);
  static Interval relative_pure_height (Grob *me, int start, int end);
  static Interval combine_pure_heights (Grob *me, SCM, int, int);
  static Interval cached_pure_height (Grob *me, int, int);
  static Interval begin_of_line_pure_height (Grob *me, int);
  static Interval rest_of_line_pure_height (Grob *me, int, int);

  static Grob *calc_pure_elts_and_common (Grob*);
  static Skyline_pair skyline_spacing (Grob *me, vector<Grob*> elements);
  static void add_element (Grob *me, Grob *);
  static void set_axes (Grob *, Axis, Axis);
  static bool has_axis (Grob *, Axis);
  static void get_children (Grob *, vector<Grob*> *);
  static Interval staff_extent (Grob *me, Grob *ref, Axis, Grob *staff, Axis);
  static SCM calc_common (Grob *, Axis);
  static Real minimum_distance (Grob*, Grob*, Axis);
  DECLARE_GROB_INTERFACE();
};

#endif /* AXIS_GROUP_INTERFACE_HH */

