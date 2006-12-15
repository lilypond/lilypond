/*
  axis-group-interface.hh -- declare Axis_group_interface

  source file of the GNU LilyPond music typesetter

  (c) 2000--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
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
  static SCM pure_group_height (Grob *me, int start, int end);
  DECLARE_SCHEME_CALLBACK (width, (SCM smob));
  DECLARE_SCHEME_CALLBACK (height, (SCM smob));
  DECLARE_SCHEME_CALLBACK (pure_height, (SCM smob, SCM start, SCM end));
  DECLARE_SCHEME_CALLBACK (calc_skylines, (SCM smob));
  DECLARE_SCHEME_CALLBACK (combine_skylines, (SCM smob));
  static Interval relative_group_extent (vector<Grob*> const &list,
					 Grob *common, Axis);
  static Interval relative_pure_height (Grob *me, vector<Grob*> const &list,
					Grob *common, int start, int end,
					bool use_cache);
  static Interval cached_pure_height (Grob *me, vector<Grob*> const &list,
				      Grob *common, int, int);

  static Skyline_pair skyline_spacing (Grob *me, vector<Grob*> elements);
  static void add_element (Grob *me, Grob *);
  static void set_axes (Grob *, Axis, Axis);
  static bool has_axis (Grob *, Axis);
  static void get_children (Grob *, vector<Grob*> *);
  DECLARE_GROB_INTERFACE();
};

#endif /* AXIS_GROUP_INTERFACE_HH */

