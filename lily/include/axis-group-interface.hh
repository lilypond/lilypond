/*
  axis-group-interface.hh -- declare Axis_group_interface

  source file of the GNU LilyPond music typesetter

  (c) 2000--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef AXIS_GROUP_INTERFACE_HH
#define AXIS_GROUP_INTERFACE_HH

#include "lily-proto.hh"
#include "lily-guile.hh"

/**
 */
struct Axis_group_interface
{
  static SCM generic_group_extent (Grob *me, Axis a);
  DECLARE_SCHEME_CALLBACK (width, (SCM smob));
  DECLARE_SCHEME_CALLBACK (height, (SCM smob));
  static Interval relative_group_extent (Link_array__Grob_ const &list,
					 Grob *common, Axis);

  static void add_element (Grob *me, Grob *);
  static void set_axes (Grob *, Axis, Axis);
  static bool has_axis (Grob *, Axis);
  static void get_children (Grob *, Link_array__Grob_ *);
  static bool has_interface (Grob *);
};

#endif /* AXIS_GROUP_INTERFACE_HH */

