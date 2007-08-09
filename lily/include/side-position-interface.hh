/*
  side-position-interface.hh -- declare Side_position_interface

  source file of the GNU LilyPond music typesetter

  (c) 1999--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef SIDE_POSITION_INTERFACE_HH
#define SIDE_POSITION_INTERFACE_HH

#include "spanner.hh"
#include "item.hh"

/*
  TODO: move out unrelated callbacks.

  TODO: reduce number of methods.
*/
struct Side_position_interface
{
public:

  DECLARE_SCHEME_CALLBACK (y_aligned_on_support_refpoints, (SCM element));
  DECLARE_SCHEME_CALLBACK (pure_y_aligned_on_support_refpoints, (SCM element, SCM start, SCM end));
  DECLARE_SCHEME_CALLBACK (x_aligned_side, (SCM element, SCM current));
  DECLARE_SCHEME_CALLBACK (y_aligned_side, (SCM element, SCM current));
  DECLARE_SCHEME_CALLBACK (pure_y_aligned_side, (SCM element, SCM start, SCM end, SCM current));

  static SCM aligned_side (Grob*me, Axis a, bool pure, int start, int end, Real *current_off_ptr);

  static SCM general_side_position (Grob *, Axis, bool, bool my_extents,
				    bool pure, int start, int end, Real *current_off);
  static Axis get_axis (Grob *);
  static void set_axis (Grob *, Axis);
  static bool has_interface (Grob *);
  static void add_support (Grob *, Grob *);
  static void add_staff_support (Grob *);
  static Direction get_direction (Grob *);
};

#endif /* SIDE_POSITION_INTERFACE_HH */

