/*
  side-position-interface.hh -- declare Side_position_interface

  source file of the GNU LilyPond music typesetter

  (c) 1999--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef SIDE_POSITION_INTERFACE_HH
#define SIDE_POSITION_INTERFACE_HH

#include "grob-interface.hh"
#include "lily-proto.hh"

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
  DECLARE_SCHEME_CALLBACK (calc_cross_staff, (SCM element));

  static SCM aligned_side (Grob*me, Axis a, bool pure, int start, int end, Real *current_off_ptr);

  static SCM general_side_position (Grob *, Axis, bool, bool my_extents,
				    bool pure, int start, int end, Real *current_off);
  static Axis get_axis (Grob *);
  static void set_axis (Grob *, Axis);
  DECLARE_GROB_INTERFACE();
  static void add_support (Grob *, Grob *);
  static void add_staff_support (Grob *);
  static Direction get_direction (Grob *);
};

#endif /* SIDE_POSITION_INTERFACE_HH */

