/*
  side-position-interface.hh -- declare Side_position_interface

  source file of the GNU LilyPond music typesetter

  (c) 1999--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef SELF_ALIGNMENT_INTERFACE_HH
#define SELF_ALIGNMENT_INTERFACE_HH

#include "spanner.hh"

struct Self_alignment_interface
{
  static bool has_interface (Grob *);

  static SCM aligned_on_self (Grob *me, Axis a);
  static SCM centered_on_object (Grob *me, Axis a);
  static SCM aligned_on_parent (Grob *me, Axis a);
  static void set_center_parent (Grob *me, Axis a);
  static void set_align_self (Grob *me, Axis a);
  
  DECLARE_SCHEME_CALLBACK (x_aligned_on_self, (SCM element));
  DECLARE_SCHEME_CALLBACK (y_aligned_on_self, (SCM element));

  DECLARE_SCHEME_CALLBACK (centered_on_x_parent, (SCM element));
  DECLARE_SCHEME_CALLBACK (centered_on_y_parent, (SCM element));
  DECLARE_SCHEME_CALLBACK (x_centered_on_y_parent, (SCM element));

  DECLARE_SCHEME_CALLBACK (aligned_on_x_parent, (SCM element));
  DECLARE_SCHEME_CALLBACK (aligned_on_y_parent, (SCM element));
};
#endif
