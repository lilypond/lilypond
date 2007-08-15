/*
  align-interface.hh -- declare Align_interface

  source file of the GNU LilyPond music typesetter

  (c) 2000--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef ALIGN_INTERFACE_HH
#define ALIGN_INTERFACE_HH

#include "lily-proto.hh"
#include "lily-guile.hh"
#include "std-vector.hh"

struct Align_interface
{
  DECLARE_SCHEME_CALLBACK (calc_positioning_done, (SCM));
  DECLARE_SCHEME_CALLBACK (stretch_after_break, (SCM element));
  static void align_to_fixed_distance (Grob *, Axis a);
  static void align_elements_to_extents (Grob *, Axis a);
  static vector<Real> get_extents_aligned_translates (Grob *, vector<Grob*> const&,
						      Axis a,
						      bool safe, int start, int end);
  static void set_ordered (Grob *);
  static Axis axis (Grob *);
  static void add_element (Grob *, Grob *);
  static int get_count (Grob *, Grob *);

  static bool has_interface (Grob *);

  static Real get_pure_child_y_translation (Grob *, Grob *child, int start, int end);
};

Grob *find_fixed_alignment_parent (Grob *g);

#endif /* ALIGN_INTERFACE_HH */

