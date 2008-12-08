/*
  align-interface.hh -- declare Align_interface

  source file of the GNU LilyPond music typesetter

  (c) 2000--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef ALIGN_INTERFACE_HH
#define ALIGN_INTERFACE_HH

#include "lily-proto.hh"
#include "std-vector.hh"
#include "grob-interface.hh"

struct Align_interface
{
  DECLARE_SCHEME_CALLBACK (calc_positioning_done, (SCM));
  DECLARE_SCHEME_CALLBACK (stretch_after_break, (SCM element));
  DECLARE_SCHEME_CALLBACK (calc_max_stretch, (SCM));
  static void stretch (Grob *, Real amount, Axis a);
  static void align_elements_to_extents (Grob *, Axis a);
  static vector<Real> get_extents_aligned_translates (Grob *, vector<Grob*> const&,
						      Axis a,
						      bool safe, int start, int end);
  static int stretchable_children_count (Grob const*);
  static void set_ordered (Grob *);
  static Axis axis (Grob *);
  static void add_element (Grob *, Grob *);
  static int get_count (Grob *, Grob *);

  DECLARE_GROB_INTERFACE();

  static Real get_pure_child_y_translation (Grob *, Grob *child, int start, int end);
};

#endif /* ALIGN_INTERFACE_HH */

