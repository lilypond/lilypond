/*
  align-interface.hh -- declare Align_interface

  source file of the GNU LilyPond music typesetter

  (c) 2000--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef ALIGN_INTERFACE_HH
#define ALIGN_INTERFACE_HH

#include "lily-proto.hh"
#include "std-vector.hh"
#include "grob-interface.hh"

struct Align_interface
{
  DECLARE_SCHEME_CALLBACK (align_to_minimum_distances, (SCM));
  DECLARE_SCHEME_CALLBACK (align_to_ideal_distances, (SCM));
  static void align_elements_to_minimum_distances(Grob *, Axis a);
  static void align_elements_to_ideal_distances(Grob *);
  static vector<Real> get_minimum_translations (Grob *, vector<Grob*> const&,
						Axis a,
						bool safe, int start, int end);
  static void set_ordered (Grob *);
  static Axis axis (Grob *);
  static void add_element (Grob *, Grob *);
  static int get_count (Grob *, Grob *);

  DECLARE_GROB_INTERFACE();

  static Real get_pure_child_y_translation (Grob *, Grob *child, int start, int end);
};

#endif /* ALIGN_INTERFACE_HH */

