/*
  align-interface.hh -- declare Align_interface
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef ALIGN_INTERFACE_HH
#define ALIGN_INTERFACE_HH

#include "axes.hh"
#include "lily-proto.hh"
#include "lily-guile.hh"

struct Align_interface {
  DECLARE_SCHEME_CALLBACK (alignment_callback, (SCM element, SCM axis));
  DECLARE_SCHEME_CALLBACK (fixed_distance_alignment_callback, (SCM element, SCM axis));
  static void align_to_fixed_distance (Grob*,Axis a);
  static void align_elements_to_extents (Grob*,Axis a);
  static void set_axis (Grob*,Axis);
  static Axis axis (Grob*) ;
  static void add_element (Grob*,Grob*, SCM callback);
  static int get_count (Grob*,Grob*);

  static bool has_interface (Grob*);
};

#endif /* ALIGN_INTERFACE_HH */

