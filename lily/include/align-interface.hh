/*
  align-interface.hh -- declare Align_interface
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef ALIGN_INTERFACE_HH
#define ALIGN_INTERFACE_HH

#include "axes.hh"
#include "lily-proto.hh"
#include "lily-guile.hh"

struct Align_interface {
  DECLARE_SCHEME_CALLBACK(alignment_callback, (SCM element, SCM axis));
  static void do_side_processing (Score_element*,Axis a);
  static void set_axis (Score_element*,Axis);
  static Axis axis (Score_element*) ;
  static void add_element (Score_element*,Score_element*);
  static int get_count (Score_element*,Score_element*);
  static void set_interface (Score_element*);
  static bool has_interface (Score_element*);
  DECLARE_SCHEME_CALLBACK(center_on_element, (SCM element, SCM axis));
};

#endif /* ALIGN_INTERFACE_HH */

