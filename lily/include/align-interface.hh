/*
  align-interface.hh -- declare Align_interface
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef ALIGN_INTERFACE_HH
#define ALIGN_INTERFACE_HH

#include "axes.hh"
#include "lily-proto.hh"

/*
  TODO: rewrite this comment.
  
  
  Order elements top to bottom/left to right/right to left etc..

  TODO: implement padding.

  document usage of this.



  *******
  
  element properties

  stacking-dir
 
  Which side to align? -1: left side, 0: centered (around
  center_l_ if not nil, or around center of width), 1: right side
*/
struct Align_interface {
  static Real alignment_callback (Score_element *,Axis);
  static void do_side_processing (Score_element*,Axis a);
  static void set_axis (Score_element*,Axis);
  static Axis axis (Score_element*) ;
  static void add_element (Score_element*,Score_element*);
  static int get_count (Score_element*,Score_element*);
  static void set_interface (Score_element*);
  static bool has_interface (Score_element*);
  static Real center_on_element (Score_element *c, Axis);
};

#endif /* ALIGN_INTERFACE_HH */

