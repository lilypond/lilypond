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
  
  Which side to align?  -1: left side, 0: centered (around
     center_l_ if not nil, or around center of width), 1: right side
 */
struct Align_interface  {
  Score_element * elt_l_;
  
  Align_interface (Score_element const*);
  static Real alignment_callback (Dimension_cache const *);
  void do_side_processing (Axis a);
  void set_axis (Axis);
  Axis axis () const;
  void add_element (Score_element*);
  int get_count (Score_element*)const;
  void set_interface ();
  bool has_interface_b ();
  static Real center_on_element (Dimension_cache const *c);
};

#endif /* ALIGN_INTERFACE_HH */

