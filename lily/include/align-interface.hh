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


/*
  Order elements top to bottom/left to right/right to left etc.


  *******
  
  element properties

  stacking-dir -- stack contents of elements in which direction ?

  align-dir -- Which side to align? -1: left side, 0: centered (around
    center_l_ if not nil, or around center of width), 1: right side

  threshold -- (cons MIN MAX), where MIN and MAX are dimensions in
    staffspace

  alignment-done -- boolean to administrate whether we've done the alignment already (to ensure that the process is done only once)

  center-element -- element which will be at the center of the group
    after aligning (when using
    Align_interface::center_on_element). The center element should
    have this object as a reference point.

  elements -- to be aligned elements 

  axes -- list of axis numbers. Should contain only one number.
  
  *******
  
  Reads the following from its elements
  
  
  minimum-space --  (cons LEFT RIGHT)

  extra-space -- (cons LEFT RIGHT)
  
*/
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

