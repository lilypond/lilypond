/*   
  side-position-interface.hh -- declare Side_position_interface
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SIDE_POSITION_INTERFACE_HH
#define SIDE_POSITION_INTERFACE_HH

#include "spanner.hh"
#include "item.hh"

/*
  TODO: move  out unrelated callbacks.

  TODO: reduce number of methods.
*/
struct Side_position
{
public:
  DECLARE_SCHEME_CALLBACK(side_position, (SCM  element, SCM axis));
  DECLARE_SCHEME_CALLBACK(aligned_on_self, (SCM  element, SCM axis));
  DECLARE_SCHEME_CALLBACK(aligned_side, (SCM element, SCM axis));  
  DECLARE_SCHEME_CALLBACK(quantised_position, (SCM element, SCM axis));
  DECLARE_SCHEME_CALLBACK(centered_on_parent, (SCM element, SCM axis));
  static void set_axis (Score_element*,Axis);
  static void set_minimum_space (Score_element*,Real);
  static void set_padding (Score_element*,Real);
  static Axis get_axis (Score_element*) ;
  static bool supported_b (Score_element*) ;
  static bool has_interface (Score_element*) ;
  static void add_support (Score_element*,Score_element*);
  static void add_staff_support (Score_element*);
  static Direction get_direction (Score_element*);
  static void set_direction (Score_element*,Direction);
};


#endif /* SIDE_POSITION_INTERFACE_HH */

