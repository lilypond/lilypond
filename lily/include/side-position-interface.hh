/*   
  side-position-interface.hh -- declare Side_position_interface
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
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
  static void set_axis (Grob*,Axis);
  static void set_minimum_space (Grob*,Real);
  static void set_padding (Grob*,Real);
  static Axis get_axis (Grob*) ;
  static bool supported_b (Grob*) ;
  static bool has_interface (Grob*) ;
  static void add_support (Grob*,Grob*);
  static void add_staff_support (Grob*);
  static Direction get_direction (Grob*);
  static void set_direction (Grob*,Direction);
};


#endif /* SIDE_POSITION_INTERFACE_HH */

