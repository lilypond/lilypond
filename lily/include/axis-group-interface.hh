/*   
  axis-group-interface.hh -- declare Axis_group_interface
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef AXIS_GROUP_INTERFACE_HH
#define AXIS_GROUP_INTERFACE_HH

#include "group-interface.hh"

/**

*/
struct Axis_group_interface 
{
  DECLARE_SCHEME_CALLBACK (group_extent_callback, (SCM smob, SCM axis));
  static Interval relative_group_extent (Axis, Grob * common, SCM list);

  static void add_element (Grob* me, Grob*);
  static void set_axes (Grob*,Axis,Axis);
  static bool axis_b (Grob*,Axis);
  static Link_array<Grob> get_children (Grob*);
  static bool has_interface (Grob*);
  
  
};

#endif /* AXIS_GROUP_INTERFACE_HH */

