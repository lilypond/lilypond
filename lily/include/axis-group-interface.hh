/*   
  axis-group-interface.hh -- declare Axis_group_interface
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef AXIS_GROUP_INTERFACE_HH
#define AXIS_GROUP_INTERFACE_HH

#include "group-interface.hh"

struct Axis_group_interface : Group_interface
{
  Axis_group_interface (Score_element*);
  static Interval group_extent_callback (Dimension_cache const*);
  void add_element (Score_element*);
  void set_axes (Axis,Axis);
  bool axis_b (Axis)const;
  Link_array<Score_element> get_children ();
  bool has_interface_b ();
  void set_interface ();
};

Axis_group_interface
axis_group (Score_element*);

#endif /* AXIS_GROUP_INTERFACE_HH */

