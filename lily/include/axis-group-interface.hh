/*   
  axis-group-interface.hh -- declare Axis_group_interface
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef AXIS_GROUP_INTERFACE_HH
#define AXIS_GROUP_INTERFACE_HH

#include "group-interface.hh"

/**
   Treat a group of elements as a union. This sets the parent of any S
   added to ELT_L_ to ELT_L_.

   Properties:

   axes -- list of axis (number) in which this group works

   transparent -- an Axis_group is transparent by default

   elements -- contains list of pointers to other elements

   interfaces -- Axis_group is added to this list.
*/
struct Axis_group_interface 
{
  Score_element *elt_l_;
  Axis_group_interface (Score_element*);

  static Interval group_extent_callback (Score_element const*,Axis);
  static Interval relative_group_extent (Axis, Score_element * common, SCM list);

  void add_element (Score_element*);
  void set_axes (Axis,Axis);
  bool axis_b (Axis)const;
  Link_array<Score_element> get_children ();
  bool has_interface_b ();
  void set_interface ();
  
};

#endif /* AXIS_GROUP_INTERFACE_HH */

