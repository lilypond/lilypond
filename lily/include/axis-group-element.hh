/*
  axis-group-element.hh -- declare Axis_group_element

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef AXIS_GROUP_ELEMENT_HH
#define AXIS_GROUP_ELEMENT_HH

#include "score-element.hh"


/** 
  Treat a group of elements a unity in either or both axis sense .
  This is a wrapper around Axis_group_administration
  */
class Axis_group_element : public virtual Score_element
{
  /// modify fields of E for removal.
  void do_remove (Score_element*e);
  void purge_extra ();



protected:
  virtual Link_array<Score_element> get_extra_dependencies() const;
  virtual Link_array<Score_element> elem_l_arr() const;
  static Interval extent_callback (Dimension_cache const*);
  


  Interval extra_extent (Axis a) const;
public:
  // keep array in order.
  bool ordered_b_;		
  Axis axes_[2];
    
  Interval my_extent (Axis) const;



  bool contains_b (Score_element const *) const;
  void add_element (Score_element*);

  /**
     add an element that only influences size, but does not have  X/Y parent
     relationship with THIS.
  */
  void add_extra_element (Score_element*);


  Axis_group_element ();
  void set_axes (Axis,Axis);

  Link_array<Score_element> get_children ();
};

#endif // AXIS_GROUP_ELEMENT_HH
