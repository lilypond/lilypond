/*
  axis-group-element.hh -- declare Axis_group_element

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef AXIS_GROUP_ELEMENT_HH
#define AXIS_GROUP_ELEMENT_HH

#include "score-element.hh"
#include "graphical-axis-group.hh"

/** 
  Treat a group of elements a unity in either or both axis sense .
  This is a wrapper around Axis_group_administration
  */
class Axis_group_element : public virtual Score_element,
			   public Graphical_axis_group {
protected:
  virtual void do_print() const;
  virtual Link_array<Score_element> get_extra_dependencies() const;

public:
  virtual Link_array<Score_element> elem_l_arr() const;
  Axis_group_element(Axis,Axis);
  Axis_group_element();  
  virtual Link_array<Score_element> get_children ();
};

#endif // AXIS_GROUP_ELEMENT_HH
