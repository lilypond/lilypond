/*
  axis-group-element.hh -- declare Axis_group_element

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef AXIS_GROUP_ELEMENT_HH
#define AXIS_GROUP_ELEMENT_HH

#include "score-element.hh"


/** 
  Treat a group of elements a unity in either or both axis sense .
  This is a wrapper around Axis_group_administration

  FIXME: remove this class, and make interface of it, so we can remove multiple inheritance.
*/
class Axis_group_element : public virtual Score_element
{
protected:
  virtual Link_array<Score_element> elem_l_arr() const;
  static Interval extent_callback (Dimension_cache const*);

public:
  void add_element (Score_element*);
  bool axis_b (Axis) const;
  Axis_group_element ();
  void set_axes (Axis,Axis);

  Link_array<Score_element> get_children ();
};

#endif // AXIS_GROUP_ELEMENT_HH
