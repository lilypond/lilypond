/*
  axis-group-element.hh -- declare Axis_group_element

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef AXIS_GROUP_ELEMENT_HH
#define AXIS_GROUP_ELEMENT_HH

#include "score-elem.hh"
#include "axis-group-administration.hh"
/** 
  Treat a group of elements a unity in either or both axis sense .
  This is a wrapper around Axis_group_administration
  */
class Axis_group_element : public virtual Score_elem {
protected:
  Axis_group_administration axis_admin_;
  virtual void do_print() const;
  virtual Link_array<Score_elem> get_extra_dependencies() const;
  virtual void do_unlink();
  virtual void do_junk_links();

public:
  virtual Link_array<Score_elem> elem_l_arr() const;
  Axis_group_element();
  virtual void remove_all()=0;
  virtual void add_element (Graphical_element*)=0;
  virtual void remove_element (Graphical_element*)=0;
  virtual bool contains_b (Graphical_element const *) const;
  DECLARE_MY_RUNTIME_TYPEINFO;
};

#endif // AXIS_GROUP_ELEMENT_HH
