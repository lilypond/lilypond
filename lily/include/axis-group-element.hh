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
			   public virtual Graphical_axis_group
{
  Link_array<Score_element> extra_elems_;

protected:
  virtual void do_print() const;
  virtual Link_array<Score_element> get_extra_dependencies() const;
  void purge_extra ();

  virtual void do_substitute_element_pointer (Score_element*,Score_element*);
  virtual Link_array<Score_element> elem_l_arr() const;
  
  virtual Interval do_height () const;
  virtual Interval do_width () const;


  Interval extra_extent (Axis a) const;

 
public:
  /**
    Override Graphical_axis_group::extent: make sure that
    Graphical_element::extent() is used as the public entry point.  */
  Interval extent (Axis a) const;

  /**
     add an element that only influences size, but does not have  X/Y parent
     relationship with THIS.
  */
  void add_extra_element (Score_element*);


  Axis_group_element ();
  virtual void set_axes (Axis,Axis);

  virtual Link_array<Score_element> get_children ();
};

#endif // AXIS_GROUP_ELEMENT_HH
