/*
  axis-group-administration.hh -- declare Graphical_axis_group

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef GRAPHICAL_AXIS_GROUP_HH
#define GRAPHICAL_AXIS_GROUP_HH


#include "parray.hh"
#include "axes.hh"
#include "real.hh"
#include "lily-proto.hh"
#include "graphical-element.hh"

/**
  Treat a group of graphical elements as an aggegrate.
 */
class Graphical_axis_group : public virtual Graphical_element {
public:
  // keep array in order.
  bool ordered_b_;		
  Link_array<Graphical_element> elem_l_arr_;
  Axis axes_[2];
    
  Interval extent (Axis) const;
  virtual void do_print() const;
  Graphical_axis_group(Graphical_axis_group const&s);
  Graphical_axis_group ();
  virtual void set_axes (Axis,Axis);
  void remove_all ();

  bool contains_b (Graphical_element const *) const;
  void add_element (Graphical_element*);
  void remove_element (Graphical_element*);
};

#endif // Graphical_axis_group_HH
