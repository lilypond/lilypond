/*
  axis-group-administration.hh -- declare Axis_group_administration

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef AXIS_GROUP_ADMINISTRATION_HH
#define AXIS_GROUP_ADMINISTRATION_HH


#include "parray.hh"
#include "axes.hh"
#include "real.hh"
#include "lily-proto.hh"

/**
  Do the dirty work for Axis_group_element.
 */
struct Axis_group_administration {
  Link_array<Graphical_element> elem_l_arr_;
    
  Interval extent (Axis) const;
  void print() const ;
  Axis_group_administration (Axis_group_administration const&);
  Axis_group_administration(){}
  void remove_all (Axis a1,   Axis a2);

  bool contains_b (Graphical_element const *) const;
  void add_element (Graphical_element*, Axis_group_element*, Axis a1, Axis a2);
  void remove_element (Graphical_element*, Axis a1, Axis a2);
};

#endif // AXIS_GROUP_ADMINISTRATION_HH
