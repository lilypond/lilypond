/*
  axis-group.cc -- implement Graphical_axis_group

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "interval.hh"
#include "graphical-axis-group.hh"
#include "axis-group-element.hh"
#include "graphical-element.hh"
#include "debug.hh"

/** don't copy anything: an element can only be in one
  Graphical_axis_group at one time.  */
Graphical_axis_group::Graphical_axis_group (Graphical_axis_group const&s)
{
  axis1_ = s.axis1_;
  axis2_ = s.axis2_;
}

bool 
Graphical_axis_group::contains_b (Graphical_element const *e) const
{
  return elem_l_arr_.find_l (e);
}

Interval
Graphical_axis_group::extent (Axis axis) const
{
  Interval r;
  for (int i=0; i < elem_l_arr_.size(); i++) 
    r.unite (elem_l_arr_[i]->extent (axis));
  return r;
}

void
Graphical_axis_group::add_element (Graphical_element*e)
{
  Graphical_axis_group *& g1 = e->axis_group_l_a_[axis1_];
  Graphical_axis_group *& g2 = e->axis_group_l_a_[axis2_];
  
  assert (!g1 || g1 == this);
  assert (!g2 || g2 == this);
  g1 = this;
  g2 = this;
  elem_l_arr_.push (e);
}



void
Graphical_axis_group::remove_element (Graphical_element*e)
{
  assert (contains_b (e));
  elem_l_arr_.unordered_substitute (e,0);
  
  e->axis_group_l_a_[axis1_] = 0;
  e->axis_group_l_a_[axis2_] = 0;    
}

void
Graphical_axis_group::remove_all ()
{
  for (int i=0; i < elem_l_arr_.size(); i++) 
    {
      Graphical_element*e=elem_l_arr_[i];
      e->axis_group_l_a_[axis1_] = 0;
      e->axis_group_l_a_[axis2_] = 0;  
    }
  elem_l_arr_.clear ();
}


void    
Graphical_axis_group::do_print() const
{
#ifndef NPRINT
  for (int i=0; i < elem_l_arr_.size(); i++) 
    DOUT << classname(elem_l_arr_[i]) << " ";
#endif
}

Graphical_axis_group::Graphical_axis_group (Axis a1, Axis a2)
{
  axis1_ =a1;
  axis2_ = a2;
}


