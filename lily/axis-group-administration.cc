/*
  axis-group.cc -- implement Axis_group_administration

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "interval.hh"
#include "axis-group-administration.hh"
#include "axis-group-element.hh"
#include "graphical-element.hh"
#include "debug.hh"

/** don't copy anything: an element can only be in one
  Axis_group_element at one time.  */
Axis_group_administration::Axis_group_administration (Axis_group_administration const&)
{
}

bool 
Axis_group_administration::contains_b (Graphical_element const *e) const
{
  return elem_l_arr_.find_l (e);
}

Interval
Axis_group_administration::extent (Axis axis) const
{
  Interval r;
  for (int i=0; i < elem_l_arr_.size(); i++) 
    r.unite (elem_l_arr_[i]->extent (axis));
  return r;
}

void
Axis_group_administration::add_element (Graphical_element*e,
					Axis_group_element*g, Axis a1, Axis a2)
{
  // don't add self to self.
  assert (e != g);
  Axis_group_element *& g1 = e->axis_group_l_a_[a1];
  Axis_group_element *& g2 = e->axis_group_l_a_[a2];
  
  assert (!g1 || g1 == g);
  assert (!g2 || g2 == g);
  g1 = g;
  g2 = g;
  elem_l_arr_.push (e);
}



void
Axis_group_administration::remove_element (Graphical_element*e, Axis a1, Axis a2)
{
  assert (contains_b (e));
  elem_l_arr_.unordered_substitute (e,0);
  
  e->axis_group_l_a_[a1] = 0;
  e->axis_group_l_a_[a2] = 0;    
}

void
Axis_group_administration::remove_all (Axis a1, Axis a2)
{
  for (int i=0; i < elem_l_arr_.size(); i++) 
    {
      Graphical_element*e=elem_l_arr_[i];
      e->axis_group_l_a_[a1] = 0;
      e->axis_group_l_a_[a2] = 0;  
    }
  elem_l_arr_.clear ();
}


void    
Axis_group_administration::print() const
{
#ifndef NPRINT
  for (int i=0; i < elem_l_arr_.size(); i++) 
    DOUT << elem_l_arr_[i]->name () << ' ';
#endif
}
