/*
  axis-group-element.cc -- implement Axis_group_element

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "axis-group-element.hh"
#include "graphical-axis-group.hh"



Link_array<Score_element>
Axis_group_element::get_extra_dependencies() const
{
  return elem_l_arr ();
}

Link_array<Score_element>
Axis_group_element::elem_l_arr () const
{  
  /*
    ugh. I know
  */
  Link_array<Score_element> r;
  for (int i=0; i < elem_l_arr_.size (); i++)
    r.push (dynamic_cast<Score_element*>(elem_l_arr_[i]));
      
  return r;
}

Link_array<Score_element> 
Axis_group_element::get_children ()
{
  Link_array<Score_element> childs;
  Link_array<Score_element> elems = elem_l_arr ();
  for (int i=0; i < elems.size (); i++) 
    {
      Score_element* e = elems[i];
      childs.push (e) ;
      Axis_group_element * axis_group= dynamic_cast <Axis_group_element *> (e);
      if (axis_group)
      	childs.concat (axis_group->get_children ());      
    }
  
  return childs;
}

void
Axis_group_element::do_print() const
{
  Graphical_axis_group::do_print();
}

Axis_group_element::Axis_group_element()
{
  set_elt_property (transparent_scm_sym, SCM_BOOL_T);
}

void
Axis_group_element::set_axes (Axis a1, Axis a2)
{
  Graphical_axis_group::set_axes (a1,a2);
  dim_cache_[X_AXIS].set_empty ((a1 != X_AXIS && a2 != X_AXIS));
  dim_cache_[Y_AXIS].set_empty ((a1 != Y_AXIS && a2 != Y_AXIS));
}


void
Axis_group_element::do_substitute_element_pointer (Score_element*o,
						   Score_element*n)
{
  int i;
  while ((i = elem_l_arr_.find_i (o))>=0) 
    if (n) 
      elem_l_arr_[i] = n;
    else
      elem_l_arr_.del (i);
}

Interval
Axis_group_element::do_height () const
{
  return Graphical_axis_group::extent (Y_AXIS);
}

Interval
Axis_group_element::do_width () const
{
  return Graphical_axis_group::extent (X_AXIS);
}
