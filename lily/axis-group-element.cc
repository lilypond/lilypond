/*
  axis-group-element.cc -- implement Axis_group_element

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "axis-group-element.hh"
#include "graphical-axis-group.hh"

void
Axis_group_element::do_unlink()
{
  remove_all();
}

void
Axis_group_element::do_junk_links()
{
  elem_l_arr_.set_size (0);
}


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

Axis_group_element::Axis_group_element(Axis a1, Axis a2)
  : Graphical_axis_group (a1,a2)
{
  transparent_b_ = true;
}

Axis_group_element::Axis_group_element ()
  : Graphical_axis_group (X_AXIS, Y_AXIS)
{
  transparent_b_ = true;
}



