/*
  axis-group-element.cc -- implement Axis_group_element

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "axis-group-element.hh"

void
Axis_group_element::do_unlink()
{
  remove_all();
}

void
Axis_group_element::do_junk_links()
{
  axis_admin_.elem_l_arr_.set_size (0);
}


bool
Axis_group_element::contains_b (Graphical_element const*e) const
{
  return axis_admin_.contains_b (e);
}

Link_array<Score_elem>
Axis_group_element::get_extra_dependencies() const
{
  return elem_l_arr ();
}

Link_array<Score_elem>
Axis_group_element::elem_l_arr () const
{  
  /*
    ugh. I know
   */
  Link_array<Score_elem> r;
  for (int i=0; i < axis_admin_.elem_l_arr_.size (); i++)
    r.push ((Score_elem*)axis_admin_.elem_l_arr_[i]);
  return r;
}

void
Axis_group_element::do_print() const
{
  axis_admin_.print();
}


Axis_group_element::Axis_group_element()
{
  transparent_b_ = true;
}


IMPLEMENT_IS_TYPE_B1(Axis_group_element, Score_elem);
