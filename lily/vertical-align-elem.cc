/*
  vertical-align-item.cc -- implement Vertical_align_elem

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "vertical-align-elem.hh"
#include "interval.hh"

void
Vertical_align_element::add (Score_elem*el_l)
{
  assert (! contains_b (el_l));
  elem_l_arr_.push (el_l);
  add_dependency (el_l);
}

void
Vertical_align_element::do_substitute_dependency (Score_elem*o,Score_elem*n)
{
  int i;
  while ((i = elem_l_arr_.find_i (o))>=0) 
    if (n) 
      elem_l_arr_[i] = n;
    else
      elem_l_arr_.del (i);
}

/**
  Align elements top to bottom. 
  The first element has its top at y =0.0 afterwards

  TODO configurable, like Horizontal_align_item
 */
void
Vertical_align_element::do_post_processing()
{
  Array<Interval> dims;
  for (int i=0; i < elem_l_arr_.size(); i++) 
    {
      Interval y = elem_l_arr_[i]->height() ;
      if (y.empty_b())
	y = Interval (0,0);
	
      dims.push (y);
    }

  Real where_f=0;
  for (int i=0 ;  i < elem_l_arr_.size(); i++) 
    {
      elem_l_arr_[i]->translate_axis (- dims[i][1] - where_f, Y_AXIS);
      where_f += dims[i].length();
    }
}

bool
Vertical_align_element::contains_b (Score_elem const *e) const
{
  return elem_l_arr_.find_l (e);
}

Vertical_align_element::Vertical_align_element()
{
  transparent_b_ = true;
  set_empty (true);
}


IMPLEMENT_IS_TYPE_B1(Vertical_align_element, Score_elem);

