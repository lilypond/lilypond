/*
  dot-column.cc -- implement Dot_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "dots.hh"
#include "dot-column.hh"
#include "rhythmic-head.hh"

void
Dot_column::add_dots (Dots *d)
{
  dot_l_arr_.push (d);
  add_dependency (d);
  add_element (d);
}

void
Dot_column::add_head (Rhythmic_head *r)
{
  if (!r->dots_l_)
    return ;
  
  head_l_arr_.push (r);
  add_dependency (r);
  add_dots (r->dots_l_);
}

void
Dot_column::do_substitute_dependency (Score_element*o,Score_element*n)
{
  Item *oi =dynamic_cast <Item *> (o);
  Item *ni = n?dynamic_cast <Item *> (n):0;
  
  if (oi&&oi->is_type_b (Rhythmic_head::static_name ()))
    head_l_arr_.substitute ((Rhythmic_head*)oi, (Rhythmic_head*)ni);
  else if (oi && oi->is_type_b (Dots::static_name ()))
    dot_l_arr_.substitute ((Dots*) oi, (Dots*) ni);
}

void
Dot_column::do_pre_processing ()
{
  Interval w;
  for (int i=0; i < head_l_arr_.size (); i++)
    w.unite (head_l_arr_[i]->width ());
  
  if (!w.empty_b ())
    translate_axis (w[RIGHT] - width() [LEFT],X_AXIS);
}

IMPLEMENT_IS_TYPE_B1(Dot_column, Horizontal_group_item);
