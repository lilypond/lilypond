/*
  dot-column.cc -- implement Dot_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "dots.hh"
#include "dot-column.hh"
#include "rhythmic-head.hh"

void
Dot_column::add (Dots *d)
{
  dot_l_arr_.push (d);
  add_dependency (d);
  add_element (d);
}

void
Dot_column::add (Rhythmic_head *r)
{
  if (!r->dots_l_)
    return ;
  
  head_l_arr_.push (r);
  add_dependency (r);
  add (r->dots_l_);
}

void
Dot_column::do_substitute_dependency (Score_elem*o,Score_elem*n)
{
  Item *oi =o->item ();
  Item *ni = n?n->item ():0;
  
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
