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
  
  if (oi && dynamic_cast<Rhythmic_head *> (oi))
    head_l_arr_.substitute (dynamic_cast<Rhythmic_head*> (oi),
			    dynamic_cast<Rhythmic_head*>(n));
  else if (oi && dynamic_cast<Dots *> (oi))
    dot_l_arr_.substitute (dynamic_cast<Dots*> (oi),
			   dynamic_cast<Dots*> (n));
}

void
Dot_column::do_pre_processing ()
{
  Interval w;
  for (int i=0; i < head_l_arr_.size (); i++)
    w.unite (head_l_arr_[i]->extent (X_AXIS));
  
  if (!w.empty_b ())
    translate_axis (w[RIGHT] - extent(X_AXIS) [LEFT],X_AXIS);
}


