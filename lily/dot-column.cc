/*
  dot-column.cc -- implement Dot_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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

  add_support (r);
  add_dots (r->dots_l_);
}

void
Dot_column::do_substitute_element_pointer (Score_element*o,Score_element*n)
{
  Note_head_side::do_substitute_element_pointer (o,n);
  if (Dots * d = dynamic_cast<Dots*> (o))
    dot_l_arr_.substitute (d, dynamic_cast<Dots*> (n));
}

int
Dot_column::compare (Dots * const &d1, Dots * const &d2)
{
  return d1->position_i_ - d2->position_i_;
}

void
Dot_column::do_pre_processing ()
{
  dot_l_arr_.sort (Dot_column::compare);
  Note_head_side::do_pre_processing ();
}

Dot_column::Dot_column ()
{
  align_dir_ = RIGHT;
  set_axes(X_AXIS,X_AXIS);
}

void
Dot_column::do_post_processing ()
{
  if (dot_l_arr_.size () < 2)
    return;
  Slice s;
  s.set_empty ();
  
  for (int i=0; i < dot_l_arr_.size (); i++)
    {
      s.unite (Slice (dot_l_arr_[i]->position_i_,dot_l_arr_[i]->position_i_));      
    }
  int  middle = s.center ();
  /*
    +1 -> off by one 
   */
  int pos = middle - dot_l_arr_.size () + 1;
  if (!(pos % 2))
    pos ++;			// center () rounds down.

  for (int i=0; i  <dot_l_arr_.size (); pos += 2, i++)
    {
      dot_l_arr_[i]->position_i_ = pos;
    }
}
