/*
  align-elem.cc -- implement Align_elem

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "align-element.hh"
#include "interval.hh"
#include "direction.hh"


struct Align_element_content {
  Score_element * elem_l_;
  int priority_i_;
  
  static int compare (Align_element_content const &h1, 
		      Align_element_content const &h2) 
  {
    return h1.priority_i_ - h2.priority_i_;
  }
  Align_element_content (Score_element *elem_l, int p) 
  {
    priority_i_ = p;
    elem_l_ = elem_l;
  }
  Align_element_content () {
    elem_l_ = 0;
    priority_i_ = 0;
  }
};



void
Align_element::add_element (Score_element*el_l)
{
  int p = priority_i_arr_.size ();
  add_element_priority (el_l, p);
}

void
Align_element::add_element_priority (Score_element *el, int p)
{
  assert (! contains_b (el));
  elem_l_arr_.push (el);
  priority_i_arr_.push (p);
  add_dependency (el);
}

void
Align_element::do_substitute_dependency (Score_element*o,
					 Score_element*n)
{
  int i;
  while ((i = elem_l_arr_.find_i (o))>=0) 
    if (n) 
      elem_l_arr_[i] = n;
    else
      elem_l_arr_.del (i);

  if (o == center_l_)
    {
      center_l_ = n;
    }
}

/**
  Align elements top to bottom. 
  The first element has its top at y = 0.0 afterwards

  TODO configurable, like Horizontal_align_item

  TODO should parametrise in direction and coordinate.
 */
void
Align_element::do_post_processing()
{
  if (axis_ == Y_AXIS)
    do_side_processing ();
}

void
Align_element::do_pre_processing ()
{
  if (axis_ == X_AXIS)
    do_side_processing ();
}

void
Align_element::do_side_processing ()
{
  sort_elements ();
  Array<Interval> dims;
  
  for (int i=0; i < elem_l_arr_.size(); i++) 
    {
      Interval y = elem_l_arr_[i]->extent(axis_) ;
      if (y.empty_b())
	y = Interval (0,0);
	
      dims.push (y);
    }

  Real where_f=0;
  Real center_f = 0.0;
  for (int i=0 ;  i < elem_l_arr_.size(); i++) 
    {
      Real dy = - stacking_dir_ * dims[i][-stacking_dir_];
      if (i)
	dy += stacking_dir_ * dims[i-1][stacking_dir_];

      if (i)
	{
	  dy = (dy >? threshold_interval_[SMALLER] )
	    <? threshold_interval_[BIGGER];
	}

      
      if (!i && align_dir_ == LEFT)
	center_f = where_f;
      else if (align_dir_ == CENTER && elem_l_arr_[i] == center_l_)
	center_f = where_f;

      where_f += stacking_dir_ * dy;

            
      elem_l_arr_[i]->translate_axis (where_f, axis_);
    }

  if (align_dir_ == RIGHT)
    center_f = where_f;
  
  if (center_f)
    for  (int i=0 ;  i < elem_l_arr_.size(); i++)
      elem_l_arr_[i]->translate_axis (- center_f, axis_);
}

Align_element::Align_element()
{
  threshold_interval_ = Interval (0, Interval::infinity ());
  transparent_b_ = true;
  set_empty (true);
  stacking_dir_ = DOWN;
  align_dir_ = LEFT;
  axis_ = X_AXIS;
  center_l_ =0;
}


bool
Align_element::contains_b (Score_element const *e) const
{
  return elem_l_arr_.find_l (e);
}


IMPLEMENT_IS_TYPE_B1(Align_element, Score_element);

void
Align_element::sort_elements ()
{
  Array<Align_element_content> content;
  for  (int i =0; i < elem_l_arr_.size(); i++) 
    content.push (Align_element_content (elem_l_arr_[i], priority_i_arr_[i]));
  content.sort (Align_element_content::compare);
  
  elem_l_arr_.clear();
  priority_i_arr_.clear();

  for  (int i =0; i < content.size(); i++) 
    {
      elem_l_arr_.push (content[i].elem_l_);
      priority_i_arr_.push (content[i].priority_i_);
    }
}

void
Align_element::do_print () const
{
#if 0
  DOUT << "contains: ";
  for (int i=0 ;  i < item_l_arr_.size(); i++) 
    DOUT << item_l_arr_[i]->name () << ", ";
#endif
}
