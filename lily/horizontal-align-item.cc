/*
  horizontal-align-item.cc -- implement Horizontal_align_item

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "horizontal-align-item.hh"
#include "debug.hh"

IMPLEMENT_IS_TYPE_B1(Horizontal_align_item,Item);

bool
Horizontal_align_item::contains_b (Item *i) const
{
  return item_l_arr_.find_l (i);
}

void
Horizontal_align_item::add (Item *i,int p)
{
  assert (! contains_b (i));
  priority_i_arr_.push (p);
  item_l_arr_.push (i);
  add_dependency (i);
}

void
Horizontal_align_item::do_substitute_dependency (Score_elem*o,Score_elem*n)
{
  int i;
  while ((i = item_l_arr_.find_i (o->item())) >=0) 
    {
      if (n)
	item_l_arr_[i] = n->item();
      else
	item_l_arr_.del (i);
    }
}

struct Horizontal_align_item_content {
  Item * item_l_;
  int priority_i_;
  static int compare (Horizontal_align_item_content const &h1, 
		      Horizontal_align_item_content const &h2) 
  {
    return h1.priority_i_ - h2.priority_i_;
  }
  Horizontal_align_item_content (Item*i, int p) 
  {
    priority_i_ = p;
    item_l_ = i;
  }
  Horizontal_align_item_content(){item_l_ =0; priority_i_ =0; }
};


void
Horizontal_align_item::do_pre_processing()
{
  {  
    Array<Horizontal_align_item_content> content;
    for  (int i =0; i < item_l_arr_.size(); i++) 
      content.push (
		    Horizontal_align_item_content (
						   item_l_arr_[i], priority_i_arr_[i]));
    content.sort (Horizontal_align_item_content::compare);
    item_l_arr_.clear();
    priority_i_arr_.clear();
    for  (int i =0; i < content.size(); i++) 
      {
	item_l_arr_.push (content[i].item_l_);
	priority_i_arr_.push (content[i].priority_i_);
      }
  }
  
  Array<Interval> dims;
  Real total =0;
  for  (int i =0; i < item_l_arr_.size(); i++) 
    {
	
      Interval item_width= item_l_arr_[i]->width();
      if (item_width.empty_b()) 
	{
	  item_width = Interval (0,0);
	}
      dims.push (item_width);
      total += item_width.length();
    }

  Real where_f= total * (align_i_-1.0)/2.0;
  Real center_dx_f = 0;
  for (int i=0 ;  i < item_l_arr_.size(); i++) 
    {
      Real dx = where_f -dims[i][-1];
      item_l_arr_[i]->translate_axis (dx , X_AXIS);
      if (item_l_arr_[i] == center_l_)
	center_dx_f = where_f;
      where_f += dims[i].length();
    }
  if (center_dx_f && !align_i_)
    for (int i=0 ;  i < item_l_arr_.size(); i++) 
      item_l_arr_[i]->translate_axis (- center_dx_f , X_AXIS);
  
}

Interval
Horizontal_align_item::do_width() const
{
  return Interval (0,0);
}

void
Horizontal_align_item::do_print() const
{
#ifndef NPRINT
  Item::do_print ();
  DOUT << "contains: ";
  for (int i=0 ;  i < item_l_arr_.size(); i++) 
    DOUT << item_l_arr_[i]->name () << ", ";
#endif
}

Horizontal_align_item::Horizontal_align_item()
{
  center_l_ = 0;
  align_i_ = 0;
  set_empty (true);
  transparent_b_ = true;
}
