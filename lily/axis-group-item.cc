/*
  axis-item.cc -- implement Axis_group_item

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "axis-group-item.hh"
#include "p-col.hh"

IMPLEMENT_IS_TYPE_B2(Axis_group_item, Axis_group_element, Item);

void
Axis_group_item::OK() const
{
  Link_array<Score_element> elems = elem_l_arr ();
  for (int i=0; i < elems.size(); i++) 
    {
      Item * it_l = dynamic_cast<Item*> (elems[i]);
      assert (it_l);
    }
}

void
Axis_group_item::do_breakable_col_processing()
{
  if (!breakable_b_ || !column_l ()->breakable_b_) // ugh should merge with Item code
    return;
  
  OK();
  copy_breakable_items();
  
	    
  Link_array<Score_element> elems = elem_l_arr ();
  for (int i=0; i < elems.size(); i++) 
    {
      Item* it_l = dynamic_cast<Item*> (elems[i]);
      Direction  j=LEFT;
      do 
	{
	  Item *new_l = 
	    it_l->find_prebroken_piece (broken_to_drul_[j]->break_status_dir_);
	  (dynamic_cast<Axis_group_item*> (broken_to_drul_[j]))->add_element (new_l);
	}
      while (flip(&j)!=LEFT);
    }
  Item::do_breakable_col_processing();
}

void
Axis_group_item::do_print() const
{ 
  Axis_group_element::do_print(); 
  Item::do_print ();
}
