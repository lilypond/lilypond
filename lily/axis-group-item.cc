/*
  axis-item.cc -- implement Axis_group_item

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "axis-group-item.hh"
#include "paper-column.hh"



void
Axis_group_item::do_breakable_col_processing()
{
#if 0
  if (!breakable_b ()) // ugh should merge with Item code
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
	  Axis_group_item * my_brok
	    = dynamic_cast<Axis_group_item*> (find_broken_piece(j));
	  Item *new_l = it_l->find_broken_piece (j);
	  my_brok->add_element (new_l);
	}
      while (flip(&j)!=LEFT);
    }
#endif
  Item::do_breakable_col_processing();
}

