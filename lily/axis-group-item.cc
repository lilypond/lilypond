/*
  axis-item.cc -- implement Axis_group_item

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "axis-group-item.hh"
#include "p-col.hh"

IMPLEMENT_IS_TYPE_B2(Axis_group_item, Axis_group_element, Item);

void
Axis_group_item::OK() const
{
    Link_array<Score_elem> elems = axis_admin_.elem_l_arr_; 
    for (int i=0; i < elems.size(); i++) {
	Item * it_l = elems[i]->item();
	assert (it_l);
  
	// somebody probably broke it in pieces
	assert (it_l->pcol_l_ == pcol_l_);
    }
}

void
Axis_group_item::do_breakable_col_processing()
{
    if (!pcol_l_->breakable_b())
	return;
    OK();
    copy_breakable_items();
    
	    
    Link_array<Score_elem> elems = axis_admin_.elem_l_arr_; 
    for (int i=0; i < elems.size(); i++) {
	Item* it_l = elems[i]->item();
	for ( int j=0; j < 2; j++) {
	    Item *new_l = it_l->find_prebroken_piece (broken_to_a_[j]->pcol_l_);
	    ((Axis_group_item*)broken_to_a_[j])->add_element (new_l);
	}
    }
    Item::do_breakable_col_processing();
}

void
Axis_group_item::do_print() const
{ 
    Axis_group_element::do_print(); 
}
