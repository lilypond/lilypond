/*
  horizontal-group-item.cc -- implement Horizontal_group_item

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "p-col.hh"
#include "horizontal-group-item.hh"

IMPLEMENT_IS_TYPE_B2(Horizontal_group_item, Horizontal_group, Item);

void
Horizontal_group_item::OK() const
{
    for (int i=0; i < elem_l_arr_.size(); i++) {
	Item * it_l = elem_l_arr_[i]->item();
	
	assert(it_l&& it_l->pcol_l_ == pcol_l_ );
    }
}

void
Horizontal_group_item::do_breakable_col_processing()
{
    if (!pcol_l_->breakable_b())
	return;
    OK();
    copy_breakable_items();
    
    Link_array<Score_elem> elems = elem_l_arr_;

    for (int i=0; i < elems.size(); i++) {
	Item* it_l = elems[i]->item();
	for ( int j=0; j < 2; j++) {
	    Item *new_l = it_l->find_prebroken_piece(broken_to_a_[j]->pcol_l_);
	    ((Horizontal_group_item*)broken_to_a_[j])->add_element( new_l );
	}
    }
}
