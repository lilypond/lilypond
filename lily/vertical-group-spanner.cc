/*
  vertical-group-spanner.cc -- implement Vertical_group_spanner

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "vertical-group-spanner.hh"
#include "item.hh"
#include "p-col.hh"

/**
  Uncouple all elements of this Vertical_group. Ugh!
 */
void
Vertical_group_spanner::remove_all()
{
    for (int i=0; i < elem_l_arr_.size(); i++){
	elem_l_arr_[i]->y_group_l_ = 0;
    }
    elem_l_arr_.set_size(0);
    junk_dependencies();   
}

void
Vertical_group_spanner::do_break_processing()
{
    set_my_columns();
    if ( line_l() ) {
	return;
    }
    break_into_pieces( false );

    Link_array<Score_elem> elem_l_arr = elem_l_arr_;
    remove_all();
    
    
    
    for (int i=0; i < elem_l_arr.size(); i++) {
	Score_elem * elt = elem_l_arr[i];
	Line_of_score *elt_line = elt->line_l();
	
	if  ( elt-> y_group_l_ )
	    continue;
	
	if ( ! elt_line ){ 
	    if (elt->spanner()) {
		Spanner * sp = elt->spanner();
		
		for (int j =0; j < broken_into_l_arr_.size(); j++) {
		    Vertical_group_spanner * my_broken_l
			 = (Vertical_group_spanner*)broken_into_l_arr_[j];
		    
		    Spanner * broken_span_l 
			= sp->find_broken_piece(my_broken_l->line_l());
		    
		    if (broken_span_l) {
			my_broken_l->add_element(broken_span_l );
		    }
		}
	    } else if (elt->item() && elt->item()->pcol_l_->breakpoint_b()
		       && elt->item()->break_status_i() == 0) {
		for (int j =0; j < 2; j++) {
		    Item * my_item = elt->item()->broken_to_a_[j];
		    Line_of_score * item_line_l_ = my_item->line_l() ;
		    if ( ! item_line_l_ ) 
			continue;
		    
		    Vertical_group_spanner * v
			= (Vertical_group_spanner*)find_broken_piece( item_line_l_);
		    if (v) {
			v->add_element( my_item );
		    }
		}
	    }
	} else {
	    Vertical_group_spanner * my_broken_l
		= (Vertical_group_spanner*)find_broken_piece( elt->line_l() );
	    my_broken_l->add_element( elt );
	}
    }
    for (int j =0; j < broken_into_l_arr_.size(); j++) 
	broken_into_l_arr_[j]->break_processing();	
}

Vertical_group_spanner::Vertical_group_spanner()
{
}

void
Vertical_group_spanner::do_print() const
{
    Vertical_group::do_print();
}



IMPLEMENT_IS_TYPE_B2(Vertical_group_spanner, Spanner, Vertical_group);

Vertical_group_spanner::Vertical_group_spanner(Vertical_group_spanner const &s)
    : Spanner(s), Vertical_group(s)
{
}
