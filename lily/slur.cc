/*
  slur.cc -- implement  Slur

  source file of the GNU LilyPond music typesetter

  (c) 1996, 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

/*

  TODO:
  
  think about crossing stems.
  Begin and end should be treated as a Script.
  
 */
#include "slur.hh"
#include "scalar.hh"
#include "lookup.hh"
#include "paper-def.hh"
#include "note-column.hh"
#include "stem.hh"
#include "p-col.hh"
#include "molecule.hh"
#include "debug.hh"
#include "boxes.hh"



void
Slur::add(Note_column*n)
{
    encompass_arr_.push(n);
    add_dependency(n);
}

void
Slur::set_default_dir()
{
    dir_i_ = -1;
    for (int i=0; i < encompass_arr_.size(); i ++) {
	if (encompass_arr_[i]->dir_i_ < 0) {
	    dir_i_ =1;
	    break;
	}
    }
}

void
Slur::do_pre_processing()
{
    right_col_l_  = encompass_arr_.top()->pcol_l_;
    left_col_l_ = encompass_arr_[0]->pcol_l_;    
}


void
Slur::do_substitute_dependency(Score_elem*o, Score_elem*n)
{
    int i;
    while((i = encompass_arr_.find_i((Note_column*)o->item())) >=0) {
	if (n)
	    encompass_arr_[i] = (Note_column*)n->item();
	else
	    encompass_arr_.del(i);
    }
}


static int 
Note_column_compare(Note_column *const&n1 , Note_column* const&n2)
{
    return n1->pcol_l_->rank_i() - n2->pcol_l_->rank_i();
}

void
Slur::do_post_processing()
{
    encompass_arr_.sort(Note_column_compare);
    if (!dir_i_)
	set_default_dir();
    Real inter_f = paper()->internote_f();
    
    if (encompass_arr_[0]->stem_l_) 
        left_pos_i_ = rint(encompass_arr_[0]->stem_l_->height()[dir_i_]/inter_f);
    else 
        left_pos_i_ = rint ( encompass_arr_[0]->head_positions_interval()[dir_i_]);
    
    if (encompass_arr_.top()->stem_l_)
        right_pos_i_ = rint(encompass_arr_.top()->stem_l_->height()[dir_i_]/inter_f);
    else 
        right_pos_i_ = rint (encompass_arr_.top()->head_positions_interval()[dir_i_]);

    left_pos_i_ += dir_i_;
    right_pos_i_ += dir_i_;
}
IMPLEMENT_STATIC_NAME(Slur);
