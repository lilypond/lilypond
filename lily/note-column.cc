/*
  note-column.cc -- implement Note_column

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "note-column.hh"
#include "debug.hh"
#include "script.hh"
#include "note-head.hh"
#include "stem.hh"

IMPLEMENT_STATIC_NAME(Note_column);
IMPLEMENT_IS_TYPE_B1(Note_column,Script_column);

void
Note_column::add(Stem*stem_l)
{
    stem_l_ = stem_l;
    add_support(stem_l);
}

void
Note_column::add(Note_head* n_l)
{
    assert(!n_l->rest_b_);
    head_l_arr_.push(n_l);
    add_support(n_l);
}

Note_column::Note_column()
{
    stem_l_ = 0;
    h_shift_b_ =false;
    dir_i_ =0;
}

void
Note_column::sort()
{
    head_l_arr_.sort( Note_head::compare);
}
    
Interval_t<int>
Note_column::head_positions_interval()const
{
    (    (Note_column*)this)->sort();
    return Interval_t<int> ( head_l_arr_[0]->position_i_, 
			     head_l_arr_.top()->position_i_);

}


void
Note_column::do_pre_processing()
{
    if (!dir_i_){
	if (stem_l_)
	    dir_i_ = stem_l_->dir_i_;
	else 
	    dir_i_ = (head_positions_interval().center() >=  5) ? -1 : 1;
    }
    Script_column::do_pre_processing();
}

    

void
Note_column::do_substitute_dependency(Score_elem*o,Score_elem*n)
{
    Script_column::do_substitute_dependency(o,n);
    if (o->name() == Note_head::static_name()) {
	head_l_arr_.substitute( (Note_head*)o->item(), 
				(n)? (Note_head*)n->item() : 0);
    }
    if (stem_l_ == o) {
	stem_l_ = n ? (Stem*)n->item():0;
    }
}
