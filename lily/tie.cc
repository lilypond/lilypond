/*
  tie.cc -- implement Tie

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "paper-def.hh"
#include "tie.hh"
#include "note-head.hh"
#include "p-col.hh"


void
Tie::set_head(int x_pos, Note_head * head_l)
{
    if (x_pos >0) {
	assert(!right_head_l_);
	right_head_l_ = head_l;
    } else {
	assert(!left_head_l_);
	left_head_l_ = head_l;
    }
    add_dependency(head_l);
}

Tie::Tie()
{
    right_head_l_ =0;
    left_head_l_ =0;
    same_pitch_b_ =false;
}

void
Tie::set_default_dir()
{
    int m= (left_head_l_->position_i_ + right_head_l_->position_i_) /2 ;
    dir_i_ =  (m < 5)? -1:1;			// ugh
}
    

void
Tie::do_add_processing()
{
    assert(left_head_l_ && right_head_l_);
    left_col_l_ = left_head_l_ -> pcol_l_;
    right_col_l_ = right_head_l_ -> pcol_l_;
}

/**
  This is already getting hairy. Should use Note_head *heads[2]
 */
void
Tie::do_post_processing()
{
    assert(left_head_l_ || right_head_l_);
    left_pos_i_ =  (left_head_l_)? 
	left_head_l_->position_i_ : right_head_l_->position_i_;
    right_pos_i_ = (right_head_l_) ? 
	right_head_l_->position_i_ : left_head_l_->position_i_;
 
    if ( right_head_l_ && right_head_l_->extremal_i_) {
	right_pos_i_ += 2*dir_i_;
	right_dx_f_ -= 0.25;
    } else
	right_dx_f_ -= 0.5;

    if (left_head_l_ && left_head_l_->extremal_i_) {
	left_pos_i_ += 2*dir_i_;
	left_dx_f_ += 0.25;
    } else
	left_dx_f_ += 0.5;
    
    if (!right_head_l_)
	right_pos_i_ = left_pos_i_;
    if (! left_head_l_)
	left_pos_i_ = right_pos_i_;
}



void
Tie::do_substitute_dependency(Score_elem*o, Score_elem*n)
{
    Note_head *new_l =n?(Note_head*)n->item():0;
    if (o->item() == left_head_l_)
	left_head_l_ = new_l;
    else if (o->item() == right_head_l_)
	right_head_l_ = new_l;
}


IMPLEMENT_STATIC_NAME(Tie);
