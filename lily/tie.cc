/*
  tie.cc -- implement Tie

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "tie.hh"
#include "notehead.hh"
#include "p-col.hh"

Spanner*
Tie::do_break_at(PCol*l, PCol*r) const
{
    Tie * tie_p = new Tie;
    Line_of_score const  *line_C=l->line_l_;
    if (left_head_l_->line_l()== line_C)
	tie_p->left_head_l_ = left_head_l_;
    if (right_head_l_->line_l() == line_C)
	tie_p->right_head_l_  = right_head_l_;
    
    return tie_p;
}

void
Tie::set_head(int x_pos, Notehead * head_l)
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
}

void
Tie::do_add_processing()
{
    assert(left_head_l_ && right_head_l_);
    left_col_l_ = left_head_l_ -> pcol_l_;
    right_col_l_ = right_head_l_ -> pcol_l_;
}

void
Tie::do_post_processing()
{
    assert(left_head_l_ || right_head_l_);
    left_pos_i_ =  (left_head_l_)? 
	left_head_l_->position_i_ : right_head_l_->position_i_;
    right_pos_i_ = (right_head_l_) ? 
	right_head_l_->position_i_ : left_head_l_->position_i_;
}


