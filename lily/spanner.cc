/*
  spanner.cc -- implement Spanner

  source file of the GNU LilyPond music typesetter

  (c) 1996,1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "debug.hh"
#include "spanner.hh"
#include "p-col.hh"
#include "p-score.hh"

IMPLEMENT_STATIC_NAME(Spanner);

void
Spanner::do_print()const
{
    if (broken_into_l_arr_.size())
	mtor << "Spanner with broken pieces\n";
}

void
Spanner::break_into_pieces()
{
    PCol * left = left_col_l_;
    PCol * right = right_col_l_;
    if (left->daddy_l_)
	left = left->daddy_l_;
    if (right->daddy_l_)
	right = right->daddy_l_;
    
    Link_array<PCol> all_cols = pscore_l_->col_range(left, right);
    
    Line_of_score *line = left->line_l_;
    if (!line) {
	left  = left->postbreak_p_;
	line = left->line_l_;
    }
    
    for (int i=1; i < all_cols.size(); i++) {
	if (!all_cols[i]->line_l_) {

	    Spanner* span_p = clone();
	    right =  all_cols[i]->prebreak_p_;
	    assert(left&&right && left->line_l_ == right->line_l_);

	    span_p->left_col_l_  = left;
	    span_p->right_col_l_ = right;
	    left = all_cols[i]->postbreak_p_;
	    line = left->line_l_;
	    
	    pscore_l_->typeset_broken_spanner(span_p);
	    broken_into_l_arr_.push( span_p );
	}
    }
}

void
Spanner::do_break_processing()
{
    if (!left_col_l_->line_l_)
	left_col_l_ = left_col_l_->postbreak_p_;
    if (!right_col_l_->line_l_)
	right_col_l_ = right_col_l_->prebreak_p_;
     
    
    if (!line_l()) {
	break_into_pieces();
	for (int i=0; i < broken_into_l_arr_.size(); i++)
	    broken_into_l_arr_[i]->handle_broken_dependencies();
    } else { 
	handle_broken_dependencies();
    }
}


Spanner::Spanner()
{
    left_col_l_ = right_col_l_ = 0;
}


Interval
Spanner::do_width()const
{
    Real r = right_col_l_->hpos;
    Real l = left_col_l_->hpos;
    assert(*left_col_l_ < *right_col_l_);
    assert(r>=l);
	
    return Interval(0, r-l);
}

Line_of_score *
Spanner::line_l()const
{
    if ( left_col_l_->line_l_ != right_col_l_->line_l_)
	return 0;
    return left_col_l_->line_l_;
}


Spanner*
Spanner::find_broken_piece(Line_of_score*l)const
{
    for (int i=0; i < broken_into_l_arr_.size(); i++)
	if(broken_into_l_arr_[i]->line_l() == l)
	    return broken_into_l_arr_[i];
    return 0;				   
	  
}

bool
Spanner::broken_b()const
{
    return broken_into_l_arr_.size();
}
