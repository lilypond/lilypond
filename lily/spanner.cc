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
IMPLEMENT_IS_TYPE_B1(Spanner,Score_elem);

void
Spanner::do_print()const
{
#ifndef NPRINT
    mtor << "Between col ";
// huh? this does not work 
//    mtor << ( left_col_l_ ? left_col_l_->rank_i() : "nop" );
    if ( left_col_l_ )
    	mtor << left_col_l_->rank_i();
    else 
	mtor << "nop";
    mtor << ", ";
//    mtor << ( right_col_l_ ? right_col_l_->rank_i() : "nop" );
    if ( right_col_l_ )
    	mtor << right_col_l_->rank_i();
    else 
	mtor << "nop";
    if (broken_into_l_arr_.size())
	mtor << "with broken pieces\n";
#endif
}

void
Spanner::break_into_pieces()
{
    PCol * left = left_col_l_;
    PCol * right = right_col_l_;
    if(left->daddy_l_) left = left->daddy_l_;
    if(right->daddy_l_) right = right->daddy_l_;
    
    
    Link_array<PCol> break_cols = pscore_l_->broken_col_range(left,right);
    Link_array<Spanner> broken_into_l_arr;

    break_cols.insert(left,0);
    break_cols.push(right);

    for (int i=1; i < break_cols.size(); i++) {
	Spanner* span_p = clone()->spanner();
	left = break_cols[i-1];
	right = break_cols[i];
	if (!right->line_l_)
	    right = right->prebreak_p_;
	if (!left->line_l_)
	    left = left->postbreak_p_;

	assert(left&&right && left->line_l_ == right->line_l_);

	span_p->left_col_l_  = left;
	span_p->right_col_l_ = right;
	
	pscore_l_->typeset_broken_spanner(span_p);
	broken_into_l_arr.push( span_p );
    }
    
    broken_into_l_arr_ = broken_into_l_arr;
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
    Real r = right_col_l_->hpos_f_;
    Real l = left_col_l_->hpos_f_;
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
