#include "debug.hh"
#include "spanner.hh"
#include "p-col.hh"



IMPLEMENT_STATIC_NAME(Spanner);

void
Spanner::do_print()const
{
    mtor << " (unknown) ";
}

Spanner*
Spanner::broken_at(PCol*c1, PCol *c2)const
{
    Spanner *span_p = do_break_at(c1,c2);

    for (int i=0; i < dependant_l_arr_.size(); i++) {
	dependant_l_arr_[i]->
	    substitute_dependency((Staff_elem*)this, span_p); 
    }
    
    span_p->left_col_l_ = c1;
    span_p->right_col_l_ = c2;
    
    return span_p;
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
