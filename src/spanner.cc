#include "debug.hh"
#include "spanner.hh"
#include "pcol.hh"

NAME_METHOD(Spanner);
void
Spanner::do_print()const
{
    mtor << " (unknown) ";
}
Spanner*
Spanner::broken_at(PCol*c1, PCol *c2)const
{
    Spanner *me_p = (Spanner*)this;
    Spanner *span_p = do_break_at(c1,c2);

    me_p->calc_children = true;
    me_p->dependencies.push(span_p);

    span_p->calc_children = false; // should handle in ctor

    span_p->left = c1;
    span_p->right = c2;
    
    return span_p;
}

Spanner::Spanner()
{
    left = right = 0;
}


Interval
Spanner::width()const
{
    Real r = right->hpos;
    Real l = left->hpos;
    assert(*left < *right);
    assert(r>=l);
	
    return Interval(0, r-l);
}
