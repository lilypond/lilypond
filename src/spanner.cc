#include "debug.hh"
#include "spanner.hh"
#include "pcol.hh"

Spanner*
Spanner::broken_at(PCol*c1, PCol *c2)const
{
    Spanner *me_p = (Spanner*)this;
    Spanner *span_p = do_break_at(c1,c2);

    me_p->calc_children = true;
    me_p->dependencies.add(span_p);

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
    Real r =right->hpos,
	l= left->hpos;
    assert(r>=l);
	
    return Interval(0, r-l);
}

void
Spanner::print() const
{
#ifndef NPRINT
    mtor << "Spanner { ";
    Staff_elem::print();
    mtor << "}\n";
#endif
}

