
#include "spanner.hh"
#include "line.hh"

#include "symbol.hh"
#include "molecule.hh"
#include "pcol.hh"

String
Spanner::TeXstring() const
{
    assert(right->line);
    Real w = left->hpos - right->hpos;
    return strets->eval(w).tex;
}

// todo.
Spanner *
Spanner::broken_at(const PCol *c1, const PCol *c2) const
{
    Spanner *sp = new Spanner(*this);
    sp->left = c1;
    sp->right = c2;
    return sp;
}

Spanner::Spanner()
{
    pstaff_=0;
    strets=0;
    left = right = 0;
}

