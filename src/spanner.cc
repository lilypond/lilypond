#include "pstaff.hh"
#include "debug.hh"
#include "pscore.hh"
#include "spanner.hh"
#include "symbol.hh"
#include "molecule.hh"
#include "pcol.hh"

String
Spanner::TeXstring() const
{
    
    assert(output);
    return output->TeXstring();
}

Spanner::Spanner()
{
    pstaff_=0;
    left = right = 0;
}

void
Spanner::process()
{
}

void
Spanner::preprocess()
{
}

Interval
Spanner::width()const
{
    return Interval(0,right->hpos - left->hpos);
}

Paperdef*
Spanner::paper()const
{
    assert(pstaff_);
    return pstaff_->pscore_->paper_;
}
void
Spanner::print()const
{
#ifndef NPRINT
    mtor << "Spanner { Output ";
    output->print();
    
    mtor << "}\n";
#endif
}
