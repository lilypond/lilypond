#include "idealspacing.hh"
#include "p-col.hh"
#include "p-score.hh"
#include "p-staff.hh"
#include "debug.hh"

void
Idealspacing::print() const
{
#ifndef NPRINT
    mtor << "idealspacing {" ;
    mtor << "distance "<<space<< " strength " << hooke ;
    mtor << "left " << left->rank() << " right " << right->rank() << "}\n";
#endif
}

Idealspacing::Idealspacing(const PCol * l,const PCol * r)
{
    space = 0.0;
    hooke = 0.0;
    left = l;
    right = r;
}

void
Idealspacing::OK() const
{
#ifndef NDEBUG
    assert(hooke >= 0 && left  && right);
#endif    
}
