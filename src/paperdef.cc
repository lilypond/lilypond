#include <math.h>
#include "misc.hh"
#include "paperdef.hh"
#include "debug.hh"
#include "lookup.hh"
#include "dimen.hh"



// golden ratio
const Real PHI = (1+sqrt(5))/2;

// see  Roelofs, p. 57
Real
Paperdef::duration_to_dist(Moment d)
{
    if (!d)
	return 0;
    
    return whole_width * pow(geometric_, log_2(d));
}

Real
Paperdef::rule_thickness()const
{
    return 0.4 PT;
}

Paperdef::Paperdef(Lookup *l)
{
    lookup_p_ = l;
    linewidth = 15 *CM_TO_PT;		// in cm for now
    whole_width = 8 * note_width();
    geometric_ = sqrt(2);
    outfile = "lelie.out";
}

Paperdef::~Paperdef()
{
    delete lookup_p_;
}
Paperdef::Paperdef(Paperdef const&s)
{
    lookup_p_ = new Lookup(*s.lookup_p_);
    geometric_ = s.geometric_;
    whole_width = s.whole_width;
    outfile = s.outfile;
    linewidth = s.linewidth;
}

void
Paperdef::set(Lookup*l)
{
    assert(l != lookup_p_);
    delete lookup_p_;
    lookup_p_ = l;
}

Real
Paperdef::interline() const
{
    return lookup_p_->ball(4).dim.y.length();
}

Real
Paperdef::internote() const
{
    return lookup_p_->internote();
}
Real
Paperdef::note_width()const
{
    return lookup_p_->ball(4).dim.x.length( );
}
Real
Paperdef::standard_height() const
{
    return 20 PT;
}

void
Paperdef::print() const
{
#ifndef NPRINT
    mtor << "Paper {width: " << print_dimen(linewidth);
    mtor << "whole: " << print_dimen(whole_width);
    mtor << "out: " <<outfile;
    mtor << "}\n";
#endif
}
