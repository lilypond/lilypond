/*
  paper-def.cc -- implement Paper_def

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include <math.h>
#include "misc.hh"
#include "paper-def.hh"
#include "debug.hh"
#include "lookup.hh"
#include "dimen.hh"



Real
Paper_def::duration_to_dist(Moment d)
{
    if (!d)
	return 0;
    
    return whole_width * pow(geometric_, log_2(d));
}

Real
Paper_def::rule_thickness()const
{
    return 0.4 PT;
}

Paper_def::Paper_def(Lookup *l)
{
    lookup_p_ = l;
    linewidth = 15 *CM_TO_PT;		// in cm for now
    whole_width = 8 * note_width();
    geometric_ = sqrt(2);
    outfile = "lelie.tex";
}

Paper_def::~Paper_def()
{
    delete lookup_p_;
}
Paper_def::Paper_def(Paper_def const&s)
{
    lookup_p_ = new Lookup(*s.lookup_p_);
    geometric_ = s.geometric_;
    whole_width = s.whole_width;
    outfile = s.outfile;
    linewidth = s.linewidth;
}

void
Paper_def::set(Lookup*l)
{
    assert(l != lookup_p_);
    delete lookup_p_;
    lookup_p_ = l;
}

Real
Paper_def::interline_f() const
{
    return lookup_p_->ball(4).dim.y.length();
}

Real
Paper_def::interbeam_f() const
{
    return lookup_p_->interbeam_f();
}
Real
Paper_def::internote_f() const
{
    return lookup_p_->internote_f();
}
Real
Paper_def::note_width()const
{
    return lookup_p_->ball(4).dim.x.length( );
}
Real
Paper_def::standard_height() const
{
    return 20 PT;
}

void
Paper_def::print() const
{
#ifndef NPRINT
    mtor << "Paper {width: " << print_dimen(linewidth);
    mtor << "whole: " << print_dimen(whole_width);
    mtor << "out: " <<outfile;
    lookup_p_->print();
    mtor << "}\n";
#endif
}
Lookup const *
Paper_def::lookup_l()
{
    return lookup_p_;
}
