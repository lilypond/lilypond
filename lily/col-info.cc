/*
  col-info.cc -- implement Colinfo

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "p-col.hh"
#include "col-info.hh"
#include "debug.hh"

void
Colinfo::print() const
{
#ifndef NPRINT
    mtor << "column { ";
    if (fixed())
	mtor << "fixed at " << fixed_position()<<", ";
    assert(pcol_l_);
    mtor << "[" << minleft() << ", " << minright() << "]";
    mtor <<"}\n";
#endif
}

Colinfo::Colinfo(PCol *col_l, Real const *fixed_C)
{
    if (fixed_C)
	fixpos_p_.set_l(fixed_C);
    ugh_b_ = false;
    pcol_l_ = col_l;
    width = pcol_l_->width();
}


Colinfo::Colinfo()
{
    ugh_b_ = false;
    pcol_l_ =0;
}

