/*
  idealspacing.cc -- implement Idealspacing

  source file of the GNU LilyPond music typesetter

  (c) 1996,1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "idealspacing.hh"
#include "p-col.hh"
#include "p-score.hh"
#include "debug.hh"

void
Idealspacing::print() const
{
#ifndef NPRINT
    mtor << "idealspacing {" ;
    mtor << "distance "<<space_f_<< " strength " << hooke_f_ ;
    mtor << "left " << left_i_ << " right " << right_i_ << "}\n";
#endif
}

Idealspacing::Idealspacing()
{
    space_f_ = 0.0;
    hooke_f_ = 0.0;
    left_i_ = -1;
    right_i_ = -1;
}

void
Idealspacing::OK() const
{
    assert(hooke_f_ >= 0);
}
