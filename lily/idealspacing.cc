/*
  idealspacing.cc -- implement Idealspacing

  source file of the GNU LilyPond music typesetter

  (c) 1996, 1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "idealspacing.hh"
#include "paper-column.hh"
#include "paper-score.hh"
#include "debug.hh"

void
Idealspacing::print() const
{
#ifndef NPRINT
  DEBUG_OUT << "idealspacing {" ;
  DEBUG_OUT << "distance " << space_f_ << " strength " << hooke_f_;
  DEBUG_OUT << "left " << cols_drul_[LEFT] << " right " << cols_drul_[RIGHT] << "}\n";
#endif
}

Idealspacing::Idealspacing()
{
  space_f_ = 0.0;
  hooke_f_ = 0.0;
  cols_drul_[LEFT] = cols_drul_[RIGHT] = -1;
}

void
Idealspacing::OK() const
{
  assert (hooke_f_ >= 0);
}

Real
Idealspacing::energy_f(Real x) const
{
  Real dx = (space_f_ - x);
  
  Real e =  sqr(dx);
  if ( dx < 0)
    e *= 4;			// ugh.
  
  return hooke_f_ * e;
}
