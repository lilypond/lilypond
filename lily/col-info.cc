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
  DOUT << "column { ";
  if (fixed_b())
    DOUT << "fixed at " << fixed_position()<<", ";
  assert (pcol_l_);
  DOUT << width_.str();
  DOUT <<"}\n";
#endif
}

Colinfo::Colinfo (Paper_column *col_l, Real const *fixed_C)
{
  if (fixed_C)
    fixpos_p_.set_l (fixed_C);
  ugh_b_ = false;
  pcol_l_ = col_l;
  width_ = pcol_l_->width();
  if (width_.empty_b())
    width_ = Interval(0,0);
}


Colinfo::Colinfo()
{
  ugh_b_ = false;
  pcol_l_ =0;
}

bool
Colinfo::fixed_b () const
{
 return fixpos_p_.get_C();
}

Real
Colinfo::fixed_position () const
{
  return *fixpos_p_;
}

int
Colinfo::rank_i () const
{
  return pcol_l_->rank_i ();
}
