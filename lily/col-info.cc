/*
  col-info.cc -- implement Column_info

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "p-col.hh"
#include "col-info.hh"
#include "debug.hh"

void
Column_info::print() const
{
#ifndef NPRINT
  DOUT << "column { ";
  if (fixed_b())
    DOUT << "fixed at " << fixed_position() << ", ";
  assert (pcol_l_);
  DOUT << width_.str();
  Direction d = LEFT;
  do {
    for (int i=0; i < rods_[d].size (); i++)
      rods_[d][i].print ();
  } while (flip (&d) != LEFT);
  
  DOUT <<"}\n";
#endif
}

Column_info::Column_info (Paper_column *col_l, Real const *fixed_C)
{
  if (fixed_C)
    fixpos_p_.set_l (fixed_C);
  ugh_b_ = false;
  pcol_l_ = col_l;
  width_ = pcol_l_->width();
  if (width_.empty_b())
    width_ = Interval(0,0);
}


Column_info::Column_info()
{
  ugh_b_ = false;
  pcol_l_ =0;
}

bool
Column_info::fixed_b () const
{
 return fixpos_p_.get_C();
}

Real
Column_info::fixed_position () const
{
  return *fixpos_p_;
}

int
Column_info::rank_i () const
{
  return pcol_l_->rank_i ();
}

void
Spacer_rod::print ()const
{
  DOUT << "Other " << other_idx_ << "dist = " << distance_f_ << '\n';
}
