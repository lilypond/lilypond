/*   
  rod.cc --  implement Rod, Column_rod
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "rod.hh"
#include "paper-column.hh"
#include "debug.hh"
#include "dimensions.hh"
#include "single-malt-grouping-item.hh"


Rod::Rod ()
{
  distance_f_ = 0.0;
  item_l_drul_[LEFT] = item_l_drul_[RIGHT] =0;
}

void
Column_rod::print () const
{
#ifndef NDEBUG
  DEBUG_OUT << "Column_rod { rank = "
       << other_l_->rank_i () << ", dist = " << distance_f_ << "}\n";   
#endif
}

Column_rod::Column_rod ()
{
  distance_f_ = 0;
  other_l_ = 0;
}
  
int
Column_rod::compare (const Column_rod &r1, const Column_rod &r2)
{
  return r1.other_l_->rank_i() - r2.other_l_->rank_i();
}

void
Rod::add_to_cols ()
{
  Direction d = LEFT;
  Drul_array<Paper_column*> cols;
  Real extra_dist = 0.0;
  do {
    cols[d] = item_l_drul_[d]->column_l ();
    extra_dist += item_l_drul_[d]->relative_coordinate (cols[d], X_AXIS);
  } while ((flip (&d))!=LEFT);

  if (cols[LEFT] != cols[RIGHT])
    do
      {
	cols[-d]->add_rod(cols[d], distance_f_ + extra_dist);
      }
    while ((flip (&d))!=LEFT);
}

