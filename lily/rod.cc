/*   
  rod.cc --  implement Rod, Column_rod
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "rod.hh"
#include "p-col.hh"
#include "debug.hh"
#include "dimensions.hh"
#include "single-malt-grouping-item.hh"


Rod::Rod (Single_malt_grouping_item *l, Single_malt_grouping_item *r)
{
  item_l_drul_[LEFT] =l;
  item_l_drul_[RIGHT]=r;

  Interval li (l->my_width ());
  Interval ri (r->my_width ());
  if (li.empty_b () || ri.empty_b ())
    distance_f_ = 0;
  else
    distance_f_ = li[RIGHT] - ri[LEFT] + 1.5 PT; // ugh
}
	  
Rod::Rod ()
{
  distance_f_ = 0.0;
  item_l_drul_[LEFT] = item_l_drul_[RIGHT] =0;
}

void
Column_rod::print () const
{
#ifndef NDEBUG
  DOUT << "Column_rod { rank = "
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
  item_l_drul_[RIGHT]->column_l ()->add_rod
    (item_l_drul_[LEFT]->column_l (), distance_f_);
  item_l_drul_[LEFT]->column_l ()->add_rod
    (item_l_drul_[RIGHT]->column_l (), distance_f_);
}
