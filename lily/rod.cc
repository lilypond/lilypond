/*   
  rod.cc --  implement Rod, Column_rod
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "rod.hh"
#include "paper-column.hh"
#include "debug.hh"
#include "dimensions.hh"
#include "separation-item.hh"


Rod::Rod ()
{
  distance_f_ = 0.0;
  item_l_drul_[LEFT] = item_l_drul_[RIGHT] =0;
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
Rod::columnize ()
{
  Direction d = LEFT;
  do {
    Paper_column * pc = item_l_drul_[d]->column_l ();
    distance_f_ += - d * item_l_drul_[d]->relative_coordinate (pc, X_AXIS);
    item_l_drul_[d] = pc;
  } while ((flip (&d))!=LEFT);

}

void
Rod::add_to_cols ()
{
  columnize();
  if (item_l_drul_[LEFT] != item_l_drul_[RIGHT])
    dynamic_cast<Paper_column*> (item_l_drul_[LEFT])->
      add_rod(dynamic_cast<Paper_column*>(item_l_drul_[RIGHT]), distance_f_ );
}

