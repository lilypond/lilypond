/*   
  spring.cc --  implement Spring
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "spring.hh"
#include "debug.hh"
#include "item.hh"
#include "paper-column.hh"

Spring::Spring ()
{
  item_l_drul_[LEFT]  =item_l_drul_[RIGHT] =0;
  distance_f_ =0.;
  strength_f_ =1.0;
}

void
Spring::add_to_cols ()
{
  Direction d = LEFT;
  do
    {
      item_l_drul_[-d]->column_l ()->add_spring
	(item_l_drul_[d]->column_l (),
	 distance_f_, strength_f_);
    }
  while ((flip (&d))!=LEFT);
}


Column_spring::Column_spring ()
{
  other_l_ = 0;
  distance_f_ =0;
  strength_f_ =1.0;
}


int
Column_spring::compare (Column_spring const & r1, Column_spring const &r2)
{
  return r1.other_l_->rank_i() - r2.other_l_->rank_i();
}

void
Column_spring::print () const
{
#ifndef NPRINT
  DOUT << "Column_spring { rank = "
       << other_l_->rank_i () << ", dist = " << distance_f_ << "}\n";   

#endif
}
