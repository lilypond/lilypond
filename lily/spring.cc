/*   
  spring.cc --  implement Spring
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "spring.hh"
#include "debug.hh"
#include "item.hh"
#include "spaceable-element.hh"
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
  Spaceable_grob::add_spring (item_l_drul_[LEFT]->column_l (),
				 item_l_drul_[RIGHT]->column_l (),
				 distance_f_, strength_f_);
}


Column_spring::Column_spring ()
{
  other_l_ = 0;
  distance_f_ =0;
  strength_f_ =1.0;
}



