/*   
  spring.cc --  implement Spring
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "spring.hh"
#include "debug.hh"
#include "item.hh"
#include "spaceable-grob.hh"
#include "paper-column.hh"

Spring::Spring ()
{
  item_l_drul_[LEFT]  =item_l_drul_[RIGHT] =0;
  distance_f_ =0.;
  strength_f_ =1.0;
}

/*

 ugh : if we go from items to cols, we should adjust distance and strength.
 */

void
Spring::add_to_cols ()
{
  Spaceable_grob::add_spring (item_l_drul_[LEFT]->column_l (),
				 item_l_drul_[RIGHT]->column_l (),
				 distance_f_, strength_f_);
}

void
Spring::set_to_cols( )
{
  Direction d = LEFT;
  do
    {
      item_l_drul_[d] = item_l_drul_[d]->column_l ();
    }
  while (flip (&d) != LEFT);

}

Column_spring::Column_spring ()
{
  other_l_ = 0;
  distance_f_ =0;
  strength_f_ =1.0;
}



