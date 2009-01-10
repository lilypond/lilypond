/*
  rod.cc -- implement Rod, Column_rod

  source file of the GNU LilyPond music typesetter

  (c) 1998--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "rod.hh"

#include "paper-column.hh"
#include "warn.hh"
#include "dimensions.hh"
#include "spaceable-grob.hh"

Rod::Rod ()
{
  distance_ = 0.0;
  item_drul_[LEFT] = item_drul_[RIGHT] = 0;
}

void
Rod::columnize ()
{
  if (!item_drul_[LEFT]
      || !item_drul_[RIGHT])
    return;

  Direction d = LEFT;
  do
    {
      Paper_column *pc = item_drul_[d]->get_column ();
      distance_ += -d * item_drul_[d]->relative_coordinate (pc, X_AXIS);
      item_drul_[d] = pc;
    }
  while ((flip (&d)) != LEFT);
}

void
Rod::add_to_cols ()
{
  columnize ();
  if (item_drul_[LEFT] != item_drul_[RIGHT]
      && item_drul_[LEFT] && item_drul_[RIGHT])
    Spaceable_grob::add_rod (item_drul_[LEFT],
			     item_drul_[RIGHT],
			     distance_);
}

