/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
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
Rod::add_to_cols ()
{
  if (!item_drul_[LEFT] || !item_drul_[RIGHT])
    return;

  for (const auto d : {LEFT, RIGHT})
    {
      Paper_column *pc = item_drul_[d]->get_column ();
      distance_ += -d * item_drul_[d]->relative_coordinate (pc, X_AXIS);
      item_drul_[d] = pc;
    }

  if (item_drul_[LEFT] != item_drul_[RIGHT] && item_drul_[LEFT]
      && item_drul_[RIGHT])
    {
      // casts are safe because we just stored Paper_columns above
      Spaceable_grob::add_rod (static_cast<Paper_column *> (item_drul_[LEFT]),
                               static_cast<Paper_column *> (item_drul_[RIGHT]),
                               distance_);
    }
}

Real
Rod::bounds_protrusion () const
{
  // Return the distance that bounds protrude into rod
  Real w = 0;
  for (const auto d : {LEFT, RIGHT})
    {
      if (item_drul_[d])
        w += -d * item_drul_[d]->extent (item_drul_[d], X_AXIS)[-d];
    }
  return w;
}
