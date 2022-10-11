/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2007--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "tie-formatting-problem.hh"
#include "grob.hh"
#include "item.hh"
#include "semi-tie.hh"
#include "spanner.hh"
#include "tie.hh"
#include "tie-specification.hh"

Tie_specification::Tie_specification ()
{
  tie_grob_ = 0;
  has_manual_position_ = false;
  has_manual_dir_ = false;
  has_manual_delta_y_ = false;
  position_ = 0;
  manual_position_ = 0;
  manual_dir_ = CENTER;
  note_head_drul_[LEFT] = note_head_drul_[RIGHT] = 0;
  column_ranks_[RIGHT] = column_ranks_[LEFT] = 0;
}

void
Tie_specification::from_grob (Grob *tie)
{
  // In this method, Tie and Semi_tie require the same logic with different
  // types.  It might be clearer to use a template.
  tie_grob_ = tie;
  if (scm_is_number (get_property_data (tie, "direction")))
    {
      manual_dir_ = from_scm<Direction> (get_property (tie, "direction"));
      has_manual_dir_ = true;
    }

  if (Spanner *spanner = dynamic_cast<Spanner *> (tie))
    position_ = Tie::get_position (spanner);
  else if (Item *item = dynamic_cast<Item *> (tie))
    position_ = Semi_tie::get_position (item);
  else
    {
      programming_error ("grob is neither a tie nor a semi-tie");
      position_ = 0;
    }

  SCM pos_scm = get_property (tie, "staff-position");
  if (scm_is_number (pos_scm))
    {
      has_manual_delta_y_ = !is_scm<Rational> (pos_scm);
      manual_position_
        = from_scm<double> (get_property (tie, "staff-position"));
      has_manual_position_ = true;
    }
}

int
Tie_specification::column_span () const
{
  return column_ranks_[RIGHT] - column_ranks_[LEFT];
}
