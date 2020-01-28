/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2012--2020 Mike Solomon <mike@mikesolomon.org>

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

#include "engraver.hh"
#include "fingering-column.hh"
#include "item.hh"
#include "pointer-group-interface.hh"
#include "side-position-interface.hh"

#include "translator.icc"

using std::vector;

/**
   Find potentially colliding scripts, and put them in a
   Fingering_column, that will fix the columns.  */
class Fingering_column_engraver : public Engraver
{
  Drul_array<Grob *> fingering_columns_;
  Drul_array<vector<Grob *>> scripts_;
  vector<Grob *> possibles_;

public:
  TRANSLATOR_DECLARATIONS (Fingering_column_engraver);

protected:
  void acknowledge_finger (Grob_info);
  void process_acknowledged ();
  void stop_translation_timestep ();
};

Fingering_column_engraver::Fingering_column_engraver (Context *c) : Engraver (c)
{
  for (LEFT_and_RIGHT (d))
    fingering_columns_[d] = 0;
}

void
Fingering_column_engraver::stop_translation_timestep ()
{
  for (vsize i = 0; i < possibles_.size (); i++)
    if (!Item::is_non_musical (possibles_[i]))
      {
        if (Side_position_interface::get_axis (possibles_[i]) == X_AXIS)
          {
            Direction d = robust_scm2dir (
                possibles_[i]->get_property ("direction"), CENTER);
            if (d)
              scripts_[d].push_back (possibles_[i]);
            else
              possibles_[i]->warning (
                  "Cannot add a fingering without a direction.");
          }
      }

  for (LEFT_and_RIGHT (d))
    {
      if (scripts_[d].size () < 2 && fingering_columns_[d])
        {
          fingering_columns_[d]->suicide ();
          fingering_columns_[d] = 0;
        }
      if (fingering_columns_[d])
        {
          for (vsize i = 0; i < scripts_[d].size (); i++)
            Fingering_column::add_fingering (fingering_columns_[d],
                                             scripts_[d][i]);
        }
      scripts_[d].clear ();
      fingering_columns_[d] = 0;
    }
  possibles_.clear ();
}

void
Fingering_column_engraver::acknowledge_finger (Grob_info inf)
{
  Item *thing = dynamic_cast<Item *> (inf.grob ());
  if (thing)
    possibles_.push_back (thing);
}

void
Fingering_column_engraver::process_acknowledged ()
{
  for (LEFT_and_RIGHT (d))
    {
      if (possibles_.size () > 1 && !fingering_columns_[d])
        fingering_columns_[d] = make_item ("FingeringColumn", SCM_EOL);
    }
}

void
Fingering_column_engraver::boot ()
{
  ADD_ACKNOWLEDGER (Fingering_column_engraver, finger);
}

ADD_TRANSLATOR (Fingering_column_engraver,
                /* doc */
                "Find potentially colliding scripts and put them into a"
                " @code{FingeringColumn} object; that will fix the collisions.",

                /* create */
                "FingeringColumn ",

                /* read */
                "",

                /* write */
                "");
