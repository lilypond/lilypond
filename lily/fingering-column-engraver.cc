/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2012--2022 Mike Solomon <mike@mikesolomon.org>

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
#include "side-position-interface.hh"
#include "pointer-group-interface.hh"
#include "fingering-column.hh"
#include "international.hh"
#include "item.hh"

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
  void acknowledge_finger (Grob_info_t<Item>);
  void process_acknowledged ();
  void stop_translation_timestep ();
};

Fingering_column_engraver::Fingering_column_engraver (Context *c)
  : Engraver (c)
{
  for (const auto d : {LEFT, RIGHT})
    fingering_columns_[d] = 0;
}

void
Fingering_column_engraver::stop_translation_timestep ()
{
  for (vsize i = 0; i < possibles_.size (); i++)
    {
      Grob *item = possibles_[i];
      if (!Item::is_non_musical (item)
          && Side_position_interface::is_on_x_axis (item))
        {
          Direction d = from_scm (get_property (item, "direction"), CENTER);
          if (d)
            scripts_[d].push_back (item);
          else
            possibles_[i]->warning (
              _ ("Cannot add a fingering without a direction."));
        }
    }

  for (const auto d : {LEFT, RIGHT})
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
Fingering_column_engraver::acknowledge_finger (Grob_info_t<Item> info)
{
  possibles_.push_back (info.grob ());
}

void
Fingering_column_engraver::process_acknowledged ()
{
  for (const auto d : {LEFT, RIGHT})
    {
      if (possibles_.size () > 1 && !fingering_columns_[d])
        fingering_columns_[d] = make_item ("FingeringColumn", SCM_EOL);
    }
}

void
Fingering_column_engraver::boot ()
{
  ADD_ACKNOWLEDGER (finger);
}

ADD_TRANSLATOR (Fingering_column_engraver,
                /* doc */
                R"(
Find potentially colliding scripts and put them into a @code{FingeringColumn}
object; that will fix the collisions.
                )",

                /* create */
                R"(
FingeringColumn
                )",

                /* read */
                R"(

                )",

                /* write */
                R"(

                )");
