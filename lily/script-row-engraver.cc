/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2022 Han-Wen Nienhuys <hanwen@lilypond.org>


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

#include "accidental-placement.hh"
#include "item.hh"
#include "script-column.hh"
#include "side-position-interface.hh"

#include "translator.icc"

#include <vector>

/**
   Find potentially colliding scripts, and put them in a
   Script_row
*/
class Script_row_engraver : public Engraver
{
  Item *script_row_ = nullptr;
  std::vector<Item *> scripts_;

public:
  TRANSLATOR_DECLARATIONS (Script_row_engraver);

protected:
  void acknowledge_accidental_placement (Grob_info_t<Item>);
  void acknowledge_side_position (Grob_info);
  void process_acknowledged ();
  void stop_translation_timestep ();
};

Script_row_engraver::Script_row_engraver (Context *c)
  : Engraver (c)
{
}

void
Script_row_engraver::stop_translation_timestep ()
{
  if (script_row_)
    {
      for (auto *scr : scripts_)
        {
          if (has_interface<Accidental_placement> (scr)
              || Side_position_interface::is_on_x_axis (scr))
            {
              Script_column::add_side_positioned (script_row_, scr);
            }
        }

      script_row_ = nullptr;
    }

  scripts_.clear ();
}

void
Script_row_engraver::acknowledge_side_position (Grob_info inf)
{
  if (auto *it = dynamic_cast<Item *> (inf.grob ()))
    {
      if (!Item::is_non_musical (it))
        scripts_.push_back (it);
    }
}

void
Script_row_engraver::acknowledge_accidental_placement (Grob_info_t<Item> inf)
{
  scripts_.push_back (inf.grob ());
}

void
Script_row_engraver::process_acknowledged ()
{
  if (!script_row_ && scripts_.size () > 1)
    script_row_ = make_item ("ScriptRow", to_scm (scripts_[0]));
}

void
Script_row_engraver::boot ()
{
  ADD_ACKNOWLEDGER (accidental_placement);
  ADD_ACKNOWLEDGER (side_position);
}

ADD_TRANSLATOR (Script_row_engraver,
                /* doc */
                R"(
Determine order in horizontal side position elements.
                )",

                /* create */
                R"(
ScriptRow
                )",

                /* read */
                R"(

                )",

                /* write */
                R"(

                )");
