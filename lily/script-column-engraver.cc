/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "script-column.hh"
#include "side-position-interface.hh"
#include "item.hh"

#include "translator.icc"

#include <vector>

/**
   Find potentially colliding scripts, and put them in a
   Script_column, that will fix the collisions.  */
class Script_column_engraver : public Engraver
{
  Item *script_column_ = nullptr;
  std::vector<Item *> scripts_;

public:
  TRANSLATOR_DECLARATIONS (Script_column_engraver);

protected:
  void acknowledge_side_position (Grob_info);
  void process_acknowledged ();
  void stop_translation_timestep ();
};

Script_column_engraver::Script_column_engraver (Context *c)
  : Engraver (c)
{
}

void
Script_column_engraver::stop_translation_timestep ()
{
  if (script_column_)
    {
      for (auto *scr : scripts_)
        {
          if (Side_position_interface::is_on_y_axis (scr))
            Script_column::add_side_positioned (script_column_, scr);
        }

      script_column_ = nullptr;
    }

  scripts_.clear ();
}

void
Script_column_engraver::acknowledge_side_position (Grob_info inf)
{
  if (auto *it = dynamic_cast<Item *> (inf.grob ()))
    {
      if (!Item::is_non_musical (it))
        scripts_.push_back (it);
    }
}

void
Script_column_engraver::process_acknowledged ()
{
  if (!script_column_ && scripts_.size () > 1)
    script_column_ = make_item ("ScriptColumn", to_scm (scripts_[0]));
}

void
Script_column_engraver::boot ()
{
  ADD_ACKNOWLEDGER (side_position);
}

ADD_TRANSLATOR (Script_column_engraver,
                /* doc */
                R"(
Find potentially colliding scripts and put them into a @code{ScriptColumn}
object; that will fix the collisions.
                )",

                /* create */
                R"(
ScriptColumn
                )",

                /* read */
                R"(

                )",

                /* write */
                R"(

                )");
