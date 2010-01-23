/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

/**
   Find potentially colliding scripts, and put them in a
   Script_column, that will fix the collisions.  */
class Script_column_engraver : public Engraver
{
  Grob *script_column_;
  vector<Grob*> scripts_;

public:
  TRANSLATOR_DECLARATIONS (Script_column_engraver);
protected:
  DECLARE_ACKNOWLEDGER (side_position);
  void process_acknowledged ();
  void stop_translation_timestep ();
};

Script_column_engraver::Script_column_engraver ()
{
  script_column_ = 0;
}

void
Script_column_engraver::stop_translation_timestep ()
{
  if (script_column_)
    {
      for (vsize i = 0; i < scripts_.size (); i++)
	if (Side_position_interface::get_axis (scripts_[i]) == Y_AXIS)
	  Script_column::add_side_positioned (script_column_, scripts_[i]);
    }

  script_column_ = 0;
  scripts_.clear ();
}

void
Script_column_engraver::acknowledge_side_position (Grob_info inf)
{
  Item *thing = dynamic_cast<Item *> (inf.grob ());
  if (thing)
    {
      if (!Item::is_non_musical (thing))
	scripts_.push_back (thing);
    }
}

void
Script_column_engraver::process_acknowledged ()
{
  if (!script_column_ && scripts_.size () > 1)
    script_column_ = make_item ("ScriptColumn", SCM_EOL);
}

ADD_ACKNOWLEDGER (Script_column_engraver, side_position);
ADD_TRANSLATOR (Script_column_engraver,
		/* doc */
		"Find potentially colliding scripts and put them into a"
		" @code{ScriptColumn} object; that will fix the collisions.",

		/* create */
		"ScriptColumn ",

		/* read */
		"",

		/* write */
		""
		);
