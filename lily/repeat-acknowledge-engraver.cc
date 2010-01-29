/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "translator-group.hh"
#include "context.hh"
#include "repeated-music.hh"

#include "translator.icc"

/*
  Objective:

  -- set and reset repeatCommands, so Unfolded_repeat_iterator knows
  where to set variables.

  -- collect information passed by Unfolded_repeat_iterator for
  Bar_engraver: writes whichBar property. (TODO: check for
  interactions with timing engraver.)
*/
class Repeat_acknowledge_engraver : public Engraver
{
public:

  TRANSLATOR_DECLARATIONS (Repeat_acknowledge_engraver);
protected:
  void start_translation_timestep ();
  void process_music ();
  virtual void initialize ();
};

void
Repeat_acknowledge_engraver::initialize ()
{
  context ()->set_property ("repeatCommands", SCM_EOL);
}

Repeat_acknowledge_engraver::Repeat_acknowledge_engraver ()
{
}

void
Repeat_acknowledge_engraver::start_translation_timestep ()
{
  SCM rc;
  Context *tr = context ()->where_defined (ly_symbol2scm ("repeatCommands"), &rc);
  if (!tr)
    tr = context ();

  tr->set_property ("repeatCommands", SCM_EOL);
}

void
Repeat_acknowledge_engraver::process_music ()
{
  /*
    At the start of a piece, we don't print any repeat bars.
  */
  if (!now_mom ().main_part_)
    return;

  SCM cs = get_property ("repeatCommands");

  string s = "";
  bool start = false;
  bool end = false;
  bool volta_found = false;
  while (scm_is_pair (cs))
    {
      SCM command = scm_car (cs);
      if (command == ly_symbol2scm ("start-repeat"))
	start = true;
      else if (command == ly_symbol2scm ("end-repeat"))
	end = true;
      else if (scm_is_pair (command) && scm_car (command) == ly_symbol2scm ("volta"))
	volta_found = true;
      cs = scm_cdr (cs);
    }

  if (start && end)
    s = robust_scm2string (get_property ("doubleRepeatType"), ":|:");
  else if (start)
    s = "|:";
  else if (end)
    s = ":|";

  /*
    TODO: line breaks might be allowed if we set whichBar to "".
  */

  /*
    We only set the barline if we wouldn't overwrite a previously set
    barline.
  */
  SCM wb = get_property ("whichBar");
  SCM db = get_property ("defaultBarType");
  if (!scm_is_string (wb) || ly_is_equal (db, wb))
    {
      if (s != "" || (volta_found && !scm_is_string (wb)))
	context ()->set_property ("whichBar", ly_string2scm (s));
    }
}

ADD_TRANSLATOR (Repeat_acknowledge_engraver,
		/* doc */
		"Acknowledge repeated music, and convert the contents of"
		" @code{repeatCommands} into an appropriate setting for"
		" @code{whichBar}.",

		/* create */
		"",

		/* read */
		"doubleRepeatType "
		"repeatCommands "
		"whichBar ",

		/* write */
		""
		);
