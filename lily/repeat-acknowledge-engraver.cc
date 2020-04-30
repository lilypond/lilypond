/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

using std::string;

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
  void initialize () override;
};

void
Repeat_acknowledge_engraver::initialize ()
{
  set_property (context (), "repeatCommands", SCM_EOL);
}

Repeat_acknowledge_engraver::Repeat_acknowledge_engraver (Context *c)
  : Engraver (c)
{
}

void
Repeat_acknowledge_engraver::start_translation_timestep ()
{
  SCM rc;
  Context *tr = context ()->where_defined (ly_symbol2scm ("repeatCommands"), &rc);
  if (!tr)
    tr = context ();

  set_property (tr, "repeatCommands", SCM_EOL);
}

void
Repeat_acknowledge_engraver::process_music ()
{
  /*
    At the start of a piece, we don't print any repeat bars.
  */
  if (!now_mom ().main_part_)
    return;

  SCM cs = get_property (this, "repeatCommands");

  string s = "";
  bool start = false;
  bool end = false;
  bool segno = false;
  bool volta_found = false;
  while (scm_is_pair (cs))
    {
      SCM command = scm_car (cs);
      if (scm_is_eq (command, ly_symbol2scm ("start-repeat")))
        start = true;
      else if (scm_is_eq (command, ly_symbol2scm ("end-repeat")))
        end = true;
      else if (scm_is_eq (command, ly_symbol2scm ("segno-display")))
        segno = true;
      else if (scm_is_pair (command)
               && scm_is_eq (scm_car (command), ly_symbol2scm ("volta")))
        volta_found = true;
      cs = scm_cdr (cs);
    }

  /*
    Select which bar type to set
  */
  if (segno)
    if (start)
      if (end) // { segno, start, end }
        s = robust_scm2string (get_property (this, "doubleRepeatSegnoType"), ":|.S.|:");
      else // { segno, start }
        s = robust_scm2string (get_property (this, "startRepeatSegnoType"), "S.|:");
    else if (end) // { segno, end }
      s = robust_scm2string (get_property (this, "endRepeatSegnoType"), ":|.S");
    else // { segno }
      s = robust_scm2string (get_property (this, "segnoType"), "S");
  else if (start)
    if (end) // { start, end }
      s = robust_scm2string (get_property (this, "doubleRepeatType"), ":..:");
    else // { start }
      s = robust_scm2string (get_property (this, "startRepeatType"), ".|:");
  else if (end) // { end }
    s = robust_scm2string (get_property (this, "endRepeatType"), ":|.");

  /*
    TODO: line breaks might be allowed if we set whichBar to "".
  */

  /*
    We only set the barline if we wouldn't overwrite a previously set
    barline.
  */
  SCM wb = get_property (this, "whichBar");
  SCM db = get_property (this, "defaultBarType");
  if (!scm_is_string (wb) || ly_is_equal (db, wb))
    {
      if (s != "" || (volta_found && !scm_is_string (wb)))
        set_property (context (), "whichBar", ly_string2scm (s));
    }
}

void
Repeat_acknowledge_engraver::boot ()
{

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
                "startRepeatType "
                "endRepeatType "
                "doubleRepeatSegnoType "
                "startRepeatSegnoType "
                "endRepeatSegnoType "
                "segnoType "
                "repeatCommands "
                "whichBar ",

                /* write */
                ""
               );
