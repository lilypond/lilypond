/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "context.hh"
#include "grob.hh"

class Default_bar_line_engraver : public Engraver
{
protected:
  void start_translation_timestep ();
  void stop_translation_timestep ();

public:
  TRANSLATOR_DECLARATIONS (Default_bar_line_engraver);
};

#include "translator.icc"

void
Default_bar_line_engraver::boot ()
{

}

ADD_TRANSLATOR (Default_bar_line_engraver,
                /* doc */
                "This engraver determines what kind of automatic bar lines"
                " should be produced, and sets @code{whichBar} accordingly."
                "  It should be at the same level as @ref{Timing_translator}.",

                /* create */
                "",

                /* read */
                "automaticBars "
                "barAlways "
                "defaultBarType "
                "measureStartNow "
                "whichBar ",

                /* write */
                "whichBar "
               );

Default_bar_line_engraver::Default_bar_line_engraver (Context *c)
  : Engraver (c)
{
}

void
Default_bar_line_engraver::start_translation_timestep ()
{
  if (from_scm<bool> (get_property (this, "measureStartNow"))
      || from_scm<bool> (get_property (this, "barAlways")))
    {
      if (from_scm<bool> (get_property (this, "automaticBars")))
        {
          /* should this work, or be junked?  See input/bugs/no-bars.ly */
          SCM which = get_property (this, "defaultBarType");
          set_property (context (), "whichBar", which);
        }
    }
}

void
Default_bar_line_engraver::stop_translation_timestep ()
{
  set_property (context (), "whichBar", SCM_EOL);
}
