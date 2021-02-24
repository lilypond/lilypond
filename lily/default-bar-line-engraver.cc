/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2021 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
                "measureStartNow ",

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
  // Gould writes that "[a] thin double barline ...  marks the written end of
  // the music when this is not the end of the piece" (Behind Bars, p.240).
  // Although it would be fairly easy to implement that as a default, we avoid
  // it on the grounds that the input is possibly not a finished work, and it
  // is easy for the user to add a \bar command at the end when it is.

  SCM wb = SCM_EOL;

  if (from_scm<bool> (get_property (this, "measureStartNow"))
      || from_scm<bool> (get_property (this, "barAlways")))
    {
      if (from_scm<bool> (get_property (this, "automaticBars")))
        {
          wb = get_property (this, "defaultBarType");
        }
    }

  // We set whichBar at each timestep because the user manuals suggest using
  // \set Timing.whichBar = ... rather than \once \set Timing.whichBar = ...,
  // so we might need to erase the user's value from the previous timestep.  We
  // don't do this in stop_translation_timestep() because other translators
  // might still want to read whichBar during stop_translation_timestep().
  //
  // It might be nice to set up a convert-ly rule to change user code to use
  // \once \set ... and then change this to the internal equivalent of \once
  // \set too.
  set_property (context (), "whichBar", wb);
}
