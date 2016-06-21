/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2015 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "context.hh"
#include "slur-proto-engraver.hh"

#include "translator.icc"

class Slur_engraver : public Slur_proto_engraver
{
  virtual void set_melisma (bool);

public:
  SCM event_symbol ();
  TRANSLATOR_DECLARATIONS (Slur_engraver);
  TRANSLATOR_INHERIT (Slur_proto_engraver);
};

Slur_engraver::Slur_engraver () :
  Slur_proto_engraver ("doubleSlurs", "Slur", "slur", "slur-event")
{
}

SCM
Slur_engraver::event_symbol ()
{
  // Need a string constant for memoization
  return ly_symbol2scm ("slur-event");
}

void
Slur_engraver::set_melisma (bool m)
{
  context ()->set_property ("slurMelismaBusy", ly_bool2scm (m));
}

void
Slur_engraver::boot ()
{
  ADD_LISTENER (Slur_engraver, slur);
  ADD_LISTENER (Slur_engraver, note);
  ADD_ACKNOWLEDGER (Slur_engraver, inline_accidental);
  ADD_ACKNOWLEDGER (Slur_engraver, fingering);
  ADD_ACKNOWLEDGER (Slur_engraver, note_column);
  ADD_ACKNOWLEDGER (Slur_engraver, script);
  ADD_ACKNOWLEDGER (Slur_engraver, text_script);
  ADD_ACKNOWLEDGER (Slur_engraver, dots);
  ADD_END_ACKNOWLEDGER (Slur_engraver, tie);
  ADD_ACKNOWLEDGER (Slur_engraver, tuplet_number);
}

ADD_TRANSLATOR (Slur_engraver,
                /* doc */
                "Build slur grobs from slur events.",

                /* create */
                "Slur ",

                /* read */
                "slurMelismaBusy "
                "doubleSlurs ",

                /* write */
                ""
               );
