/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2011--2022 Mike Solomon <mike@mikesolomon.org>

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

#include "music.hh"
#include "stream-event.hh"
#include "international.hh"
#include "item.hh"
#include "pointer-group-interface.hh"
#include "spanner.hh"
#include "system.hh"

#include "translator.icc"

class Footnote_engraver : public Engraver
{
  TRANSLATOR_DECLARATIONS (Footnote_engraver);

  void acknowledge_grob (Grob_info) override;
};

Footnote_engraver::Footnote_engraver (Context *c)
  : Engraver (c)
{
}

void
Footnote_engraver::acknowledge_grob (Grob_info info)
{
  Music *mus = unsmob<Music> (get_property (info.grob (), "footnote-music"));

  if (mus)
    {
      if (!mus->is_mus_type ("footnote-event"))
        {
          mus->programming_error (_ ("Must be footnote-event."));
          return;
        }

      Engraver *eng = info.origin_engraver ();
      eng->make_sticky ("Footnote", info.grob (),
                        mus->to_event ()->unprotect ());

      // This grob has exhausted its footnote
      set_property (info.grob (), "footnote-music", SCM_EOL);
    }
}

void
Footnote_engraver::boot ()
{
  ADD_ACKNOWLEDGER (grob);
}

ADD_TRANSLATOR (Footnote_engraver,
                /* doc */
                R"(
Create footnote texts.
                )",

                /* create */
                R"(
Footnote
                )",

                /*read*/
                R"(

                )",

                /*write*/
                R"(

                )");
