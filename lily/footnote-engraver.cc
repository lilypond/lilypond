/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2011--2021 Mike Solomon <mike@mikesolomon.org>

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

  void footnotify (Grob *, SCM);
};

Footnote_engraver::Footnote_engraver (Context *c)
  : Engraver (c)
{
}

void
Footnote_engraver::footnotify (Grob *g, SCM cause)
{
  Grob *footnote = nullptr;
  if (dynamic_cast<Spanner *> (g))
    {
      footnote = make_spanner ("FootnoteSpanner", cause);
      // Delegate ending the footnote to the Spanner_tracking_engraver.
      set_object (footnote, "sticky-host", to_scm (g));
    }
  else
    {
      footnote = make_item ("FootnoteItem", cause);
      footnote->set_x_parent (g);
    }
  footnote->set_y_parent (g);
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

      footnotify (info.grob (), mus->to_event ()->unprotect ());

      // This grob has exhausted its footnote
      set_property (info.grob (), "footnote-music", SCM_EOL);
    }
}

void
Footnote_engraver::boot ()
{
  ADD_ACKNOWLEDGER (Footnote_engraver, grob);
}

ADD_TRANSLATOR (Footnote_engraver,
                /* doc */
                "Create footnote texts.",

                /* create */
                "FootnoteItem "
                "FootnoteSpanner ",

                /*read*/
                "",

                /*write*/
                ""
               );
