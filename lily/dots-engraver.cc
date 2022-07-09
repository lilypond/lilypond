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
#include "duration.hh"
#include "item.hh"
#include "rhythmic-head.hh"
#include "stream-event.hh"

#include "translator.icc"

class Dots_engraver : public Engraver
{
  void acknowledge_rhythmic_head (Grob_info);
  TRANSLATOR_DECLARATIONS (Dots_engraver);
};

Dots_engraver::Dots_engraver (Context *c)
  : Engraver (c)
{
}

void
Dots_engraver::acknowledge_rhythmic_head (Grob_info gi)
{
  Stream_event *cause = gi.event_cause ();
  if (!cause)
    return;

  Grob *note = gi.grob ();
  if (unsmob<Grob> (get_object (note, "dot")))
    return;

  Duration *dur = unsmob<Duration> (get_property (cause, "duration"));
  if (dur && dur->dot_count ())
    {
      Item *d = make_item ("Dots", note->self_scm ());
      Rhythmic_head::set_dots (note, d);

      d->set_y_parent (note);
    }
}

void
Dots_engraver::boot ()
{
  ADD_ACKNOWLEDGER (rhythmic_head);
}

ADD_TRANSLATOR (Dots_engraver,
                R"(
Create @ref{Dots} objects for @ref{rhythmic-head-interface}s.
                )",

                /* create */
                R"(
Dots
                )",

                /* read */
                R"(

                )",

                /* write */
                R"(

                )");
