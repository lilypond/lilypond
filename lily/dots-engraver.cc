/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2010 Han-Wen Nienhuys <hanwen@lilypond.org>

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
  DECLARE_ACKNOWLEDGER (rhythmic_head);
  TRANSLATOR_DECLARATIONS (Dots_engraver);
};

Dots_engraver::Dots_engraver ()
{
}

void
Dots_engraver::acknowledge_rhythmic_head (Grob_info gi)
{
  Stream_event *cause = gi.event_cause ();
  if (!cause)
    return;

  Grob *note = gi.grob ();
  if (unsmob_grob (note->get_object ("dot")))
    return;
  
  Duration *dur = unsmob_duration (cause->get_property ("duration"));
  if (dur && dur->dot_count ())
    {
      Item *d = make_item ("Dots", note->self_scm ());
      Rhythmic_head::set_dots (note, d);

      d->set_parent (note, Y_AXIS);
    }
}


ADD_ACKNOWLEDGER (Dots_engraver, rhythmic_head);

ADD_TRANSLATOR (Dots_engraver,
		"Create @ref{Dots} objects for"
		" @ref{rhythmic-head-interface}s.",

		/* create */
		"Dots ",

		/* read */
		"",
	       
		/* write */
		""
		);
