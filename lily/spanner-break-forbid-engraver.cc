/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2007--2010 Han-Wen Nienhuys <hanwen@lilypond.org>

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
#include "international.hh"
#include "spanner.hh"
#include "stream-event.hh"
#include "warn.hh"
#include "context.hh"

#include "translator.icc"

class Spanner_break_forbid_engraver : public Engraver
{
  TRANSLATOR_DECLARATIONS (Spanner_break_forbid_engraver);
  vector<Spanner*> running_spanners_;
protected:
  DECLARE_ACKNOWLEDGER (unbreakable_spanner);
  DECLARE_END_ACKNOWLEDGER (unbreakable_spanner);

  void process_music ();
};


void
Spanner_break_forbid_engraver::process_music ()
{
  if (running_spanners_.size ())
    {
      context ()->get_score_context ()->set_property ("forbidBreak", SCM_BOOL_T);
    }
}

void
Spanner_break_forbid_engraver::acknowledge_end_unbreakable_spanner (Grob_info gi)
{
  vector<Spanner*>::iterator i = find (running_spanners_.begin (), running_spanners_.end (),
				       gi.spanner ());
  if (i != running_spanners_.end ())
    running_spanners_.erase (i);
}

void
Spanner_break_forbid_engraver::acknowledge_unbreakable_spanner (Grob_info gi)
{
  if (!to_boolean (gi.grob ()->get_property ("breakable")))
    running_spanners_.push_back (gi.spanner ());
}

Spanner_break_forbid_engraver::Spanner_break_forbid_engraver ()
{
}


ADD_END_ACKNOWLEDGER (Spanner_break_forbid_engraver, unbreakable_spanner);
ADD_ACKNOWLEDGER (Spanner_break_forbid_engraver, unbreakable_spanner);
ADD_TRANSLATOR (Spanner_break_forbid_engraver,
		/* doc */
		"Forbid breaks in certain spanners.",

		/* create */
		"",

		/* read */
		"",

		/* write */
		""
		);
