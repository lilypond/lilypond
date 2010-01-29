/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "event-chord-iterator.hh"

#include "context.hh"
#include "dispatcher.hh"
#include "duration.hh"
#include "input.hh"
#include "international.hh"
#include "music.hh"
#include "warn.hh"

Event_chord_iterator::Event_chord_iterator ()
{
}

void
Event_chord_iterator::construct_children ()
{
  Simple_music_iterator::construct_children ();
  descend_to_bottom_context ();
}

void
Event_chord_iterator::process (Moment m)
{
  if (last_processed_mom_ < Moment (0))
    {
      for (SCM s = get_music ()->get_property ("elements");
	   scm_is_pair (s); s = scm_cdr (s))
	{
	  Music *mus = unsmob_music (scm_car (s));
	  report_event (mus);
	}
      for (SCM s = get_music ()->get_property ("events");
	   scm_is_pair (s); s = scm_cdr (s))
	{
	  Stream_event *ev = unsmob_stream_event (scm_car (s));
	  get_outlet ()->event_source ()->broadcast (ev);
	}
    }
  Simple_music_iterator::process (m);
}

IMPLEMENT_CTOR_CALLBACK (Event_chord_iterator);
