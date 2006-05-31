/*
  event-chord-iterator.cc -- implement Event_chord_iterator

  source file of the GNU LilyPond music typesetter

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "event-chord-iterator.hh"

#include "context.hh"
#include "duration.hh"
#include "input.hh"
#include "international.hh"
#include "music.hh"
#include "pitch.hh"
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
    }
  Simple_music_iterator::process (m);
}

IMPLEMENT_CTOR_CALLBACK (Event_chord_iterator);
