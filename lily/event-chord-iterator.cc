/*
  event-chord-iterator.cc -- implement Event_chord_iterator

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "context.hh"
#include "warn.hh"
#include "event-chord-iterator.hh"
#include "music-list.hh"
#include "event.hh"

Event_chord_iterator::Event_chord_iterator ()
{
}

Context *
Event_chord_iterator::get_req_translator ()
{
  assert (get_outlet ());
  if (get_outlet ()->is_bottom_context ())
    return get_outlet ();

  set_translator (get_outlet ()->get_default_interpreter ());
  return get_outlet ();
}

void
Event_chord_iterator::construct_children ()
{
  Simple_music_iterator::construct_children ();
  get_req_translator ();
}

Event_chord*
Event_chord_iterator::get_elt () const
{
  return (Event_chord*) get_music ();
}


void
Event_chord_iterator::process (Moment m)
{
  if (last_processed_mom_ < Moment (0))
    {
      for (SCM s = dynamic_cast<Music_sequence *> (get_music ())->music_list ();
	   ly_c_pair_p (s);  s = ly_cdr (s))
	{
	  Music *mus = unsmob_music (ly_car (s));

	  bool gotcha = try_music (mus);
	  if (!gotcha)
	    mus->origin ()->warning (_f ("Junking event: `%s'", mus->name ()));
	}
    }
  Simple_music_iterator::process (m);
}

IMPLEMENT_CTOR_CALLBACK (Event_chord_iterator);
