/*
  time-scaled-music-iterator.cc -- implement Time_scaled_music_iterator

  source file of the GNU LilyPond music typesetter

  (c) 1998--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>,
                 Erik Sandberg <mandolaerik@gmail.com>
*/

#include "time-scaled-music-iterator.hh"

#include "context.hh"
#include "input.hh"
#include "international.hh"
#include "music.hh"

Time_scaled_music_iterator::Time_scaled_music_iterator ()
{
}

SCM
Time_scaled_music_iterator::get_music_list () const
{
  Music *mus = get_music ();
  Input *origin = mus->origin ();
  Music *child = unsmob_music (mus->get_property ("element"));

  /* Create tuplet start/stop span events before/after the music */
  SCM tuplet_symbol = ly_symbol2scm ("TupletEvent");
  SCM start_event = scm_call_2 (ly_lily_module_constant ("make-span-event"), tuplet_symbol, scm_from_int (START));
  Music *start = unsmob_music (start_event);
  start->set_spot (*origin);
  start->set_property ("numerator", mus->get_property ("numerator"));
  start->set_property ("denominator", mus->get_property ("denominator"));
  start_event = scm_call_1 (ly_lily_module_constant ("make-event-chord"), scm_list_1 (start_event));
  unsmob_music (start_event)->set_spot (*origin);

  SCM stop_event = scm_call_2 (ly_lily_module_constant ("make-span-event"), tuplet_symbol, scm_from_int (STOP));
  unsmob_music (stop_event)->set_spot (*origin);
  stop_event = scm_call_1 (ly_lily_module_constant ("make-event-chord"), scm_list_1 (stop_event));
  unsmob_music (stop_event)->set_spot (*origin);

  return scm_list_3 (start_event, child->self_scm (), stop_event);
}

IMPLEMENT_CTOR_CALLBACK (Time_scaled_music_iterator);
