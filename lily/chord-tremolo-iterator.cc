/*
  chord-tremolo-iterator.cc -- implement Chord_tremolo_iterator

  source file of the GNU LilyPond music typesetter

  (c) 2000--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
  		 Erik Sandberg <mandolaerik@gmail.com>
*/

#include "chord-tremolo-iterator.hh"

#include "input.hh"
#include "international.hh"
#include "misc.hh"
#include "repeated-music.hh"

Chord_tremolo_iterator::Chord_tremolo_iterator ()
{
}

SCM
Chord_tremolo_iterator::get_music_list () const
{
  Music *mus = get_music ();
  Input *origin = mus->origin ();
  Moment l = mus->get_length ();
  Music *body = Repeated_music::body (mus);
  bool body_is_sequential = body->is_mus_type ("sequential-music");

  int elt_count = body_is_sequential ? scm_ilength (body->get_property ("elements")) : 1;

  if (body_is_sequential &&
      (elt_count != 2
       && elt_count != 1))
    mus->origin ()->warning (_f ("expect 2 elements for chord tremolo, found %d", elt_count));

  if (elt_count <= 0)
    elt_count = 1;
      
  Rational total_dur = l.main_part_;
  Rational note_dur = total_dur / Rational (elt_count * Repeated_music::repeat_count (mus));

  SCM tremolo_type = scm_int2num (note_dur.den ());
  int expected_beaming_ = max (0, (intlog2 (total_dur.den ()) - intlog2 (total_dur.num () + 1) - 1));

  if (elt_count == 1)
    {
      Music *ev = make_music_by_name (ly_symbol2scm ("TremoloEvent"));
      ev->set_spot (*origin);
      ev->set_property ("tremolo-type", tremolo_type);
      return scm_list_2 (ev->unprotect (), body->self_scm ());
    }
  else
    { 
      SCM tremolo_symbol = ly_symbol2scm ("TremoloSpanEvent");
      SCM start_event_scm = scm_call_2 (ly_lily_module_constant ("make-span-event"), tremolo_symbol, scm_from_int (START));
      unsmob_music (start_event_scm)->set_spot (*origin);
      SCM stop_event_scm = scm_call_2 (ly_lily_module_constant ("make-span-event"), tremolo_symbol, scm_from_int (STOP));

      Music *start_event = unsmob_music (start_event_scm);
      Music *stop_event = unsmob_music (stop_event_scm);
      start_event->set_spot (*origin);
      stop_event->set_spot (*origin);
      start_event->set_property ("tremolo-type", tremolo_type);
      start_event->set_property ("expected-beam-count", scm_int2num (expected_beaming_));

      return scm_list_3 (start_event_scm, body->self_scm (), stop_event_scm);
    }
}

IMPLEMENT_CTOR_CALLBACK (Chord_tremolo_iterator);
