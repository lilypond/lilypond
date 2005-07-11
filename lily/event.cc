/*
  event.cc -- implement Event

  source file of the GNU LilyPond music typesetter

  (c) 1996--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "event.hh"
#include "warn.hh"

MAKE_SCHEME_CALLBACK (Event, length_callback, 1);
SCM
Event::length_callback (SCM m)
{
  Music *me = unsmob_music (m);
  Duration *d = unsmob_duration (me->get_property ("duration"));

  Moment mom;
  if (d)
    {
      mom = d->get_length ();
    }
  return mom.smobbed_copy ();
}

Event::Event (SCM i)
  : Music (i)
{
  if (!ly_is_procedure (length_callback_))
    {
      length_callback_ = length_callback_proc;
    }
}

ADD_MUSIC (Event);

