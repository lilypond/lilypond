/*
  event.cc -- implement Event

  source file of the GNU LilyPond music typesetter

  (c) 1996--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "event.hh"
#include "warn.hh"

MAKE_SCHEME_CALLBACK(Event,length_callback,1);
SCM
Event::length_callback (SCM m)
{
  Music* me = unsmob_music (m);
  Duration *d = unsmob_duration (me->get_property ("duration"));

  Moment mom;
  if (d)
    {
      mom = d->get_length ();
    }
  return mom.smobbed_copy();
}
  
Event::Event (SCM i)
  : Music (i)
{
  if (!ly_c_procedure_p (length_callback_))
    {
      length_callback_ = length_callback_proc;
    }
}

ADD_MUSIC (Event);

Key_change_ev::Key_change_ev (SCM x)
  : Event (x)
{
}
void
Key_change_ev::transpose (Pitch p)
{
  SCM pa = get_property ("pitch-alist");
  set_property ("pitch-alist", ly_transpose_key_alist (pa, p.smobbed_copy ()));

  Event::transpose (p);
}

ADD_MUSIC (Key_change_ev);
