/*
  music-wrapper.cc -- implement Music_wrapper

  source file of the GNU LilyPond music typesetter

  (c) 1998--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "music-wrapper.hh"

Music_wrapper::Music_wrapper (SCM x)
  : Music (x)
{
  if (!ly_c_procedure_p (length_callback_))
    length_callback_ = length_callback_proc;
  if (!ly_c_procedure_p (start_callback_))
    start_callback_ = start_callback_proc;
}

MAKE_SCHEME_CALLBACK (Music_wrapper, start_callback, 1);
SCM
Music_wrapper::start_callback (SCM m)
{
  Music *me = unsmob_music (m);
  Music *elt = unsmob_music (me->get_property ("element"));
  return elt->start_mom ().smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Music_wrapper, length_callback, 1);
SCM
Music_wrapper::length_callback (SCM m)
{
  Music *me = unsmob_music (m);
  Music *elt = unsmob_music (me->get_property ("element"));
  return elt->get_length ().smobbed_copy ();
}

Music *
Music_wrapper::element () const
{
  return unsmob_music (get_property ("element"));
}

ADD_MUSIC (Music_wrapper);

