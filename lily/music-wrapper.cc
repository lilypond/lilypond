/*
  music-wrapper.cc -- implement Music_wrapper

  source file of the GNU LilyPond music typesetter

  (c) 1998--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "music-wrapper.hh"

#include "music.hh"

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

