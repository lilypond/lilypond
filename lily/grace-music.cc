/*
  grace-music.cc -- implement Grace_music

  source file of the GNU LilyPond music typesetter

  (c) 1999--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "music.hh"
#include "music-wrapper.hh"

class Grace_music
{
public:
  DECLARE_SCHEME_CALLBACK (start_callback, (SCM));
};

MAKE_SCHEME_CALLBACK (Grace_music, start_callback, 1);
SCM
Grace_music::start_callback (SCM m)
{
  Moment *l = unsmob_moment (Music_wrapper::length_callback (m));
  Moment gl;
  gl.grace_part_ = -(l->main_part_ + l->grace_part_);
  return gl.smobbed_copy ();
}
