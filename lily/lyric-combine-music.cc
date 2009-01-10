/*
  lyric-combine-music.cc -- implement Lyric_combine_music

  source file of the GNU LilyPond music typesetter

  (c) 1999--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "music.hh"

struct Lyric_combine_music
{
public:
  DECLARE_SCHEME_CALLBACK (length_callback, (SCM));
};

MAKE_SCHEME_CALLBACK (Lyric_combine_music, length_callback, 1);
SCM
Lyric_combine_music::length_callback (SCM m)
{
  Music *me = unsmob_music (m);
  Music *melody = unsmob_music (scm_car (me->get_property ("elements")));
  return melody->get_length ().smobbed_copy ();
}
