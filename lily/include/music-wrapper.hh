/*
  music-wrapper.hh -- declare Music_wrapper

  source file of the GNU LilyPond music typesetter

  (c) 1998--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef MUSIC_WRAPPER_HH
#define MUSIC_WRAPPER_HH

#include "lily-guile.hh"

struct Music_wrapper
{
public:
  DECLARE_SCHEME_CALLBACK (length_callback, (SCM));
  DECLARE_SCHEME_CALLBACK (start_callback, (SCM));
};

#endif /* MUSIC_WRAPPER_HH */

