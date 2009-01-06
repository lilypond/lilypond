/*
  music-head.hh -- declare music_function

  source file of the GNU LilyPond music typesetter

  (c) 2004--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef MUSIC_FUNCTION_HH
#define MUSIC_FUNCTION_HH

#include "lily-guile.hh"

SCM ly_make_music_function (SCM, SCM);
SCM make_music_function (SCM, SCM);

SCM get_music_function_transform (SCM);
bool is_music_function (SCM);

#endif /* MUSIC_FUNCTION_HH */

