/* 
  music-head.hh -- declare Music_head
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2004 Han-Wen Nienhuys <hanwen@xs4all.nl>
  
*/

#ifndef MUSIC_HEAD_HH
#define MUSIC_HEAD_HH

#include "lily-guile.hh"

SCM ly_make_music_head (SCM, SCM);
SCM get_music_head_transform (SCM);
bool is_music_head (SCM);

#endif /* MUSIC_HEAD_HH */

