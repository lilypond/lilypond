/*   
  auto-change-music.hh -- declare Auto_change_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef AUTO_CHANGE_MUSIC_HH
#define AUTO_CHANGE_MUSIC_HH

#include "music-wrapper.hh"

class Auto_change_music : public Music_wrapper
{
public:
  Auto_change_music (String what, Music *);
  String what_str_;
};


#endif /* AUTO_CHANGE_MUSIC_HH */

