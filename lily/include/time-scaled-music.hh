/*   
  compressed-music.hh -- declare Time_scaled_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef Time_scaled_music_HH
#define Time_scaled_music_HH

#include "music-wrapper.hh"
/**
   Tempo expansion or compression.
 */
class Time_scaled_music : public Music_wrapper
{
public:
  Time_scaled_music (SCM);
  
  VIRTUAL_COPY_CONS(Music);
};

#endif /* Time_scaled_music_HH */

