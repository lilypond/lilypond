/*   
  compressed-music.hh -- declare Time_scaled_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
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
  int num_i_;
  int den_i_;

  Time_scaled_music (int, int, Music *);
  
  VIRTUAL_COPY_CONS(Music);
};

#endif /* Time_scaled_music_HH */

