/*   
  time-scaled-music.hh -- declare Time_scaled_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef TIME_SCALED_MUSIC_HH
#define TIME_SCALED_MUSIC_HH

#include "music-wrapper.hh"
/**
   Tempo expansion or compression.
 */
class Time_scaled_music : public Music_wrapper
{
public:
  Time_scaled_music (SCM);
  VIRTUAL_COPY_CONSTRUCTOR (Music, Time_scaled_music);
};

#endif /* TIME_SCALED_MUSIC_HH */

