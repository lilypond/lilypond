/*   
  time-scaled-music.cc --  implement Time_scaled_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "time-scaled-music.hh"

#include "time-scaled-music-iterator.hh"

Time_scaled_music::Time_scaled_music (SCM x)
  : Music_wrapper (x)
{
}

ADD_MUSIC (Time_scaled_music);
