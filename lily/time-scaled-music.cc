/*   
  time-scaled-music.cc --  implement Time_scaled_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "time-scaled-music.hh"
#include "time-scaled-music-iterator.hh"

Time_scaled_music::Time_scaled_music (SCM l)
  : Music_wrapper (l)
{
  set_mus_property ("iterator-ctor",
		    Time_scaled_music_iterator::constructor_cxx_function);
  
}




