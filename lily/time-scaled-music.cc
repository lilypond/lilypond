/*   
  time-scaled-music.cc --  implement Time_scaled_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "time-scaled-music.hh"
#include "time-scaled-music-iterator.hh"

Time_scaled_music::Time_scaled_music (int n, int d,Music *mp)
  : Music_wrapper (mp)
{
  set_mus_property ("numerator", gh_int2scm (n));
  set_mus_property ("denominator", gh_int2scm (d));
  set_mus_property ("iterator-ctor",
		    Time_scaled_music_iterator::constructor_cxx_function);
  
  compress (Moment (n,d));
}




