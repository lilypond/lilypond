/*   
  time-scaled-music.cc --  implement Time_scaled_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "time-scaled-music.hh"


Time_scaled_music::Time_scaled_music (int n, int d,Music *mp)
  : Music_wrapper (mp)
{
  num_i_ = n;
  den_i_ = d;
  compress (Moment (num_i_,den_i_));
  set_mus_property ("type", ly_symbol2scm ("time-scaled-music"));
}




