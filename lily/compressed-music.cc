/*   
  compressed-music.cc --  implement Compressed_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "compressed-music.hh"


Compressed_music::Compressed_music (int n, int d,Music *mp)
  : Music_wrapper (mp)
{
  num_i_ = n;
  den_i_ = d;
  element_p_->compress (Moment (num_i_,den_i_));
}

IMPLEMENT_IS_TYPE_B1(Compressed_music, Music_wrapper);


