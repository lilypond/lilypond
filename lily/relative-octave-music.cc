/*   
  relative-music.cc --  implement Relative_octave_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "relative-music.hh"
#include "debug.hh"

Musical_pitch
Relative_octave_music::to_relative_octave (Musical_pitch)
{
  return last_pitch_;
}


Relative_octave_music::Relative_octave_music(Music*p,Musical_pitch def)
  : Music_wrapper (p)
{
  last_pitch_ = element ()->to_relative_octave (def);
}



