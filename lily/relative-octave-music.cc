/*   
  relative-music.cc --  implement Relative_octave_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "relative-music.hh"
#include "debug.hh"

Pitch
Relative_octave_music::to_relative_octave (Pitch)
{
  return * unsmob_pitch (get_mus_property ("last-pitch"));
}


Relative_octave_music::Relative_octave_music(Music*p,Pitch def)
  : Music_wrapper (p)
{
  set_mus_property ("last-pitch", element ()->to_relative_octave (def).smobbed_copy ());
  set_mus_property ("type", ly_symbol2scm ("relative-octave-music"));  
}



