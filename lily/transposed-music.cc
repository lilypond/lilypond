/*   
  transposed-music.cc --  implement Transposed_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "transposed-music.hh"
#include "debug.hh"

Transposed_music::Transposed_music (SCM l)
  : Music_wrapper (l)
{
  set_mus_property ("type", ly_symbol2scm ("transposed-music"));
}


Pitch
Transposed_music::to_relative_octave (Pitch p)
{
  return p;
}

ADD_MUSIC (Transposed_music);
Transposed_music::Transposed_music ()
{

}
