/*   
  transposed-music.cc --  implement Transposed_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "transposed-music.hh"
#include "warn.hh"

Pitch
Transposed_music::to_relative_octave (Pitch p)
{
  return p;
}

ADD_MUSIC (Transposed_music);
Transposed_music::Transposed_music ()
{

}
