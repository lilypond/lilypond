/*   
untransposable-music.cc --  implement Untransposable_music

source file of the GNU LilyPond music typesetter

(c) 2001--2002  Han-Wen Nienhuys <hanwen@cs.uu.nl>

 */

#include "untransposable-music.hh"

void
Untransposable_music::transpose (Pitch )
{
}

Pitch
Untransposable_music::to_relative_octave (Pitch p)
{
  return p;
}

ADD_MUSIC(Untransposable_music);

Untransposable_music::Untransposable_music()
{

}
