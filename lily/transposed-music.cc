/*   
  transposed-music.cc --  implement Transposed_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "transposed-music.hh"
#include "debug.hh"

Transposed_music::Transposed_music (Music *p, Musical_pitch pit)
  : Music_wrapper (p)
{
  transpose_to_pitch_ = pit;
  p->transpose (pit);
}



void
Transposed_music::do_print () const
{
  Music_wrapper::do_print ();
  DOUT << "transposition pitch: " << transpose_to_pitch_.str ();
}

Musical_pitch
Transposed_music::to_relative_octave (Musical_pitch p)
{
#if 0				// Mats?
    warning (_("Will ignore \relative for transposed music"));
#endif
  return p;
}

IMPLEMENT_IS_TYPE_B1 (Transposed_music, Music_wrapper);
