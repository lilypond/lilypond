/*   
  grace-music.cc --  implement Grace_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "grace-music.hh"

void
Grace_music::compress (Moment)
{
  
}

Moment
Grace_music::length_mom () const
{
  return 0;
}

Grace_music::Grace_music (Music *p)
  : Music_wrapper (p)
{
}
