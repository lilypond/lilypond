/*   
  un-relativable-music.cc --  implement Un_relativable_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "un-relativable-music.hh"


Un_relativable_music::Un_relativable_music ( )
{
  set_mus_property ("type", ly_symbol2scm ("un-relativable-music"));
}

Pitch
Un_relativable_music::to_relative_octave (Pitch p)
{
  return p;
}

ADD_MUSIC(Un_relativable_music);

