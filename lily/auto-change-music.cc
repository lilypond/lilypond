
/*   
  auto-switch-music.cc --  implement 
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "auto-change-music.hh"

Auto_change_music::Auto_change_music (String what, Music * m)
  : Music_wrapper (m)
{
  what_str_ = what;
}
