
/*   
  auto-switch-music.cc --  implement 
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "auto-change-music.hh"

Auto_change_music::Auto_change_music (Music * m)
  : Music_wrapper (m)
{
  set_mus_property ("type", ly_symbol2scm ("auto-change-music"));
}
