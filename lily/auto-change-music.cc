/*   
  auto-switch-music.cc --  implement Auto_change_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "auto-change-music.hh"
#include "auto-change-iterator.hh"

Auto_change_music::Auto_change_music (SCM m)
  : Music_wrapper (m)
{
  set_mus_property ("iterator-ctor", Auto_change_iterator::constructor_cxx_function);

}
