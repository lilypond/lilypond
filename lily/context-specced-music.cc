/*   
  context-specced-music.cc --  implement Context_specced_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "context-specced-music.hh"
Context_specced_music::Context_specced_music (SCM m)
  : Music_wrapper (m)
{
  set_mus_property ("type",
		    ly_symbol2scm ("context-specced-music"));
}


Context_specced_music::Context_specced_music ()
{
  set_mus_property ("type",
		    ly_symbol2scm ("context-specced-music"));
}

ADD_MUSIC (Context_specced_music);
