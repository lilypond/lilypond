/*   
  grace-music.cc --  implement New_grace_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "new-grace-music.hh"
#include "new-grace-iterator.hh"

void
New_grace_music::compress (Moment m) 
{
  Music_wrapper::compress (m); 
}

Moment
New_grace_music::length_mom () const
{
  Moment l = Music_wrapper::length_mom ();
  Moment gl;
  gl.grace_mom_ = l.main_part_ + l.grace_mom_ ;
  return gl;
}


Moment
New_grace_music::start_mom () const
{
  return Music::start_mom ();
}

New_grace_music::New_grace_music ()
{
  set_mus_property ("iterator-ctor",
		    New_grace_iterator::constructor_cxx_function);
}

New_grace_music::New_grace_music (SCM p)
  : Music_wrapper (p)
{
  set_mus_property ("iterator-ctor",
		    New_grace_iterator::constructor_cxx_function);
}

ADD_MUSIC (New_grace_music);
