/*   
  grace-music.cc --  implement Grace_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "grace-music.hh"
#include "grace-iterator.hh"

void
Grace_music::compress (Moment m) 
{
  Music_wrapper::compress (m); 
}

Moment
Grace_music::length_mom () const
{
  Moment m ;
  return m;
}


Moment
Grace_music::start_mom () const
{
  Moment l = Music_wrapper::length_mom ();
  Moment gl;
  gl.grace_part_ = -(l.main_part_ + l.grace_part_ );
  return gl;
}

Grace_music::Grace_music ()
{
  set_mus_property ("iterator-ctor",
		    Grace_iterator::constructor_cxx_function);
}

Grace_music::Grace_music (SCM p)
  : Music_wrapper (p)
{
  set_mus_property ("iterator-ctor",
		    Grace_iterator::constructor_cxx_function);
}

ADD_MUSIC (Grace_music);
