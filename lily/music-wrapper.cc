/*   
  music-wrapper.cc --  implement Music_wrapper
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */



#include "music-wrapper.hh"


Music_wrapper::Music_wrapper ()
  : Music ()
{
}

Moment
Music_wrapper::get_length () const
{
  return element ()->get_length ();
}

Music*
Music_wrapper::element () const
{
  return unsmob_music (get_property ("element"));
}


ADD_MUSIC (Music_wrapper);


Moment
Music_wrapper::start_mom () const
{
  return element ()->start_mom ();
}
