/*   
  music-wrapper.cc --  implement Music_wrapper
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */


#include "music-wrapper.hh"




void
Music_wrapper::transpose (Musical_pitch p)
{
  if (element ())
    element ()-> transpose (p);
}


Music_wrapper::Music_wrapper(Music*p)
{
  set_mus_property ("element", p->self_scm ());
  scm_unprotect_object (p->self_scm ());
}

Moment
Music_wrapper::length_mom () const
{
  return element ()->length_mom ();
}

Musical_pitch
Music_wrapper::to_relative_octave (Musical_pitch p)
{
  return element ()->to_relative_octave (p);
}


Music*
Music_wrapper::element () const
{
  return unsmob_music (get_mus_property ("element"));
}

void
Music_wrapper::compress (Moment m)
{
  element ()->compress (m);
}
