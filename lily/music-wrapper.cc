/*   
  music-wrapper.cc --  implement Music_wrapper
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */


#include "music-wrapper.hh"

Music_wrapper::~Music_wrapper ()
{
  delete element_p_;
}

Music_wrapper::Music_wrapper (Music_wrapper const&s)
  : Music (s)
{
  element_p_ = (s.element_p_)?s.element_p_->clone ():0;
}


void
Music_wrapper::do_print () const
{
  element_p_->print ();
}


void
Music_wrapper::transpose (Musical_pitch p)
{
  if (element_p_)
    element_p_-> transpose (p);
}


Music_wrapper::Music_wrapper(Music*p)
{
  element_p_ = p;
}



Moment
Music_wrapper::length_mom () const
{
  return element_p_->length_mom ();
}

Musical_pitch
Music_wrapper::to_relative_octave (Musical_pitch p)
{
  return element_p_->to_relative_octave (p);
}


Music*
Music_wrapper::element_l () const
{
  return element_p_;
}

void
Music_wrapper::compress (Moment m)
{
  element_l ()->compress (m);
}
