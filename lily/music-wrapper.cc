/*   
  music-wrapper.cc --  implement 
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
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

IMPLEMENT_IS_TYPE_B1 (Music_wrapper, Music);

Moment
Music_wrapper::duration () const
{
  return element_p_->duration ();
}
