/*   
  music-wrapper-iterator.cc --  implement Music_wrapper_iterator
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
 */


#include "music-wrapper-iterator.hh"
#include "music-wrapper.hh"

Music_wrapper_iterator::Music_wrapper_iterator ()
{
  child_iter_p_ =0;
}

Music_wrapper_iterator::Music_wrapper_iterator (Music_wrapper_iterator const &src)
  : Music_iterator (src)
{
  if (src.child_iter_p_)
    child_iter_p_ = src.child_iter_p_->clone ();
  else
    child_iter_p_ = 0;
}

Music_wrapper_iterator::~Music_wrapper_iterator ()
{
  delete child_iter_p_;
}


void
Music_wrapper_iterator::construct_children ()
{
  child_iter_p_ =
    get_iterator_p (dynamic_cast<Music_wrapper const*> (music_l_)->element ());
}

bool
Music_wrapper_iterator::ok () const
{
  return child_iter_p_ && child_iter_p_->ok ();
}
void
Music_wrapper_iterator::skip (Moment m)
{
  /*
    FIXME: should make sure that the initial try_music () is skipped as
    well, if you would do

    iter = get_iterator (Side_effect_music); // eg. property setting
    iter->skip (1/2)
    iter->process ()

  */
  child_iter_p_->skip (m);
}

void
Music_wrapper_iterator::process (Moment m)
{
  child_iter_p_->process (m);
}

SCM
Music_wrapper_iterator::get_music (Moment m)const
{
  return child_iter_p_->get_music (m);
}

Moment
Music_wrapper_iterator::pending_moment () const
{
  return child_iter_p_->pending_moment ();
}

Music_iterator*
Music_wrapper_iterator::try_music_in_children (Music *m) const
{
  return child_iter_p_->try_music (m);
}
