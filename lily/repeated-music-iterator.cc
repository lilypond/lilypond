/*   
  repeated-music-iterator.cc --  implement Repeated_music_iterator
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>
  
 */

#include "repeated-music-iterator.hh"
#include "repeated-music.hh"
#include "musical-request.hh"
#include "translator-group.hh"
#include "command-request.hh"

Repeated_music_iterator::Repeated_music_iterator ()
{
  repeat_iter_p_ = 0;
  alternative_iter_p_ = 0;
}

Repeated_music_iterator::~Repeated_music_iterator ()
{
  delete repeat_iter_p_;
  delete alternative_iter_p_;
}

void
Repeated_music_iterator::do_print () const
{
  if (repeat_iter_p_) repeat_iter_p_->print ();
  if (alternative_iter_p_) alternative_iter_p_->print ();
}

void
Repeated_music_iterator::construct_children ()
{
  repeat_iter_p_ = get_iterator_p (repeated_music_l ()->repeat_p_);  
}

void
Repeated_music_iterator::do_process_and_next (Moment m)
{
  if (first_b_)
    {
      bool success = report_to_l ()->try_music (repeated_music_l ());
      if (!success)
	music_l_->warning ( _("No one to print a volta bracket"));
    }
  if (repeat_iter_p_ && repeat_iter_p_->ok ())
    repeat_iter_p_->process_and_next (m);
  else
    alternative_iter_p_->process_and_next (m - 
      repeated_music_l ()->repeat_p_->duration ());
  Music_iterator::do_process_and_next (m);
}

Moment
Repeated_music_iterator::next_moment () const
{
  if (repeat_iter_p_)
    return repeat_iter_p_->next_moment ();
  else if (alternative_iter_p_)
//    return alternative_iter_p_->next_moment ();
    return alternative_iter_p_->next_moment () + 
      repeated_music_l ()->repeat_p_->duration ();
// return 0;
  return repeated_music_l ()->repeat_p_->duration ();
}

bool
Repeated_music_iterator::ok () const
{
#if 0
  if (repeat_iter_p_)
    return repeat_iter_p_->ok ();
  else if (alternative_iter_p_)
    return alternative_iter_p_->ok ();
  return false;
#elif 0
  if (repeat_iter_p_ && repeat_iter_p_->ok ())
    return true;
  else if (!alternative_iter_p_)
    return true;
  return alternative_iter_p_->ok ();
#else // perhaps iterating stops because we return false on repeat_iter...
  if (repeat_iter_p_)
    {
      if (repeat_iter_p_->ok ())
        return true;
      else
        {
	  // urg, we're const
	  Repeated_music_iterator *urg = (Repeated_music_iterator*)this;
	  delete urg->repeat_iter_p_;
	  urg->repeat_iter_p_ = 0;
	  urg->alternative_iter_p_ = dynamic_cast<Music_list_iterator*>
	    (get_iterator_p ((Music*)repeated_music_l ()->alternative_p_));  
	}
    }
  if (alternative_iter_p_)
    return alternative_iter_p_->ok ();
  return false;
#endif
}

Repeated_music*
Repeated_music_iterator::repeated_music_l () const
{
  return (Repeated_music*)Music_iterator::music_l_;
}

