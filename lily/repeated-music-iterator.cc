/*   
  repeated-music-iterator.cc --  implement Repeated_music_iterator
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--1999 Jan Nieuwenhuizen <janneke@gnu.org>
  
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
  here_mom_ = 0;
  unfold_i_ = -1; 
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
  repeat_iter_p_ = get_iterator_p (dynamic_cast<Repeated_music const*> (music_l_)->repeat_p_);
}

void
Repeated_music_iterator::do_process_and_next (Moment m)
{
  if (first_b_)
    {
      bool success = report_to_l ()->try_music (dynamic_cast<Repeated_music const*> (music_l_));
      if (!success)
	music_l_->warning ( _("No one to print a volta bracket"));
    }
  if (repeat_iter_p_ && repeat_iter_p_->ok ())
    repeat_iter_p_->process_and_next (m - here_mom_);
  else
    alternative_iter_p_->process_and_next (m - here_mom_);
  Music_iterator::do_process_and_next (m);
}

Moment
Repeated_music_iterator::next_moment () const
{
  
  if (repeat_iter_p_)
    return repeat_iter_p_->next_moment () + here_mom_;
  else if (alternative_iter_p_)
    return alternative_iter_p_->next_moment () + here_mom_;

  Repeated_music const*r = dynamic_cast<Repeated_music const*>(music_l_);
  return r->alternative_p_->length_mom () + here_mom_;
}

/*
  FIXME
 */
bool
Repeated_music_iterator::ok () const
{
  if (!repeat_iter_p_ && !alternative_iter_p_)
    return false;

  if ((repeat_iter_p_ && repeat_iter_p_->ok ())
    || (alternative_iter_p_ && alternative_iter_p_->ok ()))
    return true;

  Repeated_music_iterator *urg = (Repeated_music_iterator*)this;
  // urg, we're const
  urg->start_next_element ();

  return ok ();
}


void
Repeated_music_iterator::start_next_element ()
{
  Repeated_music const*rep =dynamic_cast<Repeated_music const*> (music_l_);


 if (repeat_iter_p_)
    {
      assert (!repeat_iter_p_->ok ());
      assert (!alternative_iter_p_);
      delete repeat_iter_p_;
      repeat_iter_p_ = 0;
      alternative_iter_p_ = dynamic_cast<Music_list_iterator*>
	(get_iterator_p ((Music*)rep->alternative_p_));  
      here_mom_ += rep->repeat_p_->length_mom ();
    }
  else if (alternative_iter_p_)
    {
      assert (!alternative_iter_p_->ok ());
      assert (!repeat_iter_p_);
      delete alternative_iter_p_;
      alternative_iter_p_ = 0;
      if (unfold_i_ < 0)
	unfold_i_ = rep->unfold_b_ ? 
	  rep->repeats_i_ - 1 : 0;
      if (unfold_i_)
        {
	  unfold_i_--;
	  repeat_iter_p_ = get_iterator_p (rep->repeat_p_);
	  // urg, assume same length alternatives for now...
//	  here_mom_ += rep->alternative_p_->music_p_list_p_->top ()->length_mom ();
	  /*
	    URG
	    this is *wrong* but at least it doesn't dump core
	    when unfolding, the alternative (sequential) music 
	    shouldn't automatically move to the next alternative

	    how to intercept this...
	   */
	  here_mom_ += rep->alternative_p_->length_mom ();
	}
    }
}

