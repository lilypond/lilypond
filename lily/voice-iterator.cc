/*
  voice-iter.cc -- implement Voice_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "translator-group.hh"
#include "debug.hh"
#include "voice-iterator.hh"
#include "music-list.hh"


void
Voice_iterator::do_print() const
{
  if (iter_p_)
    iter_p_->print();
}

Voice_iterator::Voice_iterator (Voice const*v)
  : PCursor<Music*> (v->music_p_list_)
{
  here_mom_ = v->offset_mom_;
  voice_C_ = v;
  iter_p_ =0;
}

void
Voice_iterator::construct_children()
{
  while (PCursor<Music*>::ok()) 
    {
      start_next_element();
      if (!iter_p_->ok()) 
	{
	  leave_element();
	}
      else 
	{
	  set_voice_translator();
	  break;
	}
    }
}

void 
Voice_iterator::leave_element()
{
  delete iter_p_;
  iter_p_ =0;
  MInterval elt_time = ptr()->time_int ();
  if (!elt_time.empty_b())
    here_mom_ += elt_time.length();
  PCursor<Music*>::next();
}

void
Voice_iterator::start_next_element()
{
  assert (!iter_p_);
  iter_p_ = get_iterator_p (ptr());
}

void
Voice_iterator::set_voice_translator()
{
  if (iter_p_->report_to_l()->depth_i () > report_to_l ()->depth_i ())
    set_translator (iter_p_->report_to_l());
}

Voice_iterator::~Voice_iterator()
{
  assert (! iter_p_);
}


IMPLEMENT_IS_TYPE_B1(Voice_iterator,Music_iterator);

void
Voice_iterator::process_and_next (Moment until)
{
  while (1) 
    {
      Moment local_until = until - here_mom_;
      while (iter_p_->ok()) 
	{
	  Moment here = iter_p_->next_moment();
	  if (here != local_until)
	    goto loopexit;
	    
	  iter_p_->process_and_next (local_until);
	}

      if (!iter_p_->ok()) 
	{
	  leave_element();
	  
	  if (PCursor<Music*>::ok()) 
	    {
	      start_next_element();
	      set_voice_translator();
	    }
	  else 
	    {
	      goto loopexit;
	    }
	}
    }

loopexit:

  Music_iterator::process_and_next (until);
}

Moment
Voice_iterator::next_moment() const
{
  return iter_p_->next_moment() + here_mom_;
}

bool
Voice_iterator::ok() const
{
  return iter_p_;
}


