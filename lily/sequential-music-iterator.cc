/*
  Sequential_music_iterator.cc -- implement Sequential_music_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "grace-iterator.hh"
#include "translator-group.hh"
#include "debug.hh"
#include "sequential-music-iterator.hh"
#include "music-list.hh"
#include "request-chord-iterator.hh"

Sequential_music_iterator::Sequential_music_iterator ()
{
  cursor_ = 0;
  here_mom_ = 0;
  iter_p_ =0;
}

Sequential_music_iterator::Sequential_music_iterator (Sequential_music_iterator const &src)
  : Music_iterator (src)
{
  cursor_ = src.cursor_;
  here_mom_ = src.here_mom_;
  iter_p_ = src.iter_p_->clone ();
}

Sequential_music_iterator::~Sequential_music_iterator()
{
  if (iter_p_)
    {
      /*      if (iter_p_->ok () )
	music_l_->origin ()->warning (_ ("Must stop before this music ends"));
      */
      delete iter_p_;
      iter_p_ = 0;
    }
}


void
Sequential_music_iterator::construct_children()
{
  cursor_ = dynamic_cast<Music_sequence const*> (music_l_)->music_list ();
  
  while (gh_pair_p (cursor_ ))
    {
      start_next_element();
      if (!iter_p_->ok()) 
	{
	  leave_element();
	}
      else 
	{
	  set_sequential_music_translator();
	  break;
	}
    }
}

void 
Sequential_music_iterator::leave_element()
{
  delete iter_p_;
  iter_p_ =0;
  Moment elt_time = unsmob_music (gh_car (cursor_))->length_mom ();
  here_mom_ += elt_time;
  cursor_ =gh_cdr (cursor_);
}

void
Sequential_music_iterator::start_next_element()
{
  assert (!iter_p_);
  iter_p_ = get_iterator_p (unsmob_music (gh_car (cursor_)));
}

void
Sequential_music_iterator::set_sequential_music_translator()
{
  Translator_group  * child_report = child_report = iter_p_->report_to_l ();
  if (dynamic_cast<Grace_iterator*> (iter_p_))
    child_report = child_report->daddy_trans_l_;
    
  if (report_to_l()->depth_i () < child_report->depth_i ())
    set_translator (child_report);
}


SCM
Sequential_music_iterator::get_music (Moment until)const
{
#if 0
  /*
     FIXME: get_music () is const, so we must operate on a copy of child-iter.
  */
  
  SCM s = SCM_EOL;
  while (1) 
      {
	Moment local_until = until - here_mom_;
	while (iter_p_->ok ()) 
	  {
	    Moment here = iter_p_->pending_moment ();
	    if (here != local_until)
	      return s;
	    
	    s = gh_append2 (iter_p_->get_music (local_until), s);
	  }
	  
	  if (!iter_p_->ok ()) 
	    {
	      //	      leave_element ();
	      
	      if (gh_pair_p (cursor_))
		start_next_element ();
	      else
		return s;
	    }
	}
  return s;
#endif
  return SCM_EOL;
}

void
Sequential_music_iterator::process (Moment until)
{
  if (ok ())
    {
      while (1) 
	{
	  Moment local_until = until - here_mom_;
	  while (iter_p_->ok ()) 
	    {
	      Moment here = iter_p_->pending_moment ();
	      if (here != local_until)
		return ;
	      
	      iter_p_->process (local_until);
	    }
	  
	  if (!iter_p_->ok ()) 
	    {
	      set_sequential_music_translator ();
	      leave_element ();
	      
	      if (gh_pair_p (cursor_))
		start_next_element ();
	      else 
		return ;
	    }
	}
    }
}

Moment
Sequential_music_iterator::pending_moment() const
{
  return iter_p_->pending_moment() + here_mom_;
}


bool
Sequential_music_iterator::ok() const
{
  return iter_p_;
}

Music_iterator*
Sequential_music_iterator::try_music_in_children (Music *m) const
{ 
  return iter_p_ ? iter_p_->try_music (m) : 0;
}
