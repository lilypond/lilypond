/*   
  unfolded-repeat-iterator.cc --  implement Unfolded_repeat_iterator
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */


#include "repeated-music.hh"
#include "music-list.hh"
#include "unfolded-repeat-iterator.hh"
#include "debug.hh"
#include "translator-group.hh"

Unfolded_repeat_iterator::~Unfolded_repeat_iterator ()
{
  delete current_iter_p_;
}

Unfolded_repeat_iterator::Unfolded_repeat_iterator ()
{
  done_count_ =0;
  current_iter_p_ =0;
  do_main_b_ = false;
  alternative_count_i_ =0;
}

/**

If we are in the body of the repeat always go to the current alternative.

If we are not in the body, then we are in an alternative.  If we are
fully unfolding, advance the current alternative and go back to main.
If we are semi-unfolding, advance the current alternative, and go to
the  alternative just set.
   
 */
void
Unfolded_repeat_iterator::next_element () 
{
  Repeated_music const* mus =dynamic_cast<Repeated_music const*> (music_l_);
  delete current_iter_p_;
  current_iter_p_ =0;


  if (do_main_b_)
    {
      done_mom_ += mus->repeat_body_p_->length_mom ();

      if (!mus->volta_fold_b_)
	done_count_ ++;
     
      if (alternative_cons_l_)
	{
	  current_iter_p_ = get_iterator_p (alternative_cons_l_->car_);
	  do_main_b_ = false;
	}
      else if (done_count_ <  mus->repeats_i_ && !mus->volta_fold_b_) 
	{
	  current_iter_p_ = get_iterator_p (mus->repeat_body_p_);
	  do_main_b_ = true;
	}
    }
  else
    {
      /*
	we're not in the main part. So we're either in an alternative, or
	we just finished.
      */
      if (alternative_cons_l_)
	{
	  done_mom_ += alternative_cons_l_->car_->length_mom ();

	  if (mus->volta_fold_b_ || 
	      mus->repeats_i_ - done_count_  < alternative_count_i_)
	    alternative_cons_l_ = alternative_cons_l_->next_;
	  
	  /*
	    we've done the main body as well, but didn't go over the other
	    increment.  */
	  if (mus->volta_fold_b_)
	    done_count_ ++;
	}
      
      if (done_count_ < mus->repeats_i_ && alternative_cons_l_)
	{
	  if (mus->volta_fold_b_)
	    current_iter_p_ = get_iterator_p (alternative_cons_l_->car_);
	  else
	    {
	      current_iter_p_ = get_iterator_p (mus->repeat_body_p_);
	      do_main_b_ = true;
	    }
	}
    }
}


bool
Unfolded_repeat_iterator::ok () const
{
  return current_iter_p_ ;
}

Moment
Unfolded_repeat_iterator::next_moment () const
{
  return done_mom_ + current_iter_p_->next_moment ();
}

void
Unfolded_repeat_iterator::construct_children ()
{
  Repeated_music const* mus =dynamic_cast<Repeated_music const*> (music_l_);
  alternative_cons_l_ = (mus->alternatives_p_)
    ? mus->alternatives_p_->music_p_list_p_->head_
    : 0;

  for (Cons<Music> *p = alternative_cons_l_; p; p = p->next_)
    alternative_count_i_ ++;

  if (mus->repeat_body_p_)
    {
      current_iter_p_  = get_iterator_p (mus->repeat_body_p_);
      do_main_b_ = true;
    }
  else if (alternative_cons_l_)
    {
      current_iter_p_ = get_iterator_p (alternative_cons_l_->car_);
      do_main_b_ = false;
    }
}

void
Unfolded_repeat_iterator::do_process_and_next (Moment m) 
{
  if (!m)
    {
      Music_iterator *yeah = try_music (music_l_);
      if (yeah)
	set_translator (yeah->report_to_l ());
      else
	music_l_->warning ( _("No one to print a volta bracket"));
    }
  while (1)
    {
      while (!current_iter_p_->ok ())
	{
	  next_element();

	  if (!current_iter_p_)
	    return;
	}
      
      if (m - done_mom_ >= current_iter_p_->next_moment ())
	current_iter_p_->process_and_next (m - done_mom_);
      else
	return;
    }
}
  
void
Unfolded_repeat_iterator::do_print () const
{
#ifndef NPRINT
  DOUT << "count " << done_count_ << "done time " << done_mom_ << '\n';
  DOUT << "current: ";
  current_iter_p_->print();
#endif
}

Music_iterator* 
Unfolded_repeat_iterator::try_music_in_children (Music const * m) const
{
  return  current_iter_p_->try_music (m);
}
