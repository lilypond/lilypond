/*
  Sequential_music_iterator.cc -- implement Sequential_music_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "grace-iterator.hh"
#include "translator-group.hh"
#include "debug.hh"
#include "sequential-music-iterator.hh"
#include "music-list.hh"
#include "request-chord-iterator.hh"

/*
  Invariant for the data structure.


  if (gh_pair_p (cursor_))
    iter_p_->music_l_ == unsmob_music (gh_car (cursor_))
  else
    iter_p_ == 0;

  The length of musiclist from start to up to cursor_ (cursor_ not
  including), is summed

  here_mom_  = sum (length (musiclist [start ... cursor>))  %)  
  
 */


Sequential_music_iterator::Sequential_music_iterator ()
{
  cursor_ = SCM_EOL;
  here_mom_ = Moment (0);

  iter_p_ =0;
}

Sequential_music_iterator::Sequential_music_iterator (Sequential_music_iterator const &src)
  : Music_iterator (src)
{
  cursor_ = src.cursor_;
  here_mom_ = src.here_mom_;
  if (src.iter_p_)
    iter_p_ = src.iter_p_->clone ();
  else
    iter_p_ = 0;
}

Sequential_music_iterator::~Sequential_music_iterator ()
{
  delete iter_p_;
}

void
Sequential_music_iterator::construct_children ()
{
  cursor_ = dynamic_cast<Music_sequence const*> (music_l_)->music_list ();

  iter_p_ = gh_pair_p (cursor_) ?  get_iterator_p (unsmob_music (gh_car (cursor_))) : 0;
  while (iter_p_ && !iter_p_->ok ())
    {
      next_element ();
    }

  /*
    iter_p_->ok () is tautology, but what the heck.
   */
  if (iter_p_ && iter_p_->ok ()) 
    descend_to_child ();

}


/*
  maintain invariants: change cursor, iter and here_mom_ in one fell
  swoop.
*/
void
Sequential_music_iterator::next_element ()
{
  here_mom_ += iter_p_->music_length_mom ();
  delete iter_p_;
  cursor_ = gh_cdr (cursor_);

  if (gh_pair_p (cursor_))
    iter_p_ = get_iterator_p (unsmob_music (gh_car (cursor_)));
  else
    iter_p_ = 0;
}

/*
  move to context of child iterator if it is deeper down in the
  hierarchy.
  */

void
Sequential_music_iterator::descend_to_child ()
{
  Translator_group  * child_report = child_report = iter_p_->report_to_l ();
  Translator_group * me_report = report_to_l ();

  if (dynamic_cast<Grace_iterator*> (iter_p_))
    child_report = child_report->daddy_trans_l_;

  Translator_group * c = child_report;
  while (c && c != me_report)
    {
      c= c->daddy_trans_l_;
    }
  
  if (c == me_report)
    set_translator (child_report);
}


/*
  Retrieve all music (starting at HERE), until a music with length L >
  0 is found.  From the precondition, we know that UNTIL is later than
  the earliest event. Hence we know
  
  L >= (UNTIL - HERE)

  so something that comes after this thing with L > 0 happens after

  HERE + L >= HERE + (UNTIL - HERE) = UNTIL

  Hence all events after the one with L>0 are uninteresting, so we
  ignore them.
  
*/

SCM
Sequential_music_iterator::get_music (Moment until)const
{
  SCM s = SCM_EOL;
  if (until <  pending_moment ())
    return s;

  Sequential_music_iterator * me =
    dynamic_cast<Sequential_music_iterator*> (clone ());
  while (me->ok ())
    {
      SCM nm = me->iter_p_->get_music (until - me->here_mom_);
      s = gh_append2 (nm, s);
      
      Moment m = 0;
      for (SCM i = nm; gh_pair_p (i); i = gh_cdr (i))
	m = m >? unsmob_music (gh_car (i))->length_mom ();

      if (m > Moment (0))
	break ;
      else
	me->next_element ();
    }
  delete me;
  
  return s;
}
/*
  Skip events till UNTIL. We don't do any other side effects (such as
  moving descending to child iterator contexts, because they might
  depend on \context specs and \translator changes being executed
    
 */
void
Sequential_music_iterator::skip (Moment until)
{
  while (ok ())
    {
      Moment l =iter_p_->music_length_mom ();
      if (l >= until - here_mom_)
	iter_p_->skip (until - here_mom_);

      if (iter_p_->ok ())
	return ; 

      next_element ();
    }
}

void
Sequential_music_iterator::process (Moment until)
{
  while (iter_p_)
    {
      iter_p_->process (until - here_mom_);

      /*
	if the iter is still OK, there must be events left that have
	
	  TIME > LEFT
	  
      */
      if (iter_p_->ok ())
	return ;

      descend_to_child ();
      next_element ();
    }
}

Moment
Sequential_music_iterator::pending_moment () const
{
  return iter_p_->pending_moment () + here_mom_;
}


bool
Sequential_music_iterator::ok () const
{
  return iter_p_;
}

Music_iterator*
Sequential_music_iterator::try_music_in_children (Music *m) const
{ 
  return iter_p_ ? iter_p_->try_music (m) : 0;
}
IMPLEMENT_CTOR_CALLBACK (Sequential_music_iterator);
