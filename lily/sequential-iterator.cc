/*
  Sequential_iterator.cc -- implement Sequential_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "translator-group.hh"

#include "sequential-iterator.hh"
#include "music-list.hh"

Grace_fixup *copy_grace_fixups (Grace_fixup* src);
Grace_fixup *get_grace_fixups (SCM cursor);

/*
  
  TODO: handling of grace notes is exquisite pain.  This handling
  should be formally specified and then the implementation verified.

*/

/*
  Invariant for the data structure.


  if (gh_pair_p (cursor_))
    iter_->music_ == unsmob_music (ly_car (cursor_))
  else
    iter_ == 0;

  The length of musiclist from start to up to cursor_ (cursor_ not
  including), is summed

  here_mom_  = sum (length (musiclist [start ... cursor>))  %)  
  
 */
Sequential_iterator::Sequential_iterator ()
{
  here_mom_ = Moment (0);
  grace_fixups_ = 0;
  iter_ =0;
}

SCM 
Sequential_iterator::get_music_list () const
{
  return SCM_EOL;
}

Sequential_iterator::Sequential_iterator (Sequential_iterator const &src)
  : Music_iterator (src)
{
  grace_fixups_ = copy_grace_fixups (src.grace_fixups_);
  cursor_ = src.cursor_;
  list_ = src.cursor_;
  here_mom_ = src.here_mom_;
  if (src.iter_)
    iter_ = src.iter_->clone ();
  else
    iter_ = 0;

  if (iter_)
    scm_gc_unprotect_object (iter_->self_scm());
}

void
Sequential_iterator::derived_mark ()const
{
  if (iter_)
    scm_gc_mark (iter_->self_scm());
}


Grace_fixup *
get_grace_fixups (SCM cursor)
{
  Moment here;
  Moment last (-1);
  Grace_fixup *head = 0;
  Grace_fixup **tail = &head;

  for (; gh_pair_p (cursor); cursor = ly_cdr (cursor))
    {
      Music * mus = unsmob_music (ly_car (cursor));
      Moment s = mus->start_mom ();
      Moment l =mus->length_mom () - s;

      if (s.grace_part_)
	{
	  if (last != Moment (-1))
	    {
	      Grace_fixup *p =new Grace_fixup;
	      p->start_ = last;
	      p->length_ = here - last;
	      p->grace_start_ = s.grace_part_;
	      p->next_ = 0;
	      *tail = p;
	      tail = &(*tail)->next_; 
	    }

	  here.grace_part_ = s.grace_part_;
	}
      
      if (l.to_bool())
	{
	  last = here;
	  here += l;
	}
    }
  return  head;
}

Grace_fixup *
copy_grace_fixups (Grace_fixup* src)
{
  Grace_fixup * head = 0;
  Grace_fixup **dest = &head;

  while (src)
    {
      *dest = new Grace_fixup (*src);
      dest = & (*dest)->next_;
      src = src ->next_;
    }

  return head;
}

void
Sequential_iterator::construct_children ()
{
  list_ = get_music_list ();
  cursor_ = list_; 

  iter_ = 0;
  if (gh_pair_p (cursor_))
    {
      Music *m  =unsmob_music (ly_car (cursor_));
      iter_ = unsmob_iterator ( get_iterator (m));
    }
  
  while (iter_ && !iter_->ok ())
    {
      next_element (true);
    }

  here_mom_ = get_music ()->start_mom ();
  grace_fixups_ = get_grace_fixups (cursor_);

  /*
    iter_->ok () is tautology, but what the heck.
   */
  if (iter_ && iter_->ok ()) 
    descend_to_child ();
}


/*
  maintain invariants: change cursor, iter and here_mom_ in one fell
  swoop.
*/
void
Sequential_iterator::next_element (bool side_effect)
{
  Moment len =iter_->music_length_mom () - iter_->music_start_mom ();
  assert (!grace_fixups_  || grace_fixups_->start_ >= here_mom_);
  
  if (len.main_part_ && grace_fixups_ &&
      grace_fixups_->start_ == here_mom_)
    {
      here_mom_ += grace_fixups_->length_;
      here_mom_.grace_part_ += grace_fixups_->grace_start_;

      Grace_fixup * n =grace_fixups_->next_;
      delete grace_fixups_;
      grace_fixups_ = n;
    }
  else if (len.grace_part_ && !len.main_part_)
    {
      here_mom_.grace_part_ =0;
    }
  else
    {
      /*
	!len.grace_part_ || len.main_part_

	We skip over a big chunk (mainpart != 0). Any starting graces
	in that chunk should be in len.grace_part_

      */
      here_mom_ += len;
    }
  
  cursor_ = ly_cdr (cursor_);

  if (gh_pair_p (cursor_))
    iter_ = unsmob_iterator (get_iterator (unsmob_music (ly_car (cursor_))));
  else
    iter_ = 0;
}

/*
  move to context of child iterator if it is deeper down in the
  hierarchy.
  */
void
Sequential_iterator::descend_to_child ()
{
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
Sequential_iterator::get_pending_events (Moment until)const
{
  SCM s = SCM_EOL;
  if (until <  pending_moment ())
    return s;

  Sequential_iterator * me =
    dynamic_cast<Sequential_iterator*> (clone ());
  while (me->ok ())
    {
      SCM nm = me->iter_->get_pending_events (until - me->here_mom_);
      s = gh_append2 (nm, s);
      
      Moment m = 0;
      for (SCM i = nm; gh_pair_p (i); i = ly_cdr (i))
	{
	  Music *mus=unsmob_music (ly_car (i));
	  m = m >? (mus->length_mom () - mus->start_mom ());
	}
      if (m > Moment (0))
	break ;
      else
	me->next_element (false);
    }
  delete me;
  
  return s;
}


/*
  Skip events till UNTIL. We don't do any other side effects such as
  descending to child iterator contexts, because they might depend on
  \context specs and \translator changes being executed
 */
void
Sequential_iterator::skip (Moment until)
{
  while (ok ())
    {
      if (grace_fixups_ &&
	  grace_fixups_->start_ == here_mom_
	  && (grace_fixups_->start_ + grace_fixups_->length_
	      + Moment (Rational (0), grace_fixups_->grace_start_) == until))
	{
	  /*
	    do the stuff/note/rest preceding a grace.
	   */
	  iter_->skip (iter_->music_length_mom ());
	}
      else if (iter_->music_length_mom () >= until - here_mom_)
	iter_->skip (until - here_mom_ + iter_->music_start_mom ());

      if (iter_->ok ())
	return ; 

      next_element (false);
    }
}

void
Sequential_iterator::process (Moment until)
{
  while (iter_)
    {
      if (grace_fixups_ &&
	  grace_fixups_->start_ == here_mom_
	  && (grace_fixups_->start_ + grace_fixups_->length_
	      + Moment (Rational (0), grace_fixups_->grace_start_) == until))
	{
	  /*
	    do the stuff/note/rest preceding a grace.
	   */
	  iter_->process (iter_->music_length_mom ());
	}
      else
	iter_->process (until - here_mom_ + iter_->music_start_mom ());

      /*
	if the iter is still OK, there must be events left that have
	
	  TIME > LEFT
	  
      */
      if (iter_->ok ())
	return ;

      descend_to_child ();
      next_element (true);
    }
}

Moment
Sequential_iterator::pending_moment () const
{
  Moment cp = iter_->pending_moment ();

  /*
    Fix-up a grace note halfway in the music.
  */
  if (grace_fixups_ && here_mom_ == grace_fixups_->start_
      && grace_fixups_->length_ + iter_->music_start_mom () == cp)
    {
      return here_mom_ + grace_fixups_->length_ + Moment (0, grace_fixups_->grace_start_);
    }

  /*
    Fix-up a grace note at  the start of the music.
  */
  return cp + here_mom_ - iter_->music_start_mom ();
}


bool
Sequential_iterator::ok () const
{
  return iter_;
}

Music_iterator*
Sequential_iterator::try_music_in_children (Music *m) const
{ 
  return iter_ ? iter_->try_music (m) : 0;
}

IMPLEMENT_CTOR_CALLBACK (Sequential_iterator);
