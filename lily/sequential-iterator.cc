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

  TODO: the grace note handling hasn't been done for skip() and
  get_music(), meaning that staff-switching and partcombining will be
  broken with grace notes.
  
 */
/*

  TODO: the grace note handling hasn't been done for skip() and
  get_music(), meaning that staff-switching and partcombining will be
  broken with grace notes.
  
 */
/*
  Invariant for the data structure.


  if (gh_pair_p (cursor_))
    iter_p_->music_l_ == unsmob_music (ly_car (cursor_))
  else
    iter_p_ == 0;

  The length of musiclist from start to up to cursor_ (cursor_ not
  including), is summed

  here_mom_  = sum (length (musiclist [start ... cursor>))  %)  
  
 */
Sequential_iterator::Sequential_iterator ()
{
  here_mom_ = Moment (0);
  grace_fixups_ = 0;
  iter_p_ =0;
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
  here_mom_ = src.here_mom_;
  if (src.iter_p_)
    iter_p_ = src.iter_p_->clone ();
  else
    iter_p_ = 0;
}

Sequential_iterator::~Sequential_iterator ()
{
  delete iter_p_;
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
  cursor_ = get_music_list ();

  iter_p_ = gh_pair_p (cursor_) ?  get_iterator_p (unsmob_music (ly_car (cursor_))) : 0;
  while (iter_p_ && !iter_p_->ok ())
    {
      next_element (true);
    }

  here_mom_ = music_l ()->start_mom ();
  grace_fixups_ = get_grace_fixups (cursor_);

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
Sequential_iterator::next_element (bool side_effect)
{
  Moment len =iter_p_->music_length_mom () - iter_p_->music_start_mom ();
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
  
  delete iter_p_;
  cursor_ = ly_cdr (cursor_);

  if (gh_pair_p (cursor_))
    iter_p_ = get_iterator_p (unsmob_music (ly_car (cursor_)));
  else
    iter_p_ = 0;
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
Sequential_iterator::get_music (Moment until)const
{
  SCM s = SCM_EOL;
  if (until <  pending_moment ())
    return s;

  Sequential_iterator * me =
    dynamic_cast<Sequential_iterator*> (clone ());
  while (me->ok ())
    {
      SCM nm = me->iter_p_->get_music (until - me->here_mom_);
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

  TODO: check support for grace notes here.
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
	  iter_p_->skip (iter_p_->music_length_mom ());
	}
      else if (iter_p_->music_length_mom () >= until - here_mom_)
	iter_p_->skip (until - here_mom_ + iter_p_->music_start_mom ());

      if (iter_p_->ok ())
	return ; 

      next_element (false);
    }
}

void
Sequential_iterator::process (Moment until)
{
  while (iter_p_)
    {
      if (grace_fixups_ &&
	  grace_fixups_->start_ == here_mom_
	  && (grace_fixups_->start_ + grace_fixups_->length_
	      + Moment (Rational (0), grace_fixups_->grace_start_) == until))
	{
	  /*
	    do the stuff/note/rest preceding a grace.
	   */
	  iter_p_->process (iter_p_->music_length_mom ());
	}
      else
	iter_p_->process (until - here_mom_ + iter_p_->music_start_mom ());

      /*
	if the iter is still OK, there must be events left that have
	
	  TIME > LEFT
	  
      */
      if (iter_p_->ok ())
	return ;

      descend_to_child ();
      next_element (true);
    }
}

Moment
Sequential_iterator::pending_moment () const
{
  Moment cp = iter_p_->pending_moment ();

  /*
    Fix-up a grace note halfway in the music.
  */
  if (grace_fixups_ && here_mom_ == grace_fixups_->start_
      && grace_fixups_->length_ + iter_p_->music_start_mom () == cp)
    {
      return here_mom_ + grace_fixups_->length_ + Moment (0, grace_fixups_->grace_start_);
    }

  /*
    Fix-up a grace note at  the start of the music.
  */
  return cp + here_mom_ - iter_p_->music_start_mom ();
}


bool
Sequential_iterator::ok () const
{
  return iter_p_;
}

Music_iterator*
Sequential_iterator::try_music_in_children (Music *m) const
{ 
  return iter_p_ ? iter_p_->try_music (m) : 0;
}

IMPLEMENT_CTOR_CALLBACK (Sequential_iterator);
