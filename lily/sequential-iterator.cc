/*
  Sequential_iterator.cc -- implement Sequential_iterator

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "translator-group.hh"
#include "context.hh"
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
  list_ = SCM_EOL;
  cursor_ = SCM_EOL; 
  grace_fixups_ = 0;
  iter_ =0;
}

SCM 
Sequential_iterator::get_music_list () const
{
  return SCM_EOL;
}

void
Sequential_iterator::do_quit ()
{
  if (iter_)
    iter_->quit();
}




void
Sequential_iterator::derived_mark ()const
{
  if (iter_)
    scm_gc_mark (iter_->self_scm());
  scm_gc_mark (list_);
  scm_gc_mark (cursor_);
}


void
Sequential_iterator::derived_substitute (Context *f,Context *t)
{
  if (iter_)
    iter_->substitute_outlet (f,t);
  
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
      Moment l =mus->get_length () - s;

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
      iter_ = unsmob_iterator (get_iterator (m));
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
Sequential_iterator::next_element (bool)
{
  Moment len =iter_->music_get_length () - iter_->music_start_mom ();
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

  iter_->quit();
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
  Context * child_report = child_report = iter_->get_outlet ();
  Context * me_report = get_outlet ();

  Context * c = child_report;
  while (c && c != me_report)
    {
      c= c->daddy_context_;
    }
  
  if (c == me_report)
    set_translator (child_report);
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
	  iter_->process (iter_->music_get_length ());
	}
      else
	{
	  Moment w = until - here_mom_ + iter_->music_start_mom ();
	  if (w >= Moment (0)) 
	    iter_->process (w);
	}
      
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

bool
Sequential_iterator::run_always () const
{
  return iter_ ? iter_->run_always () : false; 
}
