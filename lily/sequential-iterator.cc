/*
  sequential-iterator.cc -- implement Sequential_iterator

  source file of the GNU LilyPond music typesetter

  (c) 1997--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "sequential-iterator.hh"
#include "music.hh"
#include "translator-group.hh"
#include "context.hh"
#include "grace-fixup.hh"

/*
  TODO: handling of grace notes is exquisite pain.  This handling
  should be formally specified and then the implementation verified.
*/

/*
  Invariant for the data structure.


  if (scm_is_pair (cursor_))
  iter_->music_ == unsmob_music (scm_car (cursor_))
  else
  iter_ == 0;

  The length of musiclist from start to up to cursor_ (cursor_ not
  including), is summed

  here_mom_  = sum (length (musiclist [start ... cursor>))  %)
*/
Sequential_iterator::Sequential_iterator ()
{
  here_mom_ = Moment (0);
  cursor_ = SCM_EOL;
  grace_fixups_ = 0;
  iter_ = 0;
}

SCM
Sequential_iterator::get_music_list () const
{
  Music *m = get_music ();
  SCM proc = m->get_property ("elements-callback");
  if (scm_procedure_p (proc))
    return scm_call_1 (proc, m->self_scm ());
  else
    return SCM_EOL;
}

void
Sequential_iterator::do_quit ()
{
  if (iter_)
    iter_->quit ();
}

void
Sequential_iterator::derived_mark () const
{
  if (iter_)
    scm_gc_mark (iter_->self_scm ());
  scm_gc_mark (cursor_);
}

void
Sequential_iterator::derived_substitute (Context *f, Context *t)
{
  if (iter_)
    iter_->substitute_outlet (f, t);
}

/*
  TODO: this should be made lazily.
*/
Grace_fixup *
create_grace_fixup_list (SCM cursor)
{
  Moment here;
  Moment last (-1);
  Grace_fixup *head = 0;
  Grace_fixup **tail = &head;

  for (; scm_is_pair (cursor); cursor = scm_cdr (cursor))
    {
      Music *mus = unsmob_music (scm_car (cursor));
      Moment s = mus->start_mom ();
      Moment l = mus->get_length () - s;

      if (s.grace_part_)
	{
	  if (last != Moment (-1))
	    {
	      Grace_fixup *p = new Grace_fixup;
	      p->start_ = last;
	      p->length_ = here - last;
	      p->grace_start_ = s.grace_part_;
	      p->next_ = 0;
	      *tail = p;
	      tail = &(*tail)->next_;
	    }

	  here.grace_part_ = s.grace_part_;
	}

      if (l.to_bool ())
	{
	  last = here;
	  here += l;
	}
    }

  return head;
}

void
Sequential_iterator::construct_children ()
{
  cursor_ = get_music_list ();

  iter_ = 0;
  if (scm_is_pair (cursor_))
    {
      Music *m = unsmob_music (scm_car (cursor_));
      iter_ = unsmob_iterator (get_iterator (m));
    }

  while (iter_ && !iter_->ok ())
    next_element (true);

  last_mom_ = Moment (-1);
  here_mom_ = get_music ()->start_mom ();
  grace_fixups_ = create_grace_fixup_list (cursor_);

  /*
    iter_->ok () is tautology, but what the heck.
  */
  if (iter_ && iter_->ok ())
    descend_to_child (iter_->get_outlet ());
}

/*
  maintain invariants: change cursor, iter and here_mom_ in one fell
  swoop.
*/
void
Sequential_iterator::next_element (bool)
{
  Moment len = iter_->music_get_length () - iter_->music_start_mom ();
  assert (!grace_fixups_ || grace_fixups_->start_ >= here_mom_);

  if (len.main_part_
      && get_grace_fixup ())
    {
      Grace_fixup *gf = get_grace_fixup ();

      last_mom_ = here_mom_;
      here_mom_ += gf->length_;
      here_mom_.grace_part_ += gf->grace_start_;

      next_grace_fixup ();
    }
  else if (len.grace_part_ && !len.main_part_)
    {
      last_mom_ = here_mom_;
      here_mom_.grace_part_ = 0;
    }
  else
    {
      /*
	!len.grace_part_ || len.main_part_

	We skip over a big chunk (mainpart != 0). Any starting graces
	in that chunk should be in len.grace_part_

      */
      last_mom_ = here_mom_;
      here_mom_ += len;
    }

  cursor_ = scm_cdr (cursor_);

  iter_->quit ();
  if (scm_is_pair (cursor_))
    iter_ = unsmob_iterator (get_iterator (unsmob_music (scm_car (cursor_))));
  else
    iter_ = 0;
}

void
Sequential_iterator::process (Moment until)
{
  while (iter_)
    {
      Grace_fixup *gf = get_grace_fixup ();
      if (gf
	  && gf->start_ + gf->length_
	  + Moment (Rational (0), gf->grace_start_) == until)
	{
	  /*
	    do the stuff/note/rest preceding a grace.
	  */
	  iter_->process (iter_->music_get_length ());
	}
      else
	{
	  Moment w = until - here_mom_ + iter_->music_start_mom ();
	  iter_->process (w);
	}

      /*
	if the iter is still OK, there must be events left that have

	TIME > LEFT

      */
      if (iter_->ok ())
	return;

      descend_to_child (iter_->get_outlet ());
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
  Grace_fixup *gf = get_grace_fixup ();
  if (gf
      && gf->length_ + iter_->music_start_mom () == cp)
    return here_mom_ + gf->length_ + Moment (0, gf->grace_start_);

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

IMPLEMENT_CTOR_CALLBACK (Sequential_iterator);

bool
Sequential_iterator::run_always () const
{
  return iter_ ? iter_->run_always () : false;
}

void
Sequential_iterator::next_grace_fixup ()
{
  Grace_fixup *n = grace_fixups_->next_;
  delete grace_fixups_;
  grace_fixups_ = n;
}

Grace_fixup *
Sequential_iterator::get_grace_fixup () const
{
  if (grace_fixups_ && grace_fixups_->start_ == here_mom_)
    return grace_fixups_;
  else
    return 0;
}
