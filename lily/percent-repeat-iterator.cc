/*
  percent-repeat-iterator.cc -- implement Percent_repeat_iterator

  source file of the GNU LilyPond music typesetter

  (c) 2001--2006  Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "percent-repeat-iterator.hh"

#include "input.hh"
#include "international.hh"
#include "repeated-music.hh"

IMPLEMENT_CTOR_CALLBACK (Percent_repeat_iterator);

Percent_repeat_iterator::Percent_repeat_iterator ()
{
  child_iter_ = 0;
}

void
Percent_repeat_iterator::do_quit ()
{
  if (child_iter_)
    child_iter_->quit ();
}

bool
Percent_repeat_iterator::ok () const
{
  return child_iter_;
}

void
Percent_repeat_iterator::construct_children ()
{
  Music *mus = get_music ();
  finish_mom_ = mus->get_length ();
  child_iter_ = unsmob_iterator (get_iterator (Repeated_music::body (mus)));
}

void
Percent_repeat_iterator::process (Moment m)
{
  if (!m.to_bool ())
    {
      Music_iterator *yeah = try_music (get_music ());
      if (yeah)
	set_context (yeah->get_outlet ());
      else
	get_music ()->origin ()->warning (_ ("no one to print a percent"));
    }

  if (child_iter_->ok ())
    child_iter_->process (m);

  if (finish_mom_ <= m)
    {
      child_iter_->quit ();
      child_iter_ = 0;
    }
}

Moment
Percent_repeat_iterator::pending_moment ()const
{
  if (child_iter_->ok ())
    return child_iter_->pending_moment ();
  else
    return finish_mom_;
}

Music_iterator *
Percent_repeat_iterator::try_music_in_children (Music *m) const
{
  return child_iter_->try_music (m);
}

void
Percent_repeat_iterator::derived_mark ()const
{
  if (child_iter_)
    scm_gc_mark (child_iter_->self_scm ());
}

void
Percent_repeat_iterator::derived_substitute (Context *f, Context *t)
{
  if (child_iter_)
    child_iter_->substitute_outlet (f, t);
}
