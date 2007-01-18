/*
  folded-repeat-iterator.cc -- implement Folded_repeat_iterator

  source file of the GNU LilyPond music typesetter

  (c) 1999--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "folded-repeat-iterator.hh"

#include "context.hh"
#include "input.hh"
#include "international.hh"
#include "repeated-music.hh"
#include "simultaneous-music-iterator.hh"

Folded_repeat_iterator::Folded_repeat_iterator ()
{
  main_iter_ = 0;
  alternative_iter_ = 0;
}

bool
Folded_repeat_iterator::ok () const
{
  return main_iter_ || alternative_iter_;
}
void
Folded_repeat_iterator::do_quit ()
{
  if (main_iter_)main_iter_->quit ();
  if (alternative_iter_)alternative_iter_->quit ();
}

Moment
Folded_repeat_iterator::pending_moment () const
{
  if (main_iter_)
    return main_iter_->pending_moment ();
  else
    return main_length_mom_ + alternative_iter_->pending_moment ();
}

void
Folded_repeat_iterator::construct_children ()
{
  Music *mus = get_music ();
  main_iter_ = unsmob_iterator (get_iterator (Repeated_music::body (mus)));
  if (!main_iter_->ok ())
    {
      leave_body ();
      enter_alternative ();
    }
}

void
Folded_repeat_iterator::process (Moment m)
{
  if (main_iter_)
    {
      main_iter_->process (m);
      if (!main_iter_->ok ())
	leave_body ();
    }

  if (!main_iter_ && !alternative_iter_)
    enter_alternative ();

  if (alternative_iter_)
    {
      alternative_iter_->process (m - main_length_mom_);
      if (!alternative_iter_->ok ())
	{
	  alternative_iter_->quit ();
	  alternative_iter_ = 0;
	}
    }
}

void
Folded_repeat_iterator::leave_body ()
{
  Music *mus = get_music ();

  main_iter_->quit ();
  main_iter_ = 0;
  main_length_mom_ += Repeated_music::body (mus)->get_length ();
}

void
Folded_repeat_iterator::enter_alternative ()
{
  Music *mus = get_music ();
  if (scm_is_pair (Repeated_music::alternatives (mus)))
    {
      /*
	ugh.
      */
      Simultaneous_music_iterator *s = new Simultaneous_music_iterator;
      s->create_separate_contexts_ = true;
      s->init_context (mus, get_outlet ());

      alternative_iter_ = s;
      alternative_iter_->construct_children ();

      s->unprotect ();
    }
}

void
Folded_repeat_iterator::derived_mark () const
{
  if (main_iter_)
    scm_gc_mark (main_iter_->self_scm ());
  if (alternative_iter_)
    scm_gc_mark (alternative_iter_->self_scm ());
}

void
Folded_repeat_iterator::derived_substitute (Context *f, Context *t)
{
  if (main_iter_)
    main_iter_->substitute_outlet (f, t);
  if (alternative_iter_)
    alternative_iter_->substitute_outlet (f, t);
}

IMPLEMENT_CTOR_CALLBACK (Folded_repeat_iterator);
