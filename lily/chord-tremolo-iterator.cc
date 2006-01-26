/*
  chord-tremolo-iterator.cc -- implement Chord_tremolo_iterator

  source file of the GNU LilyPond music typesetter

  (c) 2000--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "chord-tremolo-iterator.hh"

#include "input.hh"
#include "international.hh"
#include "repeated-music.hh"

void
Chord_tremolo_iterator::construct_children ()
{
  Music *m = get_music ();

  factor_ = Moment (Rational (1, 1));
  child_iter_ = unsmob_iterator (get_iterator (Repeated_music::body (m)));
}

Chord_tremolo_iterator::Chord_tremolo_iterator ()
{
  factor_ = 1;
  child_iter_ = 0;
}

void
Chord_tremolo_iterator::do_quit ()
{
  if (child_iter_)
    child_iter_->quit ();
}

void
Chord_tremolo_iterator::derived_mark () const
{
  if (child_iter_)
    scm_gc_mark (child_iter_->self_scm ());
}

void
Chord_tremolo_iterator::derived_substitute (Context *f, Context *t)
{
  if (child_iter_)
    child_iter_->substitute_outlet (f, t);
}

void
Chord_tremolo_iterator::process (Moment m)
{
  if (!m.to_bool ())
    {
      Music_iterator *yeah = try_music (get_music ());
      if (yeah)
	set_context (yeah->get_outlet ());
      else
	get_music ()->origin ()->warning (_ ("no one to print a tremolos"));
    }

  child_iter_->process (factor_ * m);
}

Moment
Chord_tremolo_iterator::pending_moment () const
{
  return child_iter_->pending_moment () / factor_;
}

bool
Chord_tremolo_iterator::ok () const
{
  return child_iter_ && child_iter_->ok ();
}

Music_iterator *
Chord_tremolo_iterator::try_music_in_children (Music *m) const
{
  return child_iter_->try_music (m);
}

IMPLEMENT_CTOR_CALLBACK (Chord_tremolo_iterator);

