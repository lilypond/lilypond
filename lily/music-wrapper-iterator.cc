/*
  music-wrapper-iterator.cc -- implement Music_wrapper_iterator

  source file of the GNU LilyPond music typesetter

  (c) 1998--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "music-wrapper-iterator.hh"
#include "music-wrapper.hh"
#include "music.hh"

Music_wrapper_iterator::Music_wrapper_iterator ()
{
  child_iter_ = 0;
}

void
Music_wrapper_iterator::do_quit ()
{
  if (child_iter_)
    child_iter_->quit ();
}

void
Music_wrapper_iterator::derived_mark () const
{
  if (child_iter_)
    scm_gc_mark (child_iter_->self_scm ());
}

void
Music_wrapper_iterator::derived_substitute (Context *f, Context *t)
{
  if (child_iter_)
    child_iter_->substitute_outlet (f, t);
}

void
Music_wrapper_iterator::construct_children ()
{
  Music *my_music = get_music ();
  Music *child = unsmob_music (my_music->get_property ("element"));
  child_iter_ = (child)
    ? unsmob_iterator (get_iterator (child))
    : 0;
}

bool
Music_wrapper_iterator::ok () const
{
  return child_iter_ && child_iter_->ok ();
}

void
Music_wrapper_iterator::process (Moment m)
{
  if (child_iter_)
    child_iter_->process (m);
}

Moment
Music_wrapper_iterator::pending_moment () const
{
  if (child_iter_)
    return child_iter_->pending_moment ();
  else
    return Music_iterator::pending_moment ();
}

IMPLEMENT_CTOR_CALLBACK (Music_wrapper_iterator);

bool
Music_wrapper_iterator::run_always () const
{
  return (child_iter_ &&  child_iter_->run_always ());
}
