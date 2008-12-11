/*
  grace-music.cc -- implement Grace_music

  source file of the GNU LilyPond music typesetter

  (c) 1999--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "grace-iterator.hh"
#include "global-context.hh"
#include "warn.hh"

void
Grace_iterator::process (Moment m)
{
  Moment main;
  main.main_part_ = -start_mom_.grace_part_ + m.grace_part_;
  Music_wrapper_iterator::process (main);

  /* We can safely do this, since \grace should always be inside
     sequential.  */
  descend_to_child (child_iter_->get_outlet ());
}

Moment
Grace_iterator::pending_moment () const
{
  Moment cp = Music_wrapper_iterator::pending_moment ();

  Moment pending;
  pending.grace_part_ = start_mom_.grace_part_ + cp.main_part_;

  return pending;
}

IMPLEMENT_CTOR_CALLBACK (Grace_iterator);
