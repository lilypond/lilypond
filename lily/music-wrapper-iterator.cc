/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
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
Music_wrapper_iterator::preorder_walk (
  const std::function<void (Music_iterator *)> &visit)
{
  Music_iterator::preorder_walk (visit);
  if (child_iter_)
    child_iter_->preorder_walk (visit);
}

void
Music_wrapper_iterator::create_children ()
{
  Music_iterator::create_children ();

  if (auto m = unsmob<Music> (get_property (get_music (), "element")))
    {
      SCM it_scm = create_child (m);
      child_iter_ = unsmob<Music_iterator> (it_scm);
    }
}

void
Music_wrapper_iterator::create_contexts ()
{
  Music_iterator::create_contexts ();
  if (child_iter_)
    child_iter_->init_context (get_own_context ());
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

Context *
Music_wrapper_iterator::get_context () const
{
  if (child_iter_)
    return child_iter_->get_context ();

  // We could fall back on returning this wrapper's context, but there is no
  // good reason for it.  The code creating the hierarchy knows whether the
  // wrapped iterator is supposed to have been created yet, and it can call
  // get_own_context () if it hasn't.
  return nullptr;
}

void
Music_wrapper_iterator::set_context (Context *trans)
{
  if (child_iter_)
    child_iter_->set_context (trans);

  // Is keeping the context of the wrapper in sync with the wrapped iterator
  // just paranoia?  I haven't found a case that demonstrates its necessity.
  // Still, I hesitate to remove it. [DE]
  Music_iterator::set_context (trans);
}

IMPLEMENT_CTOR_CALLBACK (Music_wrapper_iterator);

bool
Music_wrapper_iterator::run_always () const
{
  return (child_iter_ && child_iter_->run_always ());
}
