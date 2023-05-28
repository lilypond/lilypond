/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2023 Daniel Eble <nine.fierce.ballads@gmail.com>

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

#include "music-iterator.hh"

#include "lily-guile-macros.hh"
#include "music.hh"

// This iterator is a bit like Music_wrapper_iterator, but the only thing it
// does with the wrapped music is set the initial context.
class Initial_context_music_iterator final : public Music_iterator
{
public:
  OVERRIDE_CLASS_NAME (Initial_context_music_iterator);
  DECLARE_SCHEME_CALLBACK (constructor, ());
  Initial_context_music_iterator () = default;

  void derived_mark () const override;
  Moment pending_moment () const override;

protected:
  void create_children () override;
  void create_contexts () override;
  void do_quit () override;

private:
  Music_iterator *child_iter_ = nullptr;
};

void
Initial_context_music_iterator::derived_mark () const
{
  if (child_iter_)
    scm_gc_mark (child_iter_->self_scm ());
}

void
Initial_context_music_iterator::create_children ()
{
  Music_iterator::create_children ();

  if (auto m = unsmob<Music> (get_property (get_music (), "element")))
    {
      SCM it_scm = create_child (m);
      child_iter_ = unsmob<Music_iterator> (it_scm);
      scm_remember_upto_here_1 (it_scm);
    }
}

void
Initial_context_music_iterator::create_contexts ()
{
  Music_iterator::create_contexts ();

  if (child_iter_)
    {
      child_iter_->init_context (get_own_context ());
      set_own_context (child_iter_->get_context ()); // mission accomplished
      child_iter_->quit ();
      child_iter_ = nullptr;
    }
}

Moment
Initial_context_music_iterator::pending_moment () const
{
  return child_iter_ ? Moment () : Moment::infinity ();
}

void
Initial_context_music_iterator::do_quit ()
{
  if (child_iter_)
    child_iter_->quit ();
}

IMPLEMENT_CTOR_CALLBACK (Initial_context_music_iterator);
