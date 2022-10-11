/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "simultaneous-music-iterator.hh"

#include "context.hh"
#include "input.hh"
#include "music.hh"

using std::string;

void
Simultaneous_music_iterator::derived_mark () const
{
  children_list_.gc_mark ();
}

void
Simultaneous_music_iterator::preorder_walk (
  const std::function<void (Music_iterator *)> &visit)
{
  Music_iterator::preorder_walk (visit);
  for (auto *child : children_list_)
    child->preorder_walk (visit);
}

void
Simultaneous_music_iterator::create_children ()
{
  Music_iterator::create_children ();

  children_list_.clear ();
  auto tail = children_list_.begin ();
  const ly_smob_list<Music> elements (get_property (get_music (), "elements"));
  for (auto *mus : elements)
    {
      SCM scm_iter = create_child (mus);
      Music_iterator *mi = unsmob<Music_iterator> (scm_iter);
      tail = children_list_.insert_before (tail, mi);
      ++tail;
      scm_remember_upto_here_1 (scm_iter);
    }
}

void
Simultaneous_music_iterator::create_contexts ()
{
  Music_iterator::create_contexts ();

  auto *my_context = get_context ();
  for (auto proc = children_list_.begin (); proc != children_list_.end ();
       /*in loop*/)
    {
      auto *child = *proc;
      child->init_context (my_context);

      // Why might a newly created iterator not be OK?  An example is a
      // Sequential_iterator with no elements.
      if (!child->ok ())
        {
          child->quit ();
          proc = children_list_.erase_at (proc);
        }
      else
        {
          ++proc;
        }
    }

  // Sequential_iterator follows its children into their contexts.  This
  // iterator does not--which one would it follow?  (Maybe it could track the
  // nearest common ancestor, but that would probably make its behavior even
  // harder to understand.)  At least avoid squatting in Global context.
  descend_to_user_accessible_context ();
}

// If we have some iterators with definite next moment and no of them
// remain after processing, we take the iterators with indefinite next
// moment along.  That makes sure that no Lyric_combine_music_iterator
// will outstay its welcome (issue 2010).

void
Simultaneous_music_iterator::process (Moment until)
{
  bool finite = !isinf (pending_moment ().main_part_);
  for (auto proc = children_list_.begin (); proc != children_list_.end ();
       /*in loop*/)
    {
      auto *child = *proc;
      if (child->run_always () || (child->pending_moment () == until))
        child->process (until);
      if (!child->ok ())
        {
          child->quit ();
          proc = children_list_.erase_at (proc);
        }
      else
        {
          ++proc;
        }
    }
  // If there were definite-ended iterators and all of them died, take
  // the rest of the iterators along with them.  They have
  // likely lost their reference iterators.  Basing this on the actual
  // music contexts is not reliable since something like
  // \new Voice = blah {
  //    << \context Voice = blah { c4 d }
  //       \addlyrics { oh no }
  //    >> e f
  // }
  // cannot wait for the death of context blah before ending the
  // simultaneous iterator.
  if (finite && isinf (pending_moment ().main_part_))
    {
      for (auto *child : children_list_)
        child->quit ();
      children_list_.clear ();
    }
}

Moment
Simultaneous_music_iterator::pending_moment () const
{
  auto next = Moment::infinity ();

  for (const auto *child : children_list_)
    {
      next = std::min (next, child->pending_moment ());
    }

  return next;
}

bool
Simultaneous_music_iterator::run_always () const
{
  for (const auto *child : children_list_)
    {
      if (child->run_always ())
        return true;
    }
  return false;
}

void
Simultaneous_music_iterator::do_quit ()
{
  for (auto *child : children_list_)
    child->quit ();
}

IMPLEMENT_CTOR_CALLBACK (Simultaneous_music_iterator);
