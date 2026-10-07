/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2026 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
  bool only_run_always = true;

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
          if (only_run_always)
            only_run_always = child->run_always ();
          ++proc;
        }
    }

  // We make the decision of whether this iterator is run_always (following
  // each outside timestep) or not (timing only controlled by finite iterators
  // inside) after weeding out iterators that are not ok from the get-go, like
  // one for (make-music 'Music).
  run_always_ = only_run_always && !children_list_.empty ();

  // Sequential_iterator follows its children into their contexts.  This
  // iterator does not--which one would it follow?  (Maybe it could track the
  // nearest common ancestor, but that would probably make its behavior even
  // harder to understand.)  At least avoid squatting in Global context.
  descend_to_user_accessible_context ();
}

void
Simultaneous_music_iterator::process (Moment until)
{
  for (auto proc = children_list_.begin (); proc != children_list_.end ();
       /*in loop*/)
    {
      auto *child = *proc;
      // Loose processing is reduced to grace time, which also means that its
      // pitfalls don't affect anything else.
      Moment pm = child->pending_moment ();

      if (until.grace_part_ && child->loose ()
          && pm.main_part_ == until.main_part_
          && pm.grace_part_ > until.grace_part_)
        {
          child->process (until);
          // Since the Sequential_iterator does not cater for loose processing,
          // it falls to this iterator's tasks to keep processing an iterator
          // until only non-loose elements remain.  So we don't advance proc
          // here.
          if (child->ok ())
            continue;
        }
      // Other elements get one iteration at a time.
      else if (child->run_always () || pm == until)
        {
          child->process (until);
          if (child->ok ())
            {
              ++proc;
              continue;
            }
        }
      else
        {
          // If a child has turned to finite behavior, we stop exporting
          // run_always_
          if (run_always_ && !child->run_always ())
            run_always_ = false;
          ++proc;
          continue;
        }
      child->quit ();
      proc = children_list_.erase_at (proc);
    }
  if (children_list_.empty ())
    run_always_ = false; // allows the iterator to become !ok ()
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

// The simultaneous iterator does _not_ export run_always unless there is no
// non-run_always expression inside.  This has several consequences.  It means
// that the inside of the Simultaneous_music_iterator does not get additional
// process_moment calls whenever the timing _outside_ of it advances.  It also
// means that the iterator does not need to keep track of when the last finite
// track inside of it quits: when it does, pending_moment will move to infinity
// (since that is the default for the remaining run_always tracks) and
// consequently ok () will return false (it only stays true when the iterator
// itself is run_always, but we explicitly say it is not).  So this will cause
// the Simultaneous_music_iterator to get terminated regularly from its caller.
//
// Downside: a << >> construct containing only run_always iterators needs
// special treatment in order not to get terminated on the spot.
//
// Upside: this avoids every run_always iterator to run essentially at
// fine-grained global timing rather than have its process_moment moments only
// depend on what is happening inside its Simultaneous_music_iterator
// confinement.

bool
Simultaneous_music_iterator::run_always () const
{
  return run_always_;
}

bool
Simultaneous_music_iterator::loose () const
{
  // Any loose child renders the expression as such loose but only if it is in
  // the next upcoming _main_ timestep.
  Moment earliest = Moment::infinity ();
  bool is_loose = true;
  for (const auto *child : children_list_)
    {
      Moment pm = child->pending_moment ();

      if (pm.main_part_ > earliest.main_part_)
        continue;
      if (pm.main_part_ == earliest.main_part_)
        is_loose = is_loose || child->loose ();
      else
        {
          earliest = pm;
          is_loose = child->loose ();
        }
    }
  return is_loose;
}

void
Simultaneous_music_iterator::do_quit ()
{
  for (auto *child : children_list_)
    child->quit ();
}

IMPLEMENT_CTOR_CALLBACK (Simultaneous_music_iterator);
