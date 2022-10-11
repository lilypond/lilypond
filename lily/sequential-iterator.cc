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

#include "sequential-iterator.hh"

#include "calculated-sequential-music.hh"
#include "international.hh"
#include "music.hh"
#include "music-sequence.hh"
#include "context.hh"
#include "warn.hh"

#include <algorithm>

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
  scm_gc_mark (remaining_music_);
}

void
Sequential_iterator::preorder_walk (
  const std::function<void (Music_iterator *)> &visit)
{
  Music_iterator::preorder_walk (visit);
  if (iter_)
    iter_->preorder_walk (visit);
}

void
Sequential_iterator::create_children ()
{
  Music_iterator::create_children ();

  remaining_music_ = Calculated_sequential_music::calc_elements (get_music ());
  ahead_music_ = remaining_music_;
  iter_start_mom_ = music_start_mom ();
  ahead_mom_ = iter_start_mom_;

  pop_element ();
}

void
Sequential_iterator::create_contexts ()
{
  Music_iterator::create_contexts ();

  if (iter_)
    {
      iter_->init_context (get_own_context ());
      descend_to_child (iter_->get_context ());
    }
}

void
Sequential_iterator::look_ahead ()
{
  // Move past elements that have no main duration, then move past the first
  // one with duration.
  while (scm_is_pair (ahead_music_))
    {
      auto *mus = unsmob<Music> (scm_car (ahead_music_));
      ahead_music_ = scm_cdr (ahead_music_);

      if (mus) // paranoia; other things should have complained already
        {
          const auto &end_mom = mus->get_length ();
          if (end_mom.main_part_ > 0)
            {
              ahead_mom_.main_part_ += end_mom.main_part_;
              break;
            }
        }
    }

  // The current state is similar to the initial state of sequential music
  // before it has called the start-callback, now with fewer elements.
  const auto &start_mom = Music_sequence::first_start (ahead_music_);
  ahead_mom_.grace_part_ = start_mom.grace_part_;
  // we keep the accumulated main part
}

// remove the next element to be processed and create an iterator for it
void
Sequential_iterator::pop_element ()
{
  iter_ = nullptr;

  const auto have_ready_music
    = [this] { return !scm_is_eq (remaining_music_, ahead_music_); };

  if (have_ready_music () || (look_ahead (), have_ready_music ()))
    {
      SCM mus_scm = scm_car (remaining_music_); // pop the next element
      remaining_music_ = scm_cdr (remaining_music_);

      if (!scm_is_pair (remaining_music_)) // that was the last one
        ahead_mom_ = Moment::infinity ();

      if (auto *mus = unsmob<Music> (mus_scm))
        {
          iter_ = unsmob<Music_iterator> (create_child (mus));
          scm_remember_upto_here_1 (mus_scm);
        }
    }

  if (!iter_) // end of sequence
    {
      if (iter_start_mom_ != music_get_length ())
        {
          // Maybe a callback provided music inconsistent with the
          // precomputed length.
          // TODO: It might be nice to log the actual music that was
          // iterated in a debug message.
          warning (_ ("total length of sequential music elements "
                      "is different than anticipated"));
        }
    }
}

void
Sequential_iterator::process (Moment until)
{
  while (iter_)
    {
      // moments in the timeline of this sequence
      const auto iter_zero = iter_start_mom_ - iter_->music_start_mom ();
      const auto iter_end = iter_zero + iter_->music_get_length ();

      if (iter_->ok ())
        {
          // When it is time to advance the main part, we try to finish all
          // prior elements, even if it is before their time when grace notes
          // are considered.
          const bool fast_forward = (ahead_mom_ <= until);
          const auto &proc_mom = fast_forward ? iter_end : until;
          iter_->process (proc_mom - iter_zero);
          if (iter_->ok ())
            return;
        }

      const auto &next_mom = std::min (iter_end, ahead_mom_);
      if ((until < next_mom) && (next_mom < Moment::infinity ()))
        {
          // iter_ is !ok earlier than the length of its music predicts.
          // Mitigate by waiting until the expected time so that the next
          // element starts in sync.
          iter_->warning (_ ("music is shorter than anticipated"));
          return;
        }

      iter_start_mom_ = next_mom;
      descend_to_child (iter_->get_context ());
      iter_->quit ();
      iter_ = nullptr;

      pop_element ();
      if (iter_)
        iter_->init_context (get_own_context ());

      next_element (); // let subclasses do certain things
    }

  iter_start_mom_ = until;
}

Moment
Sequential_iterator::pending_moment () const
{
  if (!iter_)
    {
      // Defensive: If for any reason we haven't advanced the full length of
      // the music, stay alive until the end to help keep things in sync.
      // Normally, we'll skip to infinity here.
      const auto &end = music_get_length ();
      return (iter_start_mom_ < end) ? end : Moment::infinity ();
    }

  // moments in the timeline of this sequence
  const auto iter_zero = iter_start_mom_ - iter_->music_start_mom ();
  const auto iter_end = iter_zero + iter_->music_get_length ();
  const auto iter_pend = iter_zero + iter_->pending_moment ();

  // Don't overshoot either of these.  The current element's ending time might
  // fall within a span of grace notes that ahead_mom_ already looks beyond.
  // ahead_mom_ might account for grace notes that need to borrow time from the
  // current element.
  const auto &next_mom = std::min (iter_end, ahead_mom_);
  return std::min (iter_pend, next_mom);
}

IMPLEMENT_CTOR_CALLBACK (Sequential_iterator);

bool
Sequential_iterator::run_always () const
{
  return iter_ ? iter_->run_always () : false;
}
