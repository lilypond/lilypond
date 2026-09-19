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
  scm_gc_mark (uniterated_music_);
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

  uniterated_music_
    = Calculated_sequential_music::calc_elements (get_music ());

  pop_element ();
  // If we don't have anything to show, be !ok right away
  if (!iter_)
    iter_offset_ = Moment::infinity ();
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
Sequential_iterator::get_iter ()
{
  // This routine fetches the next music to be handled in iteration and does
  // the accounting for it.
  //
  // Things like { c1*0 \grace { d4 } } will become a bit wild.  c1*0 may
  // not call itself "loose", but to make progress it has to go anyway, or time
  // will jump backward when it is removed on its own.

  // \grace { c4 } d1*0 e f \grace { d2 } is an even more unpleasant idea.
  // Again, to avoid backward jumps in time (from 0-1/4G to 0-1/2G), we have to
  // be prepared to process all of this in one lump.  So we are selling this as
  // starting time 0-3/4G .  Anybody inputting this kind of music is getting
  // what they deserve; we just do our best and try not getting out of sync.

  // Here is the variable we are maintaining for bookkeeping:
  //
  // iter_offset_ contains the offset the currently iterated music element
  // iter_ entertains to the overall time calculations of the
  // Sequential_iterator.
  //
  // For keeping it in line with the need of upcoming grace-containing music,
  // we maintain the variable unscanned_music_: we set it up to point after the
  // next music with non-zero main part.
  //
  // In a similar vein, we maintain a variable iterated_music_ which will point
  // just beyond the music we are currently iterating.
  //
  // Every iterated music, whether it contains grace components or not, will
  // iterate until reaching its get_length () value.
  //
  // The incoming "until" may reserve space for upcoming grace notes following
  // the sequential expression.  The iteration will take this reservation only
  // into account during the final main moment of the sequence.
  //
  // So in a nutshell, we maintain two appearances: one to the inside
  // iterators, and one as an iterator to the outside.
  //
  // The outside timing lets us end every main time step at zero grace time so
  // that our own grace time slots in perfectly with the progression of outer
  // grace time.
  //
  // This is the same at every time step, and this is the timing we need to
  // provide our inner iterators with.
  //
  // If an inner iterator is not at its final main time step, it is fully
  // synchronized with the outer grace timing.  When an inner iterator is at
  // its final time step, this is different: we now need to adjust its grace
  // timing so that it accommodates upcoming grace.
  //
  // For simple bookkeeping, we always calculate the upcoming grace into our
  // iter_offset_ value, but only take it into account for the inner iterators
  // on their final main time.

  bool first = !iter_;
  if (iter_)
    {
      iter_offset_ += iter_->music_get_length ();
      iter_ = nullptr;
    }

  while (scm_is_pair (uniterated_music_))
    {
      SCM mus_scm = scm_car (uniterated_music_);
      auto *mus = unsmob<Music> (mus_scm);

      uniterated_music_ = scm_cdr (uniterated_music_);

      if (mus)
        {
          iter_ = unsmob<Music_iterator> (create_child (mus));
          scm_remember_upto_here_1 (mus_scm);

          if (first)
            {
              unscanned_music_ = uniterated_music_;
              break;
            }
          iter_offset_ -= iter_->music_start_mom ();
          if (!iter_->music_get_length ().main_part_)
            return;

          break;
        }
    }

  // uniterated_music_ has caught up, let unscanned_music_ advance to the next
  // main_part_ having music
  while (scm_is_pair (unscanned_music_))
    {
      SCM mus_scm = scm_car (unscanned_music_);
      auto *mus = unsmob<Music> (mus_scm);
      unscanned_music_ = scm_cdr (unscanned_music_);

      if (mus)
        {
          iter_offset_ += mus->start_mom ();
          if (mus->get_length ().main_part_)
            return;
        }
      scm_remember_upto_here_1 (mus_scm);
    }
}

// remove the next element to be processed and create an iterator for it. This
// is the only member function creating iterators

void
Sequential_iterator::pop_element ()
{
  get_iter ();

  if (!iter_) // end of sequence
    {
      if (iter_offset_ != music_get_length ())
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
  // process needs to make progress.  Calling a single iterator once in its ok
  // state _is_ sufficient progress.  If we end up being non-ok, we continue
  // with the next music element.

  while (iter_)
    {
      if (iter_->ok ())
        {
          iter_->process (iter_moment (until));
          if (iter_->ok ())
            return;
        }
      descend_to_child (iter_->get_context ());
      iter_->quit ();

      pop_element ();

      if (!iter_)
        {
          next_element (); // let subclasses do certain things
          // If we weren't actually finished, we solicit one more iteration
          if (until < iter_offset_)
            return;

          break;
        }

      iter_->init_context (get_own_context ());

      next_element (); // let subclasses do certain things

      // If the current iterator has been called into being before its nominal
      // starting time, that's it.  We don't move on to the next: that is the
      // job of the simultaneous iterator responsible for having called us
      // early in the context of "loose" processing.

      if (iter_moment (until) < iter_->music_start_mom ())
        return;
    }
  iter_offset_ = Moment::infinity ();
}

// As long as we are not finished, we don't signal to be anywhere behind
// the current iterator in order to never get beyond the start of the
// next one.  That makes sure that callers' consideration of
// pending_moment () does not cause more than a single moment to get
// cleaned up.

// Since parent_time is limited to time conversion by active child iterators,
// we can do all of that limiting inside of parent_time, avoiding duplicated
// checks.

Moment
Sequential_iterator::pending_moment () const
{
  if (!iter_)
    return iter_offset_;
  return parent_time (iter_->pending_moment ());
}

// We cannot really convert timings outside of the main_part_ time span of the
// active iterator anyway, so capping the conversion range for the sake of
// pending_moment () requirements does not further constrain the usability of
// parent_time.
Moment
Sequential_iterator::parent_time (Moment pm) const
{
  assert (iter_);

  Moment iter_len = iter_->music_get_length ();

  if (pm > iter_len)
    pm = iter_len;

  if (pm.main_part_ == iter_len.main_part_)
    return pm + iter_offset_;
  else
    return pm + iter_offset_.main_part_;
}

IMPLEMENT_CTOR_CALLBACK (Sequential_iterator);

bool
Sequential_iterator::run_always () const
{
  return iter_ ? iter_->run_always () : false;
}

bool
Sequential_iterator::loose () const
{
  return !iter_ || iter_->loose ();
}
