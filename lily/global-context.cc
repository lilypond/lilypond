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

#include "global-context.hh"

#include "context-def.hh"
#include "cpu-timer.hh"
#include "dispatcher.hh"
#include "international.hh"
#include "music-iterator.hh"
#include "music.hh"
#include "output-def.hh"
#include "warn.hh"

#include <cstdio>

using std::string;

Global_context::Global_context (Output_def *odef, Context_def *cdef)
  : Context (cdef, SCM_EOL)
{
  output_def_ = odef;
  definition_ = cdef->self_scm ();

  now_mom_.main_part_ = -Rational::infinity ();
  prev_mom_.main_part_ = -Rational::infinity ();

  /* We only need the most basic stuff to bootstrap the context tree */
  event_source ()->add_listener (GET_LISTENER (this, create_context_from_event),
                                 ly_symbol2scm ("CreateContext"));
  event_source ()->add_listener (GET_LISTENER (this, prepare),
                                 ly_symbol2scm ("Prepare"));
  events_below ()->register_as_listener (event_source_);

  cdef->apply_default_property_operations (this);
}

void
Global_context::derived_mark () const
{
  if (output_def_)
    scm_gc_mark (output_def_->self_scm ());
}

void
Global_context::add_moment_to_process (Moment m)
{
  if (m < now_mom_)
    programming_error ("trying to freeze in time");

  for (vsize i = 0; i < extra_mom_pq_.size (); i++)
    if (extra_mom_pq_[i] == m)
      return;
  extra_mom_pq_.insert (m);
}

Moment
Global_context::sneaky_insert_extra_moment (Moment w)
{
  while (extra_mom_pq_.size () && extra_mom_pq_.front () <= w)
    w = extra_mom_pq_.get ();
  return w;
}

vsize
Global_context::get_moments_left () const
{
  return extra_mom_pq_.size ();
}

void
Global_context::prepare (SCM sev)
{
  Stream_event *ev = unsmob<Stream_event> (sev);
  Moment *mom = unsmob<Moment> (get_property (ev, "moment"));

  assert (mom);

  if (isinf (prev_mom_.main_part_) && prev_mom_ < 0)
    prev_mom_ = *mom;
  else
    prev_mom_ = now_mom_;
  now_mom_ = *mom;
}

Context *
Global_context::get_score_context () const
{
  return (scm_is_pair (context_list_))
           ? unsmob<Context> (scm_car (context_list_))
           : 0;
}

SCM
Global_context::get_output ()
{
  Context *c = get_score_context ();
  if (c)
    return get_property (c, "output");
  else
    return SCM_EOL;
}

bool
Global_context::iterate (Music *music, bool force_found_music)
{
  Cpu_timer timer;

  SCM protected_iter = Music_iterator::create_top_iterator (music);
  auto iter = unsmob<Music_iterator> (protected_iter);

  bool found_music = force_found_music;
  if (!force_found_music)
    {
      const auto &len = iter->music_get_length () - iter->music_start_mom ();
      found_music = len && iter->ok ();
    }

  if (found_music)
    {
      prev_mom_.main_part_ = -Rational::infinity ();
      now_mom_.main_part_ = -Rational::infinity ();
      auto final_mom = iter->music_get_length ();
      if (isinf (final_mom.main_part_))
        {
          // The top-level music is LyricCombineMusic or something similar.
          music->warning (_ ("cannot determine music length"));

          // Lyric_combine_music_iterator cannot be trusted to behave such that
          // the loop below will terminate.  We have no use case for
          // indefinite-length music at the top level, so there is no harm in
          // cutting it short.  To understand how things go wrong without this,
          // try regression test lyric-combine-top-level-no-music.ly.
          final_mom = 0;
        }

      // This forces at least one full pass through the loop to initialize
      // contexts even if the iterator has nothing to process.
      add_moment_to_process (0);

      for (bool first = true; true; first = false)
        {
          Moment w = iter->pending_moment ();
          // write out iter->ok () to save a call to pending_moment ()
          const bool ok = (w < Moment::infinity ()) || iter->run_always ();

          w = sneaky_insert_extra_moment (w);
          if (w > final_mom)
            break;

          if (w == prev_mom_)
            {
              programming_error ("Moment is not increasing."
                                 "  Aborting interpretation.");
              break;
            }

          send_stream_event (this, "Prepare", 0, ly_symbol2scm ("moment"),
                             w.smobbed_copy ());

          if (first)
            iter->init_context (this);

          if (ok)
            iter->process (w);

          send_stream_event (this, "OneTimeStep", 0);
          apply_finalizations ();
          check_removal ();
        }

      iter->quit ();
      scm_remember_upto_here_1 (protected_iter);
      check_removal ();
      send_stream_event (this, "Finish", 0);
    }

  debug_output (_f ("elapsed time: %.2f seconds", timer.read ()));
  return found_music;
}

void
Global_context::apply_finalizations ()
{
  SCM lst = get_property (this, "finalizations");
  set_property (this, "finalizations", SCM_EOL);
  for (SCM s = lst; scm_is_pair (s); s = scm_cdr (s))
    scm_apply_0 (scm_caar (s), scm_cdar (s));
}

/* Add a function to execute before stepping to the next time step.  */
void
Global_context::add_finalization (SCM x)
{
  SCM lst = get_property (this, "finalizations");
  lst = scm_cons (x, lst);
  set_property (this, "finalizations", lst);
}

Moment
Global_context::previous_moment () const
{
  return prev_mom_;
}

Global_context *
find_global_context (Context *where)
{
  if (!where)
    return nullptr;
  if (auto g = dynamic_cast<Global_context *> (find_top_context (where)))
    return g;
  programming_error ("no global context");
  abort ();
}
