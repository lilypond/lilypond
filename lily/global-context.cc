/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "dispatcher.hh"
#include "international.hh"
#include "music-iterator.hh"
#include "music.hh"
#include "output-def.hh"
#include "warn.hh"

#include <cstdio>

using std::string;

Preinit_Global_context::Preinit_Global_context ()
{
  output_def_ = 0;
}

Global_context::Global_context (Output_def *o)
  : Context ()
{
  output_def_ = o;
  definition_ = find_context_def (o, ly_symbol2scm ("Global"));

  now_mom_.set_infinite (-1);
  prev_mom_.set_infinite (-1);

  /* We only need the most basic stuff to bootstrap the context tree */
  event_source ()->add_listener (GET_LISTENER (this, create_context_from_event),
                                 ly_symbol2scm ("CreateContext"));
  event_source ()->add_listener (GET_LISTENER (this, prepare),
                                 ly_symbol2scm ("Prepare"));
  events_below ()->register_as_listener (event_source_);

  Context_def *globaldef = unsmob<Context_def> (definition_);
  if (!globaldef)
    programming_error ("no `Global' context found");
  else
    globaldef->apply_default_property_operations (this);

  acceptance_.accept_default (ly_symbol2scm ("Score"));
}

void
Global_context::derived_mark () const
{
  if (output_def_)
    scm_gc_mark (output_def_->self_scm ());
}

Output_def *
Global_context::get_output_def () const
{
  return output_def_;
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

  if (prev_mom_.main_part_.is_infinity () && prev_mom_ < 0)
    prev_mom_ = *mom;
  else
    prev_mom_ = now_mom_;
  now_mom_ = *mom;
}

Moment
Global_context::now_mom () const
{
  return now_mom_;
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

void
Global_context::run_iterator_on_me (Music_iterator *iter)
{
  prev_mom_.set_infinite (-1);
  now_mom_.set_infinite (-1);
  Moment final_mom = iter->get_music ()->get_length ();

  bool first = true;
  while (iter->ok () || get_moments_left ())
    {
      Moment w;
      w.set_infinite (1);
      if (iter->ok ())
        w = iter->pending_moment ();

      w = sneaky_insert_extra_moment (w);
      if (w.main_part_.is_infinity () || w > final_mom)
        break;

      if (w == prev_mom_)
        {
          programming_error ("Moment is not increasing."
                             "  Aborting interpretation.");
          break;
        }

      if (first)
        {
          /*
            Need this to get grace notes at start of a piece correct.
          */
          first = false;
          set_property (this, "measurePosition", w.smobbed_copy ());
        }

      send_stream_event (this, "Prepare", 0,
                         ly_symbol2scm ("moment"), w.smobbed_copy ());

      if (iter->ok ())
        iter->process (w);

      send_stream_event (this, "OneTimeStep", 0);
      apply_finalizations ();
      check_removal ();
    }
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
