/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include <cstdio>
using namespace std;

#include "context-def.hh"
#include "dispatcher.hh"
#include "international.hh"
#include "music-iterator.hh"
#include "music.hh"
#include "output-def.hh"
#include "warn.hh"

Global_context::Global_context (Output_def *o)
  : Context ()
{
  output_def_ = o;
  definition_ = find_context_def (o, ly_symbol2scm ("Global"));

  now_mom_.set_infinite (-1);
  prev_mom_.set_infinite (-1);

  /* We only need the most basic stuff to bootstrap the context tree */
  event_source ()->add_listener (GET_LISTENER (create_context_from_event),
                                ly_symbol2scm ("CreateContext"));
  event_source ()->add_listener (GET_LISTENER (prepare),
                                ly_symbol2scm ("Prepare"));
  events_below ()->register_as_listener (event_source_);

  Context_def *globaldef = unsmob_context_def (definition_);
  if (!globaldef)
    programming_error ("no `Global' context found");
  else
    globaldef->apply_default_property_operations (this);
  accepts_list_ = scm_list_1 (ly_symbol2scm ("Score"));
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

int
Global_context::get_moments_left () const
{
  return extra_mom_pq_.size ();
}

IMPLEMENT_LISTENER (Global_context, prepare);
void
Global_context::prepare (SCM sev)
{
  Stream_event *ev = unsmob_stream_event (sev);
  Moment *mom = unsmob_moment (ev->get_property ("moment"));

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
    ? unsmob_context (scm_car (context_list_))
    : 0;
}

SCM
Global_context::get_output ()
{
  Context * c = get_score_context ();
  if (c)
    return c->get_property ("output");
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
	  programming_error ("Moment is not increasing. Aborting interpretation.");
	  break ;
	}

      
      if (first)
	{
	  /*
	    Need this to get grace notes at start of a piece correct.
	  */
	  first = false;
	  set_property ("measurePosition", w.smobbed_copy ());
	}

      send_stream_event (this, "Prepare", 0,
			 ly_symbol2scm ("moment"), w.smobbed_copy ());

      if (iter->ok ())
	iter->process (w);

      send_stream_event (this, "OneTimeStep", 0, 0);
      apply_finalizations ();
      check_removal ();
    }
}

void
Global_context::apply_finalizations ()
{
  SCM lst = get_property ("finalizations");
  set_property ("finalizations", SCM_EOL);
  for (SCM s = lst; scm_is_pair (s); s = scm_cdr (s))

    /* TODO: make safe.  */
    scm_primitive_eval (scm_car (s));
}

/* Add a function to execute before stepping to the next time step.  */
void
Global_context::add_finalization (SCM x)
{
  SCM lst = get_property ("finalizations");
  lst = scm_cons (x, lst);
  set_property ("finalizations", lst);
}

Moment
Global_context::previous_moment () const
{
  return prev_mom_;
}

Context *
Global_context::get_default_interpreter ()
{
  if (get_score_context ())
    return get_score_context ()->get_default_interpreter ();
  else
    return Context::get_default_interpreter ();
}

Global_context *
unsmob_global_context (SCM x)
{
  return dynamic_cast<Global_context *> (unsmob_context (x));
}
