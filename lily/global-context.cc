/*
  global-translator.cc -- implement global_context

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include <stdio.h>

#include "warn.hh"
#include "music.hh"
#include "event.hh"
#include "music-list.hh"
#include "music-iterator.hh"
#include "global-context.hh"
#include "score-context.hh"
#include "context-def.hh"
#include "music-output-def.hh"
#include "grace-fixup.hh"

Global_context::Global_context (Music_output_def *o, Moment final)
{
  output_def_ = o;
  final_mom_ = final;
  definition_ = o->find_context_def (ly_symbol2scm ("Global"));
  unsmob_context_def (definition_)->apply_default_property_operations (this);
  accepts_list_ = scm_list_1 (ly_symbol2scm ("Score"));
}

Music_output_def* 
Global_context::get_output_def () const
{
  return output_def_;
}

void
Global_context::add_moment_to_process (Moment m)
{
  if (m > final_mom_)
    return;

  if (m < now_mom_)
    programming_error ("Trying to freeze in time.");
  
  for (int i=0; i <  extra_mom_pq_.size (); i++)
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

void
Global_context::prepare (Moment m)
{
  prev_mom_  = now_mom_;
  now_mom_ = m;

  if (get_score_context ())
    get_score_context ()->prepare (m);
}

Moment
Global_context::now_mom () const
{
  return now_mom_;
}

Score_context*
Global_context::get_score_context () const
{
  return (ly_pair_p (context_list_))
    ? dynamic_cast<Score_context*> (unsmob_context (ly_car (context_list_)))
    : 0;
}

Music_output*
Global_context::get_output ()
{
  return get_score_context ()->get_output ();
}

void
Global_context::one_time_step ()
{
  get_score_context ()->one_time_step ();
  apply_finalizations ();
  check_removal ();
}

void
Global_context::finish ()
{
  if (get_score_context ())
    get_score_context ()->finish ();
}

void
Global_context::run_iterator_on_me (Music_iterator * iter)
{
  if (iter-> ok ())
    prev_mom_ = now_mom_ = iter->pending_moment ();

  bool first = true;
  while (iter->ok () || get_moments_left ())
    {
      Moment w;
      w.set_infinite (1);
      if (iter->ok ())
	{
	  w = iter->pending_moment ();
	}

      w = sneaky_insert_extra_moment (w);
      if (w.main_part_.is_infinity ())
	break ;
      
      if (first)
        {
	  /*
	    Need this to get grace notes at start of a piece correct.
	   */
          first = false;
          set_property ("measurePosition", w.smobbed_copy ());
        }


      prepare (w);

      if (iter->ok ())
	iter->process (w);

      if (!get_score_context ()) 
	{
	  SCM key = ly_symbol2scm ("Score");
	  Context_def * t = unsmob_context_def (get_output_def ()
						->find_context_def (key));
	  if (!t)
	    error (_f ("can't find `%s' context", "Score"));

	  Context *c = t->instantiate (SCM_EOL);
	  add_context (c);

	  Score_context *sc = dynamic_cast<Score_context*> (c);
	  sc->prepare (w);
	}
      
      one_time_step ();
    }
}

void
Global_context::apply_finalizations ()
{
  SCM lst = get_property ("finalizations");
  set_property ("finalizations" , SCM_EOL); 
  for (SCM s = lst ; ly_pair_p (s); s = ly_cdr (s))
    {
      scm_primitive_eval (ly_car (s)); // TODO: make safe.
    }
}

/*
  Add a function to execute before stepping to the next time step.
*/
void
Global_context::add_finalization (SCM x)
{
  SCM lst = get_property ("finalizations");
  lst = scm_cons (x, lst);
  set_property ("finalizations" ,lst); 
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
