/*
  score-performer.cc -- implement Score_performer

  source file of the GNU LilyPond music typesetter

  (c) 1996--2006 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "score-performer.hh"

#include "audio-column.hh"
#include "audio-item.hh"
#include "context-def.hh"
#include "context.hh"
#include "dispatcher.hh"
#include "global-context.hh"
#include "performance.hh"
#include "midi-stream.hh"
#include "moment.hh"
#include "output-def.hh"
#include "string-convert.hh"
#include "warn.hh"

ADD_TRANSLATOR_GROUP (Score_performer,
		      /* doc */ "",
		      /* create */ "",
		      /* accept */ "",
		      /* read */ "",
		      /* write */ "");

Score_performer::Score_performer ()
{
  performance_ = 0;
  skipping_ = false;
}

Score_performer::~Score_performer ()
{
}

void
Score_performer::play_element (Audio_element *p)
{
  if (Audio_item *i = dynamic_cast<Audio_item *> (p))
    audio_column_->add_audio_item (i);
  performance_->add_element (p);
}

void
Score_performer::announce_element (Audio_element_info info)
{
  announce_infos_.push_back (info);
}

void
Score_performer::connect_to_context (Context *c)
{
  Performer_group::connect_to_context (c);
  
  Dispatcher *d = c->get_global_context ()->event_source ();
  d->add_listener (GET_LISTENER (one_time_step), ly_symbol2scm ("OneTimeStep"));
  d->add_listener (GET_LISTENER (prepare), ly_symbol2scm ("Prepare"));
  d->add_listener (GET_LISTENER (finish), ly_symbol2scm ("Finish"));
}

void
Score_performer::disconnect_from_context ()
{
  Dispatcher *d = context ()->get_global_context ()->event_source ();
  d->remove_listener (GET_LISTENER (one_time_step), ly_symbol2scm ("OneTimeStep"));
  d->remove_listener (GET_LISTENER (prepare), ly_symbol2scm ("Prepare"));
  d->remove_listener (GET_LISTENER (finish), ly_symbol2scm ("Finish"));

  Performer_group::disconnect_from_context ();
}

IMPLEMENT_LISTENER (Score_performer, prepare);
void
Score_performer::prepare (SCM sev)
{
  Stream_event *ev = unsmob_stream_event (sev);
  SCM sm = ev->get_property ("moment");
  Moment *m = unsmob_moment (sm);
  audio_column_ = new Audio_column (*m);
  play_element (audio_column_);
  precomputed_recurse_over_translators (context (), START_TRANSLATION_TIMESTEP, UP);
}

IMPLEMENT_LISTENER (Score_performer, finish);
void
Score_performer::finish (SCM)
{
  recurse_over_translators (context (),
			    &Translator::finalize,
			    &Translator_group::finalize,
			    UP);
}

IMPLEMENT_LISTENER (Score_performer, one_time_step);
void
Score_performer::one_time_step (SCM)
{
  if (to_boolean (context ()->get_property ("skipTypesetting")))
    {
      if (!skipping_)
        {
	  skip_start_mom_ = audio_column_->at_mom ();
	  skipping_ = true;
        }
    }
  else
    {
      if (skipping_)
        {
	  offset_mom_ -= audio_column_->at_mom () - skip_start_mom_;
	  skipping_ = false;
	}

      audio_column_->offset_at_mom (offset_mom_);
      precomputed_recurse_over_translators (context (), PROCESS_MUSIC, UP);
      do_announces ();
    }

  precomputed_recurse_over_translators (context (), STOP_TRANSLATION_TIMESTEP, UP);
}

void
Score_performer::derived_mark () const
{
  if (performance_)
    scm_gc_mark (performance_->self_scm ());

  Performer_group::derived_mark ();
}

void
Score_performer::initialize ()
{
  performance_ = new Performance;
  performance_->unprotect ();
  context ()->set_property ("output", performance_->self_scm ()); 
  performance_->midi_ = context ()->get_output_def ();

  Translator_group::initialize ();
}
