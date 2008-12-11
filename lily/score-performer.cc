/*
  score-performer.cc -- implement Score_performer

  source file of the GNU LilyPond music typesetter

  (c) 1996--2008 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "score-performer.hh"

#include "audio-column.hh"
#include "audio-item.hh"
#include "context-def.hh"
#include "dispatcher.hh"
#include "global-context.hh"
#include "performance.hh"
#include "midi-stream.hh"
#include "output-def.hh"
#include "string-convert.hh"
#include "warn.hh"
#include "audio-staff.hh"
#include "audio-item.hh"

ADD_TRANSLATOR_GROUP (Score_performer,
		      /* doc */
		      "",

		      /* create */
		      "",

		      /* read */
		      "",

		      /* write */
		      ""
		      );

Score_performer::Score_performer ()
{
  performance_ = 0;
  skipping_ = false;
  audio_column_ = 0;
}

Score_performer::~Score_performer ()
{
}

void
Score_performer::announce_element (Audio_element_info info)
{
  announce_infos_.push_back (info);
  if (Audio_staff *s = dynamic_cast<Audio_staff*> (info.elem_))
    {
      performance_->audio_staffs_.push_back (s);
    }

  performance_->add_element (info.elem_);
}

void
Score_performer::acknowledge_audio_elements ()
{
  for (vsize i = 0; i < announce_infos_.size (); i++)
    {
      if (Audio_item *ai = dynamic_cast<Audio_item *> (announce_infos_[i].elem_))
	audio_column_->add_audio_item (ai);
    }
  Performer_group::acknowledge_audio_elements ();
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
  announce_element (Audio_element_info (audio_column_, 0));
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
	  skip_start_mom_ = audio_column_->when ();
	  skipping_ = true;
        }
    }
  else
    {
      if (skipping_)
        {
	  offset_mom_ -= audio_column_->when () - skip_start_mom_;
	  skipping_ = false;
	}

      audio_column_->offset_when (offset_mom_);
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


