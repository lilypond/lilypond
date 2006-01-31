/*
  score-performer.cc -- implement Score_performer

  source file of the GNU LilyPond music typesetter

  (c) 1996--2006 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "score-performer.hh"

#include "audio-column.hh"
#include "audio-item.hh"
#include "performance.hh"
#include "midi-stream.hh"
#include "string-convert.hh"
#include "warn.hh"
#include "context-def.hh"
#include "output-def.hh"
#include "context.hh"

ADD_TRANSLATOR_GROUP (Score_performer,
		      /* doc */ "",
		      /* create */ "",
		      /* accept */ "",
		      /* read */ "",
		      /* write */ "");

Score_performer::Score_performer ()
{
  performance_ = 0;
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
Score_performer::prepare (Moment m)
{
  audio_column_ = new Audio_column (m);
  play_element (audio_column_);
  precomputed_recurse_over_translators (context (), START_TRANSLATION_TIMESTEP, UP);
}

void
Score_performer::finish ()
{
  recurse_over_translators (context (),
			    &Translator::finalize,
			    &Translator_group::finalize,
			    UP);
}

void
Score_performer::one_time_step ()
{
  precomputed_recurse_over_translators (context (), PROCESS_MUSIC, UP);
  do_announces ();
  precomputed_recurse_over_translators (context (), STOP_TRANSLATION_TIMESTEP, UP);
}

int
Score_performer::get_tempo () const
{
  return ::get_tempo (performance_->midi_, Moment (Rational (1, 4)));
}

SCM
Score_performer::get_output ()
{
  Music_output *o = performance_;
  performance_ = 0;
  return o->self_scm ();
}

void
Score_performer::derived_mark () const
{
  if (performance_)
    scm_gc_mark (performance_->self_scm ());

  Score_translator::derived_mark ();
  Performer_group::derived_mark ();
}

void
Score_performer::initialize ()
{
  performance_ = new Performance;
  performance_->unprotect ();
  performance_->midi_ = context ()->get_output_def ();

  Translator_group::initialize ();
}
