/*
  score-performer.cc -- implement Score_performer

  source file of the GNU LilyPond music typesetter

  (c) 1996--2004 Jan Nieuwenhuizen <janneke@gnu.org>
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

ENTER_DESCRIPTION (Score_performer,
/* descr */       "",
/* creats*/       "",
/* accepts */     "",
/* acks  */      "",
/* reads */       "",
/* write */       "");


Score_performer::Score_performer ()
{
  performance_ = 0;
}


Score_performer::~Score_performer ()
{
}

void
Score_performer::play_element (Audio_element * p)
{
  if (Audio_item * i=dynamic_cast<Audio_item *> (p)) 
    {
      audio_column_->add_audio_item (i);
    }
  performance_->add_element (p);
}

void
Score_performer::announce_element (Audio_element_info info)
{
  announce_infos_.push (info);
}

void 
Score_performer::prepare (Moment m)
{
  audio_column_ = new Audio_column (m);
  play_element (audio_column_);
  recurse_over_translators (context (), &Translator::start_translation_timestep, UP);
}

void
Score_performer::finish ()
{
  recurse_over_translators (context (), &Translator::finalize, UP);
}
  
void 
Score_performer::one_time_step ()
{
  recurse_over_translators (context (), &Performer::process_music, UP);
  recurse_over_translators (context (), &Performer::do_announces, UP);
  recurse_over_translators (context (), &Translator::stop_translation_timestep, UP);
}

int
Score_performer::get_tempo () const
{
  return ::get_tempo (performance_->midi_, Moment (Rational (1, 4)));
}


Music_output *
Score_performer::get_output ()
{
  Music_output * o = performance_;
  performance_ = 0;
  return o;
}

void
Score_performer::initialize ()
{
  performance_ = new Performance;
  performance_->midi_ = get_output_def ();

  Translator_group::initialize ();
}
