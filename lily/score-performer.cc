/*
  score-performer.cc -- implement Score_performer

  source file of the GNU LilyPond music typesetter

  (c) 1996--2002 Jan Nieuwenhuizen <janneke@gnu.org>
 */

#include "score-performer.hh"
#include "midi-def.hh"
#include "audio-column.hh"
#include "audio-item.hh"
#include "performance.hh"
#include "midi-stream.hh"
#include "string-convert.hh"
#include "warn.hh"
#include "translator-def.hh"



ENTER_DESCRIPTION(Score_performer,
/* descr */       "",
/* creats*/       "",
/* accepts */     "general-music",
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


  /*
    huh?
    copied from score-engraver, but
    this way staff gets in list twice
  if (Audio_item* i = dynamic_cast<Audio_item*> (info.elem_))
    performance_->add_element (i);
  */
}

void 
Score_performer::prepare (Moment m)
{
  Global_translator::prepare (m);
  audio_column_ = new Audio_column (m);
  play_element (audio_column_);
  start_translation_timestep ();
}


void 
Score_performer::one_time_step ()
{
  // fixme: put this back.
  // process_music ();
  do_announces ();
  stop_translation_timestep ();
  check_removal ();
}

void
Score_performer::start ()
{
}


int
Score_performer::get_tempo () const
{
  return performance_->midi_->get_tempo (Moment (Rational (1, 4)));
}

void
Score_performer::finish ()
{
  check_removal ();
  removal_processing ();
}

Music_output *
Score_performer::get_output ()
{
  Music_output * o = performance_;
  performance_ =0;
  return o;
}

void
Score_performer::initialize ()
{
  unsmob_translator_def (definition_)->apply_property_operations (this);
  assert (dynamic_cast<Midi_def *> (output_def_));
  performance_ = new Performance;
  performance_->midi_ = dynamic_cast<Midi_def*> (output_def_);

  Translator_group::initialize ();
}
