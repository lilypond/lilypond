/*
  score-performer.cc -- implement Score_performer

  source file of the GNU LilyPond music typesetter

  (c) 1996--2001 Jan Nieuwenhuizen <janneke@gnu.org>
 */

#include "score-performer.hh"
#include "midi-def.hh"
#include "audio-column.hh"
#include "audio-item.hh"
#include "performance.hh"
#include "midi-stream.hh"
#include "string-convert.hh"
#include "debug.hh"
#include "translator-def.hh"

ADD_THIS_TRANSLATOR (Score_performer);


Score_performer::Score_performer ()
{
  performance_p_ = 0;
}


Score_performer::~Score_performer ()
{
}

void
Score_performer::play_element (Audio_element * p)
{
  if  (Audio_item * i=dynamic_cast<Audio_item *> (p)) 
    {
      audio_column_l_->add_audio_item (i);
    }
  performance_p_->add_element (p);
}

void
Score_performer::announce_element (Audio_element_info info)
{
  announce_info_arr_.push (info);


  /*
    huh?
    copied from score-engraver, but
    this way staff gets in list twice
  if (Audio_item* i = dynamic_cast<Audio_item*> (info.elem_l_))
    performance_p_->add_element (i);
  */
}

void 
Score_performer::prepare (Moment m)
{
  Global_translator::prepare (m);
  audio_column_l_ = new Audio_column (m);
  play_element (audio_column_l_);
  post_move_processing ();
}


void 
Score_performer::one_time_step ()
{
  // fixme: put this back.
  // process_music();
  announces ();
  pre_move_processing();
  check_removal();
}

void
Score_performer::start()
{
}


int
Score_performer::get_tempo_i() const
{
  return performance_p_->midi_l_->get_tempo_i (Moment (1, 4));
}

void
Score_performer::finish()
{
  check_removal ();
  removal_processing();
}

Music_output *
Score_performer::get_output_p ()
{
  Music_output * o = performance_p_;
  performance_p_ =0;
  return o;
}

void
Score_performer::initialize ()
{
  unsmob_translator_def (definition_)->apply_property_operations (this);
  assert (dynamic_cast<Midi_def *> (output_def_l_));
  performance_p_ = new Performance;
  performance_p_->midi_l_ = dynamic_cast<Midi_def*>(output_def_l_);

  Translator_group::initialize ();
}
