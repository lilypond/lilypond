/*
  score-performer.cc -- implement Score_performer

  source file of the GNU LilyPond music typesetter

  (c) 1996,  1997--1999 Jan Nieuwenhuizen <janneke@gnu.org>
 */

#include "score-performer.hh"
#include "midi-def.hh"
#include "audio-column.hh"
#include "audio-item.hh"
#include "performance.hh"
#include "midi-stream.hh"
#include "string-convert.hh"
#include "debug.hh"
#include "score.hh"
#include "source-file.hh"
#include "source.hh"
#include "audio-staff.hh"


ADD_THIS_TRANSLATOR(Score_performer);


Score_performer::Score_performer()
{
}


Score_performer::~Score_performer()
{
}

void
Score_performer::play (Audio_element * p)
{
  if  (Audio_item * i=dynamic_cast<Audio_item *> (p)) 
    {
      audio_column_l_->add_audio_item (i);
    }
  else if (Audio_staff*s=dynamic_cast<Audio_staff *> (p)) 
    {
      performance_p_->add_staff (s);
    }
  performance_p_->add_element (p);
}

void 
Score_performer::prepare (Moment m)
{
  Global_translator::prepare (m);
  audio_column_l_ = new Audio_column (m);
  performance_p_->add_column (audio_column_l_);
  post_move_processing ();
}


void 
Score_performer::process()
{
  process_requests();
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
Score_performer::do_add_processing ()
{
  Translator_group::do_add_processing ();
  assert (dynamic_cast<Midi_def *> (output_def_l_));
  performance_p_ = new Performance;
  performance_p_->midi_l_ = dynamic_cast<Midi_def*>(output_def_l_); 
}
