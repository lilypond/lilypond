/*
  tempo-performer.cc -- implement Tempo_performer

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "tempo-performer.hh"
#include "command-request.hh"
#include "audio-item.hh"

ADD_THIS_TRANSLATOR (Tempo_performer);

Tempo_performer::Tempo_performer ()
{
  tempo_req_l_ = 0;
  audio_p_ = 0;
}

Tempo_performer::~Tempo_performer ()
{
}


void
Tempo_performer::do_process_music ()
{
  if (tempo_req_l_)
    {
      audio_p_ = new Audio_tempo (tempo_req_l_->dur_.length_mom () /
				  Moment (1, 4) 
				  * Moment(tempo_req_l_->metronome_i_));
      Audio_element_info info (audio_p_, tempo_req_l_);
      announce_element (info);
      tempo_req_l_ = 0;
    }
}

void
Tempo_performer::do_pre_move_processing ()
{
  if (audio_p_)
    {
      play_element (audio_p_);
      audio_p_ = 0;
    }
}

bool
Tempo_performer::do_try_music (Music* req_l)
{
  if (tempo_req_l_)
    return false;

  if (Tempo_req *t =
      dynamic_cast <Tempo_req *> (req_l))
    {
      tempo_req_l_ = t;
      return true;
    }

  return false;
}

