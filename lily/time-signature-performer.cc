/*
  time-signature-performer.cc -- implement Time_signature_performer

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "time-signature-performer.hh"
#include "command-request.hh"
#include "audio-item.hh"

ADD_THIS_TRANSLATOR (Time_signature_performer);

Time_signature_performer::Time_signature_performer ()
{
  time_signature_req_l_ = 0;
  audio_p_ = 0;
}

Time_signature_performer::~Time_signature_performer ()
{
}

void 
Time_signature_performer::do_print () const
{
#ifndef NPRINT
  if (time_signature_req_l_)
    time_signature_req_l_->print ();
#endif
}

void
Time_signature_performer::do_process_music ()
{
  if (time_signature_req_l_)
    {
      audio_p_ = new Audio_time_signature (time_signature_req_l_->beats_i_, time_signature_req_l_->one_beat_i_);
      Audio_element_info info (audio_p_, time_signature_req_l_);
      announce_element (info);
      time_signature_req_l_ = 0;
    }
}

void
Time_signature_performer::do_pre_move_processing ()
{
  if (audio_p_)
    {
      play_element (audio_p_);
      audio_p_ = 0;
    }
}

bool
Time_signature_performer::do_try_music (Music* req_l)
{
  if (time_signature_req_l_)
    return false;

  if (Time_signature_change_req *t =
      dynamic_cast <Time_signature_change_req *> (req_l))
    {
      time_signature_req_l_ = t;
      return true;
    }

  return false;
}

