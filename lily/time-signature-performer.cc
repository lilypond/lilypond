/*
  time_signature-performer.cc -- implement Time_signature_performer

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "time-signature-performer.hh"
#include "command-request.hh"
#include "audio-item.hh"


ADD_THIS_TRANSLATOR(Time_signature_performer);

Time_signature_performer::Time_signature_performer()
{
  time_signature_req_l_ = 0;
}

Time_signature_performer::~Time_signature_performer()
{
}

void 
Time_signature_performer::do_print() const
{
#ifndef NPRINT
  if (time_signature_req_l_)
    time_signature_req_l_->print();
#endif
}

void
Time_signature_performer::do_process_requests()
{
  if (time_signature_req_l_)
    play (new Audio_time_signature (time_signature_req_l_));
  time_signature_req_l_ = 0;
}

bool
Time_signature_performer::do_try_music (Music* req_l)
{
  if (time_signature_req_l_)
    return false;

  if (dynamic_cast <Command_req *> (req_l))
    time_signature_req_l_ = dynamic_cast <Time_signature_change_req *> (req_l);

  if (time_signature_req_l_)
    return true;

  return false;
}

