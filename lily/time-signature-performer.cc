/*
  time_signature-performer.cc -- implement Time_signature_performer

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "time-signature-performer.hh"
#include "command-request.hh"
#include "audio-item.hh"

IMPLEMENT_IS_TYPE_B1(Time_signature_performer,Performer);
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
Time_signature_performer::do_try_request (Request* req_l)
{
  if (time_signature_req_l_)
    return false;

  if (req_l->access_Command_req ())
    time_signature_req_l_ = req_l->access_Command_req ()->access_Time_signature_change_req ();

  if (time_signature_req_l_)
    return true;

  return false;
}

