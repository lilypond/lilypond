/*
  note-performer.cc -- implement Note_performer

  source file of the GNU LilyPond music typesetter

  (c) 1996,  1997--1998 Jan Nieuwenhuizen <jan@digicash.com>
 */

#include "note-performer.hh"
#include "musical-request.hh"
#include "audio-item.hh"
#include "debug.hh"

IMPLEMENT_IS_TYPE_B1(Note_performer,Performer);
ADD_THIS_TRANSLATOR(Note_performer);

Note_performer::Note_performer()
{
  note_req_l_ = 0;
}

void 
Note_performer::do_print() const
{
#ifndef NPRINT
  if (note_req_l_) 
    {
      note_req_l_->print();
    }
#endif
}

void 
Note_performer::do_process_requests() 
{
  // this is _really_ braindead, but it generates some output
  if (!note_req_l_ || !note_req_l_->melodic()  || !note_req_l_->rhythmic ())
    return;

  play (new Audio_note (note_req_l_));
  note_req_l_ = 0;
}

bool 
Note_performer::do_try_request (Request* req_l)
{
  if (note_req_l_)
    return false;
  
  if (!req_l->musical() || !req_l->musical ()->note ())
    return false;

  note_req_l_ = req_l->musical()->melodic ();
  return true;
}
