/*
  key-performer.cc -- implement Key_performer

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "key-performer.hh"
#include "command-request.hh"
#include "audio-item.hh"



IMPLEMENT_IS_TYPE_B1(Key_performer,Performer);
ADD_THIS_TRANSLATOR(Key_performer);

Key_performer::Key_performer()
{
  key_req_l_ = 0;
}

Key_performer::~Key_performer()
{
}

void 
Key_performer::do_print() const
{
#ifndef NPRINT
  if (key_req_l_)
  	key_req_l_->print();
#endif
}

void
Key_performer::do_process_requests()
{
  if (key_req_l_)
	play (new Audio_key (key_req_l_));
  key_req_l_ = 0;
}

bool
Key_performer::do_try_request (Request* req_l)
{
  if (key_req_l_)
	return false;

  if (req_l->access_Command_req ())
	key_req_l_ = req_l->access_Command_req ()->access_Key_change_req ();

  if (key_req_l_)
	return true;

  return false;
}

