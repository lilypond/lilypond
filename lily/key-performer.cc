/*
  key-performer.cc -- implement Key_performer

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "key-performer.hh"
#include "command-request.hh"
#include "audio-item.hh"




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
Key_performer::do_try_music (Music* req_l)
{
  if (Key_change_req *kc = dynamic_cast <Key_change_req *> (req_l))
    {
      if (key_req_l_)
	warning ("FIXME. Key change merge");

      key_req_l_ = kc;
      return true;
    }

  return false;
}

