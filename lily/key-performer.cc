/*
  key-performer.cc -- implement Key_performer

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "key-performer.hh"
#include "command-request.hh"
#include "audio-item.hh"


ADD_THIS_TRANSLATOR (Key_performer);

Key_performer::Key_performer ()
{
  key_req_l_ = 0;
  audio_p_ = 0;
}

Key_performer::~Key_performer ()
{
}

void
Key_performer::do_process_music ()
{
  if (key_req_l_ && key_req_l_->key_)
    {
      audio_p_ = new Audio_key (); // *key_req_l_->key_);
      Audio_element_info info (audio_p_, key_req_l_);
      announce_element (info);
      key_req_l_ = 0;
    }
}

void
Key_performer::do_pre_move_processing ()
{
  if (audio_p_)
    {
      play_element (audio_p_);
      audio_p_ = 0;
    }
}

bool
Key_performer::do_try_music (Music* req_l)
{
  if (Key_change_req *kc = dynamic_cast <Key_change_req *> (req_l))
    {
      if (key_req_l_)
	warning (_ ("FIXME: key change merge"));

      key_req_l_ = kc;
      return true;
    }

  return false;
}

