/*
  lyric-performer.cc -- implement Lyric_performer

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "lyric-performer.hh"
#include "musical-request.hh"
#include "audio-item.hh"




ADD_THIS_TRANSLATOR(Lyric_performer);

void 
Lyric_performer::do_print() const
{
#ifndef NPRINT
  if (lreq_arr_.size())
    lreq_arr_[0]->print();
#endif
}

void
Lyric_performer::do_process_requests()
{
  if (lreq_arr_.size() && lreq_arr_[0]->text_str_.length_i())
    play (new Audio_text (Audio_text::LYRIC, lreq_arr_[0]->text_str_));
  lreq_arr_.clear();
}

bool
Lyric_performer::do_try_music (Music* req_l)
{
  if (Lyric_req *lr = dynamic_cast <Lyric_req *> (req_l))
    {
      lreq_arr_.push (lr);
      return true;
    }
  return false;
}

