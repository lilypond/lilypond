/*
  lyric-performer.cc -- implement Lyric_performer

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "lyric-performer.hh"
#include "musical-request.hh"
#include "audio-item.hh"

ADD_THIS_TRANSLATOR (Lyric_performer);

Lyric_performer::Lyric_performer ()
{
  audio_p_ = 0;
}


void
Lyric_performer::do_process_music ()
{
  // FIXME: won't work with fancy lyrics
  if (lreq_arr_.size ()
      && gh_string_p (lreq_arr_[0]->get_mus_property ("text"))
      && ly_scm2string (lreq_arr_[0]->get_mus_property ("text")).length_i ())
    {
      audio_p_ = new Audio_text (Audio_text::LYRIC,
				 ly_scm2string (lreq_arr_[0]->get_mus_property ("text")));
      Audio_element_info info (audio_p_, lreq_arr_[0]);
      announce_element (info);
    }
  lreq_arr_.clear();
}

void
Lyric_performer::do_pre_move_processing ()
{
  if (audio_p_)
    {
      play_element (audio_p_);
      audio_p_ = 0;
    }
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

