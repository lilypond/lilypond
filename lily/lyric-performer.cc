/*
  lyric-performer.cc -- implement Lyric_performer

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "musical-request.hh"
#include "audio-item.hh"
#include "lily-proto.hh"
#include "performer.hh"
#include "array.hh"

class Lyric_performer : public Performer {
public:
  VIRTUAL_COPY_CONS(Translator);
 Lyric_performer ();

protected:

  virtual bool try_music (Music* req_l);
  virtual void stop_translation_timestep ();
  virtual void create_audio_elements ();

private:
  Link_array<Lyric_req> lreq_arr_;
  Audio_text* audio_p_;
};

ADD_THIS_TRANSLATOR (Lyric_performer);

Lyric_performer::Lyric_performer ()
{
  audio_p_ = 0;
}


void
Lyric_performer::create_audio_elements ()
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
Lyric_performer::stop_translation_timestep ()
{
  if (audio_p_)
    {
      play_element (audio_p_);
      audio_p_ = 0;
    }
  lreq_arr_.clear();
}

bool
Lyric_performer::try_music (Music* req_l)
{
  if (Lyric_req *lr = dynamic_cast <Lyric_req *> (req_l))
    {
      lreq_arr_.push (lr);
      return true;
    }
  return false;
}

