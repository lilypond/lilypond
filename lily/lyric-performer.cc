/*
  lyric-performer.cc -- implement Lyric_performer

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "request.hh"
#include "audio-item.hh"
#include "lily-proto.hh"
#include "performer.hh"
#include "array.hh"

class Lyric_performer : public Performer {
public:
  TRANSLATOR_DECLARATIONS(Lyric_performer);
protected:

  virtual bool try_music (Music* req);
  virtual void stop_translation_timestep ();
  virtual void create_audio_elements ();

private:
  Link_array<Music> lreqs_;
  Audio_text* audio_;
};




Lyric_performer::Lyric_performer ()
{
  audio_ = 0;
}


void
Lyric_performer::create_audio_elements ()
{
  // FIXME: won't work with fancy lyrics
  if (lreqs_.size ()
      && gh_string_p (lreqs_[0]->get_mus_property ("text"))
      && ly_scm2string (lreqs_[0]->get_mus_property ("text")).length ())
    {
      audio_ = new Audio_text (Audio_text::LYRIC,
				 ly_scm2string (lreqs_[0]->get_mus_property ("text")));
      Audio_element_info info (audio_, lreqs_[0]);
      announce_element (info);
    }
  lreqs_.clear ();
}

void
Lyric_performer::stop_translation_timestep ()
{
  if (audio_)
    {
      play_element (audio_);
      audio_ = 0;
    }
  lreqs_.clear ();
}

bool
Lyric_performer::try_music (Music* req)
{
  if (req->is_mus_type ("lyric-event"))
    {
      lreqs_.push (req);
      return true;
    }
  return false;
}

ENTER_DESCRIPTION(Lyric_performer,"","","lyric-event","","","");
