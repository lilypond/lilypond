/*
  lyric-performer.cc -- implement Lyric_performer

  source file of the GNU LilyPond music typesetter

  (c) 1997--2008 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "audio-item.hh"
#include "performer.hh"
#include "stream-event.hh"
#include "translator.icc"

class Lyric_performer : public Performer
{
public:
  TRANSLATOR_DECLARATIONS (Lyric_performer);
protected:

  void stop_translation_timestep ();
  void process_music ();
  DECLARE_TRANSLATOR_LISTENER (lyric);
private:
  vector<Stream_event *> events_;
  Audio_text *audio_;
};

Lyric_performer::Lyric_performer ()
{
  audio_ = 0;
}

void
Lyric_performer::process_music ()
{
  // FIXME: won't work with fancy lyrics
  if (events_.size ()
      && scm_is_string (events_[0]->get_property ("text"))
      && ly_scm2string (events_[0]->get_property ("text")).length ())
    {
      audio_ = new Audio_text (Audio_text::LYRIC,
			       ly_scm2string (events_[0]->get_property ("text")));
      Audio_element_info info (audio_, events_[0]);
      announce_element (info);
    }
  events_.clear ();
}

void
Lyric_performer::stop_translation_timestep ()
{
  if (audio_)
    {
      audio_ = 0;
    }
  events_.clear ();
}

IMPLEMENT_TRANSLATOR_LISTENER (Lyric_performer, lyric);
void
Lyric_performer::listen_lyric (Stream_event *event)
{
  events_.push_back (event);
}

ADD_TRANSLATOR (Lyric_performer,
		/* doc */
		"",

		/* create */
		"",

		/* read */
		"",

		/* write */
		""
		);
