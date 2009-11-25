/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2009 Jan Nieuwenhuizen <janneke@gnu.org>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
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
