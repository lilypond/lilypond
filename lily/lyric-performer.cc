/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2021 Jan Nieuwenhuizen <janneke@gnu.org>

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

using std::vector;

class Lyric_performer : public Performer
{
public:
  TRANSLATOR_DECLARATIONS (Lyric_performer);
protected:

  void stop_translation_timestep ();
  void process_music ();
  void listen_lyric (Stream_event *);
private:
  vector<Stream_event *> events_;
};

Lyric_performer::Lyric_performer (Context *c)
  : Performer (c)
{
}

void
Lyric_performer::process_music ()
{
  // FIXME: won't work with fancy lyrics
  if (events_.size ()
      && scm_is_string (get_property (events_[0], "text"))
      && ly_scm2string (get_property (events_[0], "text")).length ())
    {
      announce<Audio_text> (events_[0], Audio_text::LYRIC,
                            ly_scm2string (get_property (events_[0], "text")));
    }
  events_.clear ();
}

void
Lyric_performer::stop_translation_timestep ()
{
  events_.clear ();
}

void
Lyric_performer::listen_lyric (Stream_event *event)
{
  events_.push_back (event);
}

void
Lyric_performer::boot ()
{
  ADD_LISTENER (Lyric_performer, lyric);
}

ADD_TRANSLATOR (Lyric_performer,
                /* doc */
                R"(

                )",

                /* create */
                R"(

                )",

                /* read */
                R"(

                )",

                /* write */
                R"(

                )");
