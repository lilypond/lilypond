/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Jan Nieuwenhuizen <janneke@gnu.org>

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
#include "lily-imports.hh"
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
  void listen_lyric (Stream_event *);

private:
  Stream_event *event_ = nullptr;
};

Lyric_performer::Lyric_performer (Context *c)
  : Performer (c)
{
}

void
Lyric_performer::process_music ()
{
  if (event_)
    {
      SCM text = get_property (event_, "text");
      // Mimic lyric-text::print by wrapping text in \tied-lyric if a string.
      // This ensures that the custom markup->string handler of \tied-lyric will
      // convert tildes to Unicode underties.
      if (scm_is_string (text))
        text = Lily::make_tied_lyric_markup (text);
      if (!scm_is_null (text))
        announce<Audio_text> (event_, Audio_text::LYRIC, text);

      event_ = nullptr;
    }
}

void
Lyric_performer::stop_translation_timestep ()
{
  event_ = nullptr;
}

void
Lyric_performer::listen_lyric (Stream_event *event)
{
  if (!event_)
    event_ = event;
}

void
Lyric_performer::boot ()
{
  ADD_LISTENER (lyric);
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
