/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2022 Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "performer.hh"
#include "audio-item.hh"
#include "audio-column.hh"
#include "global-context.hh"
#include "span-event-listener.hh"
#include "stream-event.hh"
#include "warn.hh"

#include "translator.icc"

class Beam_performer : public Performer
{
public:
  TRANSLATOR_DECLARATIONS (Beam_performer);

protected:
  void start_translation_timestep ();
  void process_music ();
  void set_melisma (bool);

private:
  Last_span_event_listener beam_listener_;
};

Beam_performer::Beam_performer (Context *c)
  : Performer (c)
{
}

void
Beam_performer::process_music ()
{
  if (beam_listener_.get_start ())
    set_melisma (true);
  else if (beam_listener_.get_stop ())
    set_melisma (false);
}

void
Beam_performer::set_melisma (bool ml)
{
  if (!from_scm<bool> (get_property (this, "autoBeaming")))
    set_property (context (), "beamMelismaBusy", to_scm (ml));
}

void
Beam_performer::start_translation_timestep ()
{
  beam_listener_.reset ();
}

void
Beam_performer::boot ()
{
  ADD_DELEGATE_LISTENER (beam);
}

ADD_TRANSLATOR (Beam_performer,
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
