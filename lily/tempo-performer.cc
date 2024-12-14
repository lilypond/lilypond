/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2023 Jan Nieuwenhuizen <janneke@gnu.org>

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
#include "duration.hh"
#include "stream-event.hh"

#include "translator.icc"

class Tempo_performer final : public Performer
{
public:
  TRANSLATOR_DECLARATIONS (Tempo_performer);
  ~Tempo_performer ();

protected:
  void process_music ();

private:
  Rational wpm_ = -Rational::infinity ();
};

Tempo_performer::Tempo_performer (Context *c)
  : Performer (c)
{
}

Tempo_performer::~Tempo_performer ()
{
}

void
Tempo_performer::process_music ()
{
  auto wpm = from_scm (get_property (this, "tempoWholesPerMinute"), wpm_);
  if (wpm != wpm_)
    {
      Stream_event *cause = nullptr;
      announce<Audio_tempo> (cause, wpm);
      wpm_ = wpm;
    }
}

void
Tempo_performer::boot ()
{
}

ADD_TRANSLATOR (Tempo_performer,
                /* doc */
                R"(

                )",

                /* create */
                R"(

                )",

                /* read */
                R"(
tempoWholesPerMinute
                )",

                /* write */
                R"(

                )");
