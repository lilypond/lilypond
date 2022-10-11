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

#include "performer.hh"

#include "audio-item.hh"
#include "duration.hh"
#include "stream-event.hh"

#include "translator.icc"

class Tempo_performer : public Performer
{
public:
  TRANSLATOR_DECLARATIONS (Tempo_performer);
  ~Tempo_performer ();

protected:
  void derived_mark () const override;
  void process_music ();

private:
  SCM last_tempo_;
};

void
Tempo_performer::derived_mark () const
{
  scm_gc_mark (last_tempo_);
}

Tempo_performer::Tempo_performer (Context *c)
  : Performer (c)
{
  last_tempo_ = SCM_EOL;
}

Tempo_performer::~Tempo_performer ()
{
}

void
Tempo_performer::process_music ()
{
  SCM w = get_property (this, "tempoWholesPerMinute");
  if (unsmob<Moment> (w) && !ly_is_equal (w, last_tempo_))
    {
      Rational r = unsmob<Moment> (w)->main_part_;
      r *= Rational (4, 1);

      Stream_event *cause = nullptr;
      announce<Audio_tempo> (cause, static_cast<int> (r.trunc_int ()));

      last_tempo_ = w;
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
