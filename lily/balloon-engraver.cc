/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2022 Han-Wen Nienhuys <hanwen@lilypond.org>

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

#include "engraver.hh"

#include "item.hh"
#include "spanner.hh"
#include "stream-event.hh"

#include "translator.icc"

using std::vector;

class Balloon_engraver : public Engraver
{
  TRANSLATOR_DECLARATIONS (Balloon_engraver);

  void listen_annotate_output (Stream_event *);
  void acknowledge_grob (Grob_info) override;
  vector<Stream_event *> events_;
  void stop_translation_timestep ();
};

void
Balloon_engraver::listen_annotate_output (Stream_event *ev)
{
  events_.push_back (ev);
}

void
Balloon_engraver::stop_translation_timestep ()
{
  events_.clear ();
}

Balloon_engraver::Balloon_engraver (Context *c)
  : Engraver (c)
{
}

void
Balloon_engraver::acknowledge_grob (Grob_info info)
{
  Stream_event *cause = info.event_cause ();
  Engraver *eng = info.origin_engraver ();

  SCM arts = cause ? get_property (cause, "articulations") : SCM_EOL;
  for (SCM s = arts; scm_is_pair (s); s = scm_cdr (s))
    {
      SCM e_scm = scm_car (s);
      Stream_event *e = unsmob<Stream_event> (e_scm);
      if (e->in_event_class ("annotate-output-event"))
        {
          eng->make_sticky ("BalloonText", info.grob (), e_scm);
        }
    }

  for (Stream_event *ev : events_)
    {
      if (info.grob ()->name ()
          == ly_symbol2string (get_property (ev, "symbol")))
        eng->make_sticky ("BalloonText", info.grob (), ev->self_scm ());
    }
}

void
Balloon_engraver::boot ()
{
  ADD_LISTENER (annotate_output);
  ADD_ACKNOWLEDGER (grob);
}

ADD_TRANSLATOR (Balloon_engraver,
                /* doc */
                R"(
Create balloon texts.
                )",

                /* create */
                R"(
BalloonText
                )",

                /*read*/
                R"(

                )",

                /*write*/
                R"(

                )");
