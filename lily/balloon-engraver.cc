/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2021 Han-Wen Nienhuys <hanwen@lilypond.org>

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

  void balloonify (Grob *, Stream_event *);
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
Balloon_engraver::balloonify (Grob *g, Stream_event *event)
{
  Grob *balloon = nullptr;
  if (dynamic_cast<Item *> (g))
    {
      balloon = make_item ("BalloonTextItem", event->self_scm ());
      balloon->set_x_parent (g);
    }
  else if (dynamic_cast<Spanner *> (g))
    {
      balloon = make_spanner ("BalloonTextSpanner", event->self_scm ());
      // Delegate ending the balloon to the Spanner_tracking_engraver.
      set_object (balloon, "sticky-host", to_scm (g));
    }
  balloon->set_y_parent (g);
}

void
Balloon_engraver::acknowledge_grob (Grob_info info)
{
  Stream_event *cause = info.event_cause ();

  SCM arts = cause ? get_property (cause, "articulations") : SCM_EOL;
  for (SCM s = arts; scm_is_pair (s); s = scm_cdr (s))
    {
      Stream_event *e = unsmob<Stream_event> (scm_car (s));
      if (e->in_event_class ("annotate-output-event"))
        {
          balloonify (info.grob (), e);
        }
    }

  for (Stream_event *ev : events_)
    {
      if (info.grob ()->name ()
          == ly_symbol2string (get_property (ev, "symbol")))
        balloonify (info.grob (), ev);
    }
}

void
Balloon_engraver::boot ()
{
  ADD_LISTENER (Balloon_engraver, annotate_output);
  ADD_ACKNOWLEDGER (Balloon_engraver, grob);
}

ADD_TRANSLATOR (Balloon_engraver,
                /* doc */
                "Create balloon texts.",

                /* create */
                "BalloonTextItem "
                "BalloonTextSpanner ",

                /*read*/
                "",

                /*write*/
                ""
               );
