/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2020 Han-Wen Nienhuys <hanwen@lilypond.org>

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
  void acknowledge_end_grob (Grob_info info);
  void acknowledge_grob (Grob_info) override;
  vector<Stream_event *> events_;
  vector<Spanner *> spanners_;
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
  if (dynamic_cast<Item *> (g))
    {
      Grob *b = make_item ("BalloonTextItem", event->self_scm ());
      set_property (b, "text", get_property (event, "text"));
      b->set_parent (g, Y_AXIS);
      b->set_parent (g, X_AXIS);
    }
  else if (dynamic_cast<Spanner *> (g))
    {
      Spanner *sp = make_spanner ("BalloonTextSpanner", event->self_scm ());
      set_property (sp, "text", get_property (event, "text"));
      sp->set_parent (g, Y_AXIS);
      spanners_.push_back (sp);
    }
}

void
Balloon_engraver::acknowledge_end_grob (Grob_info info)
{
  vector<Spanner *> next;
  for (Spanner *sp : spanners_)
    if (sp->get_parent (Y_AXIS) == info.grob ())
      {
        for (LEFT_and_RIGHT (d))
          sp->set_bound (d,
                         dynamic_cast<Spanner *> (info.grob ())->get_bound (d));
      }
    else
      {
        next.push_back (sp);
      }
  spanners_.swap (next);
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
  ADD_END_ACKNOWLEDGER (Balloon_engraver, grob);
}

ADD_TRANSLATOR (Balloon_engraver,
                /* doc */
                "Create balloon texts.",

                /* create */
                "BalloonTextItem ",

                /*read*/
                "",

                /*write*/
                ""
               );
