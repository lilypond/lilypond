/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2011 Han-Wen Nienhuys <hanwen@lilypond.org>

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

#include "stream-event.hh"
#include "item.hh"
#include "spanner.hh"

#include "translator.icc"

class Footnote_engraver : public Engraver
{
  TRANSLATOR_DECLARATIONS (Footnote_engraver);

  DECLARE_TRANSLATOR_LISTENER (footnote);
  DECLARE_ACKNOWLEDGER (grob);
  vector<Stream_event *> events_;

  void stop_translation_timestep ();

  void footnotify (Grob *, Stream_event *);
};

IMPLEMENT_TRANSLATOR_LISTENER (Footnote_engraver, footnote);
void
Footnote_engraver::listen_footnote (Stream_event *ev)
{
  events_.push_back (ev);
}

void
Footnote_engraver::stop_translation_timestep ()
{
  events_.clear ();
}

Footnote_engraver::Footnote_engraver ()
{
}

void
Footnote_engraver::footnotify (Grob *g, Stream_event *event)
{
  Spanner *s = dynamic_cast<Spanner *>(g);
  
  if (s)
    {
      Spanner *b = make_spanner ("FootnoteSpanner", event->self_scm ());
      b->set_property ("footnote-text", event->get_property ("footnote-text"));
      b->set_property ("text", event->get_property ("text"));
      b->set_parent (s, Y_AXIS);
      b->set_parent (s, X_AXIS);
      bool bar = scm_is_string (get_property ("whichBar"));
      Grob *bound = unsmob_grob (bar
                                 ? get_property ("currentCommandColumn")
                                 : get_property ("currentMusicalColumn"));
      b->set_bound (LEFT, bound);
      b->set_bound (RIGHT, bound);
      b->set_property ("parent-spanner", s->self_scm ());
    }
  else
    {
      Grob *b = make_item ("Footnote", event->self_scm ());
      b->set_property ("footnote-text", event->get_property ("footnote-text"));
      b->set_property ("text", event->get_property ("text"));
      b->set_parent (g, Y_AXIS);
      b->set_parent (g, X_AXIS);
    }
}

void
Footnote_engraver::acknowledge_grob (Grob_info info)
{
  Stream_event *cause = info.event_cause ();

  SCM arts = cause ? cause->get_property ("articulations") : SCM_EOL;
  for (SCM s = arts; scm_is_pair (s); s = scm_cdr (s))
    {
      Stream_event *e = unsmob_stream_event (scm_car (s));
      if (e->in_event_class ("footnote-event"))
	{
	  footnotify (info.grob (), e);
	}
    }

  for (vsize i = 0; i < events_.size (); i++)
    {
      if (info.grob ()->name () == ly_symbol2string (events_[i]->get_property ("symbol")))
	footnotify (info.grob (), events_[i]);
    }
}



ADD_ACKNOWLEDGER (Footnote_engraver, grob);

ADD_TRANSLATOR (Footnote_engraver,
	       /* doc */
	       "Create footnote texts.",

	       /* create */
	       "Footnote ",

	       /*read*/
	       "",

	       /*write*/
	       ""
	       );
