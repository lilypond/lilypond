/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2009 Han-Wen Nienhuys <hanwen@lilypond.org>

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

#include "translator.icc"

class Balloon_engraver : public Engraver
{
  TRANSLATOR_DECLARATIONS (Balloon_engraver);

  DECLARE_TRANSLATOR_LISTENER (annotate_output);
  DECLARE_ACKNOWLEDGER (grob);
  vector<Stream_event *> events_;

  void stop_translation_timestep ();
  
  void balloonify (Grob *, Stream_event *); 
};

IMPLEMENT_TRANSLATOR_LISTENER (Balloon_engraver, annotate_output);
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

Balloon_engraver::Balloon_engraver ()
{
}

void
Balloon_engraver::balloonify (Grob *g, Stream_event *event)
{
  Grob * b = make_item ("BalloonTextItem", event->self_scm ());
  b->set_property ("text", event->get_property ("text"));
  b->set_parent (g, Y_AXIS);
  b->set_parent (g, X_AXIS);
}

void
Balloon_engraver::acknowledge_grob (Grob_info info)
{
  Stream_event *cause = info.event_cause ();
  
  SCM arts = cause ? cause->get_property ("articulations") : SCM_EOL;
  for (SCM s = arts; scm_is_pair (s); s = scm_cdr (s))
    {
      Stream_event *e = unsmob_stream_event (scm_car (s));
      if (e->in_event_class ("annotate-output-event"))
	{
	  balloonify (info.grob (), e);
	}
    }

  for (vsize i = 0; i < events_.size (); i++)
    {
      if (info.grob ()->name () == ly_symbol2string (events_[i]->get_property ("symbol")))
	balloonify (info.grob (), events_[i]);
    }
}


  
ADD_ACKNOWLEDGER (Balloon_engraver, grob);
  
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
