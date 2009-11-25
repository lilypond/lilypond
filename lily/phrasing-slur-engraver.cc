/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "context.hh"
#include "directional-element-interface.hh"
#include "international.hh"
#include "note-column.hh"
#include "slur.hh"
#include "spanner.hh"
#include "stream-event.hh"
#include "warn.hh"

#include "translator.icc"

/*
  It is possible that a slur starts and ends on the same note.  At
  least, it is for phrasing slurs: a note can be both beginning and
  ending of a phrase.

*/

/*
  NOTE NOTE NOTE

  This is largely similar to Slur_engraver. Check if fixes apply there too.  

  (on principle, engravers don't use inheritance for code sharing)
  
 */
class Phrasing_slur_engraver : public Engraver
{
  Drul_array<Stream_event *> events_;
  Stream_event *running_slur_start_;
  vector<Grob*> slurs_;
  vector<Grob*> end_slurs_;

protected:
  void acknowledge_extra_object (Grob_info);
  DECLARE_ACKNOWLEDGER (accidental);
  DECLARE_ACKNOWLEDGER (fingering);
  DECLARE_ACKNOWLEDGER (note_column);
  DECLARE_ACKNOWLEDGER (slur);
  DECLARE_ACKNOWLEDGER (script);
  DECLARE_ACKNOWLEDGER (text_script);
  DECLARE_ACKNOWLEDGER (tie);
  DECLARE_ACKNOWLEDGER (tuplet_number);
  DECLARE_TRANSLATOR_LISTENER (phrasing_slur);

  void stop_translation_timestep ();
  virtual void finalize ();
  void process_music ();

public:
  TRANSLATOR_DECLARATIONS (Phrasing_slur_engraver);
};

Phrasing_slur_engraver::Phrasing_slur_engraver ()
{
  events_[START] = events_[STOP] = 0;
}

IMPLEMENT_TRANSLATOR_LISTENER (Phrasing_slur_engraver, phrasing_slur);
void
Phrasing_slur_engraver::listen_phrasing_slur (Stream_event *ev)
{
  /*
    Let's not start more than one slur per moment.
  */
  Direction d = to_dir (ev->get_property ("span-direction"));
  if (d == START)
    ASSIGN_EVENT_ONCE (events_[START], ev);
  else if (d == STOP && !slurs_.empty ())
    ASSIGN_EVENT_ONCE (events_[STOP], ev);
}

void
Phrasing_slur_engraver::acknowledge_note_column (Grob_info info)
{
  Grob *e = info.grob ();
  for (vsize i = slurs_.size (); i--;)
    Slur::add_column (slurs_[i], e);
  for (vsize i = end_slurs_.size (); i--;)
    Slur::add_column (end_slurs_[i], e);
}

void
Phrasing_slur_engraver::acknowledge_extra_object (Grob_info info)
{
  Slur::auxiliary_acknowledge_extra_object (info, slurs_, end_slurs_);
}

void
Phrasing_slur_engraver::acknowledge_accidental (Grob_info info)
{
  acknowledge_extra_object (info);
}

void
Phrasing_slur_engraver::acknowledge_fingering (Grob_info info)
{
  acknowledge_extra_object (info);
}

void
Phrasing_slur_engraver::acknowledge_text_script (Grob_info info)
{
  acknowledge_extra_object (info);
}

void
Phrasing_slur_engraver::acknowledge_script (Grob_info info)
{
  if (!info.grob ()->internal_has_interface (ly_symbol2scm ("dynamic-interface")))
    acknowledge_extra_object (info);
}

void
Phrasing_slur_engraver::acknowledge_tie (Grob_info info)
{
  acknowledge_extra_object (info);
}

void
Phrasing_slur_engraver::acknowledge_tuplet_number (Grob_info info)
{
  acknowledge_extra_object (info);
}

void
Phrasing_slur_engraver::acknowledge_slur (Grob_info info)
{
  acknowledge_extra_object (info);
}

void
Phrasing_slur_engraver::finalize ()
{
  if (slurs_.size ())
    slurs_[0]->warning (_ ("unterminated phrasing slur"));
}

void
Phrasing_slur_engraver::process_music ()
{
  if (events_[STOP])
    {
      end_slurs_ = slurs_;
      slurs_.clear ();
    }

  if (events_[START] && slurs_.empty ())
    {
      Stream_event *ev = events_[START];

      Grob *slur = make_spanner ("PhrasingSlur", events_[START]->self_scm ());
      Direction updown = to_dir (ev->get_property ("direction"));
      if (updown)
	set_grob_direction (slur, updown);

      slurs_.push_back (slur);
    }
}

void
Phrasing_slur_engraver::stop_translation_timestep ()
{
  end_slurs_.clear ();
  events_[START] = events_[STOP] = 0;
}

ADD_ACKNOWLEDGER (Phrasing_slur_engraver, accidental);
ADD_ACKNOWLEDGER (Phrasing_slur_engraver, fingering)
ADD_ACKNOWLEDGER (Phrasing_slur_engraver, note_column);
ADD_ACKNOWLEDGER (Phrasing_slur_engraver, slur);
ADD_ACKNOWLEDGER (Phrasing_slur_engraver, script);
ADD_ACKNOWLEDGER (Phrasing_slur_engraver, text_script);
ADD_ACKNOWLEDGER (Phrasing_slur_engraver, tie);
ADD_ACKNOWLEDGER (Phrasing_slur_engraver, tuplet_number);

ADD_TRANSLATOR (Phrasing_slur_engraver,
		/* doc */
		"Print phrasing slurs.  Similar to @ref{Slur_engraver}.",

		/* create */
		"PhrasingSlur ",

		/* read */
		"",

		/* write */
		""
		);
