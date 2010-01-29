/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
  NOTE NOTE NOTE

  This is largely similar to Phrasing_slur_engraver. Check if fixes
  apply there too.

  (on principle, engravers don't use inheritance for code sharing)
  
 */

/*
  It is possible that a slur starts and ends on the same note.  At
  least, it is for phrasing slurs: a note can be both beginning and
  ending of a phrase.
*/
class Slur_engraver : public Engraver
{
  Drul_array<Stream_event *> events_;
  Stream_event *running_slur_start_;
  vector<Grob*> slurs_;
  vector<Grob*> end_slurs_;

  void set_melisma (bool);

protected:
  DECLARE_TRANSLATOR_LISTENER (slur);
  DECLARE_ACKNOWLEDGER (accidental);
  DECLARE_ACKNOWLEDGER (fingering);
  DECLARE_ACKNOWLEDGER (note_column);
  DECLARE_ACKNOWLEDGER (script);
  DECLARE_ACKNOWLEDGER (text_script);
  DECLARE_ACKNOWLEDGER (tie);
  DECLARE_ACKNOWLEDGER (tuplet_number);

  void acknowledge_extra_object (Grob_info);
  void stop_translation_timestep ();
  void process_music ();

  virtual void finalize ();


public:
  TRANSLATOR_DECLARATIONS (Slur_engraver);
};

Slur_engraver::Slur_engraver ()
{
  events_[START] = events_[STOP] = 0;
}

IMPLEMENT_TRANSLATOR_LISTENER (Slur_engraver, slur);
void
Slur_engraver::listen_slur (Stream_event *ev)
{
  Direction d = to_dir (ev->get_property ("span-direction"));
  if (d == START)
    ASSIGN_EVENT_ONCE (events_[START], ev);
  else if (d == STOP)
    ASSIGN_EVENT_ONCE (events_[STOP], ev);
  else ev->origin ()->warning (_f ("direction of %s invalid: %d",
				   "slur-event", int (d)));
}

void
Slur_engraver::set_melisma (bool m)
{
  context ()->set_property ("slurMelismaBusy", m ? SCM_BOOL_T : SCM_BOOL_F);
}

void
Slur_engraver::acknowledge_note_column (Grob_info info)
{
  Grob *e = info.grob ();
  for (vsize i = slurs_.size (); i--;)
    Slur::add_column (slurs_[i], e);
  for (vsize i = end_slurs_.size (); i--;)
    Slur::add_column (end_slurs_[i], e);
}

void
Slur_engraver::acknowledge_extra_object (Grob_info info)
{
  Slur::auxiliary_acknowledge_extra_object (info, slurs_, end_slurs_);
}

void
Slur_engraver::acknowledge_accidental (Grob_info info)
{
  acknowledge_extra_object (info);
}

void
Slur_engraver::acknowledge_fingering (Grob_info info)
{
  acknowledge_extra_object (info);
}

void
Slur_engraver::acknowledge_tuplet_number (Grob_info info)
{
  acknowledge_extra_object (info);
}


void
Slur_engraver::acknowledge_script (Grob_info info)
{
  if (!info.grob ()->internal_has_interface (ly_symbol2scm ("dynamic-interface")))
    acknowledge_extra_object (info);
}

void
Slur_engraver::acknowledge_text_script (Grob_info info)
{
  acknowledge_extra_object (info);
}

void
Slur_engraver::acknowledge_tie (Grob_info info)
{
  acknowledge_extra_object (info);
}

void
Slur_engraver::finalize ()
{
  if (slurs_.size ())
    {
      slurs_[0]->warning (_ ("unterminated slur"));
      for (vsize i = 0; i < slurs_.size (); i++)
	slurs_[i]->suicide ();
    }
}

void
Slur_engraver::process_music ()
{
  if (events_[STOP])
    {
      if (slurs_.size () == 0)
	events_[STOP]->origin ()->warning (_ ("cannot end slur"));

      
      end_slurs_ = slurs_;
      slurs_.clear ();
    }

  if (events_[START] && slurs_.empty ())
    {
      Stream_event *ev = events_[START];

      bool double_slurs = to_boolean (get_property ("doubleSlurs"));

      Grob *slur = make_spanner ("Slur", events_[START]->self_scm ());
      Direction updown = to_dir (ev->get_property ("direction"));
      if (updown && !double_slurs)
	set_grob_direction (slur, updown);

      slurs_.push_back (slur);

      if (double_slurs)
	{
	  set_grob_direction (slur, DOWN);
	  slur = make_spanner ("Slur", events_[START]->self_scm ());
	  set_grob_direction (slur, UP);
	  slurs_.push_back (slur);
	}
    }
  set_melisma (slurs_.size ());
}

void
Slur_engraver::stop_translation_timestep ()
{
  if (Grob *g = unsmob_grob (get_property ("currentCommandColumn")))
    {
      for (vsize i = 0; i < end_slurs_.size (); i++)
	Slur::add_extra_encompass (end_slurs_[i], g);

      if (!events_[START])
	for (vsize i = 0; i < slurs_.size (); i++)
	  Slur::add_extra_encompass (slurs_[i], g);
    }
  
  
  for (vsize i = 0; i < end_slurs_.size (); i++)
    {
      Spanner * s = dynamic_cast<Spanner*> (end_slurs_[i]);
      if (!s->get_bound (RIGHT))
	s->set_bound (RIGHT, unsmob_grob (get_property ("currentMusicalColumn")));
      announce_end_grob (s, SCM_EOL);
    }
  end_slurs_.clear ();
  events_[START] = events_[STOP] = 0;
}

ADD_ACKNOWLEDGER (Slur_engraver, accidental);
ADD_ACKNOWLEDGER (Slur_engraver, fingering);
ADD_ACKNOWLEDGER (Slur_engraver, note_column);
ADD_ACKNOWLEDGER (Slur_engraver, script);
ADD_ACKNOWLEDGER (Slur_engraver, text_script);
ADD_ACKNOWLEDGER (Slur_engraver, tie);
ADD_ACKNOWLEDGER (Slur_engraver, tuplet_number);
ADD_TRANSLATOR (Slur_engraver,
		/* doc */
		"Build slur grobs from slur events.",

		/* create */
		"Slur ",

		/* read */
		"slurMelismaBusy "
		"doubleSlurs ",

		/* write */
		""
		);
