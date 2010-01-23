/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2010 Juergen Reuter <reuter@ipd.uka.de>,
  Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "bar-line.hh"
#include "item.hh"
#include "note-head.hh"
#include "pitch.hh"
#include "staff-symbol-referencer.hh"
#include "stream-event.hh"
#include "warn.hh"

#include "translator.icc"

/*
 * This class implements an engraver for custos symbols.
 *
 * FIXME: note heads inside of ligatures (i.e. ligature heads) are
 * sometimes not recognized by this engraver. --jr
 */
class Custos_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Custos_engraver);
  void start_translation_timestep ();
  DECLARE_ACKNOWLEDGER (bar);
  DECLARE_ACKNOWLEDGER (note_head);
  void process_acknowledged ();
  void stop_translation_timestep ();
  virtual void finalize ();

private:
  Item *create_custos ();
  bool custos_permitted_;
  vector<Grob*> custodes_;
  vector<Pitch> pitches_;
};

Custos_engraver::Custos_engraver ()
{
  custos_permitted_ = false;
}

void
Custos_engraver::stop_translation_timestep ()
{
  /*
    delay typeset until we're at the next moment, so we can silence custodes at the end of the piece.
  */
  pitches_.clear ();

  custos_permitted_ = false;
}

void
Custos_engraver::start_translation_timestep ()
{
  custodes_.clear ();
}

void
Custos_engraver::acknowledge_bar (Grob_info /* info */)
{
  custos_permitted_ = true;
}

void
Custos_engraver::acknowledge_note_head (Grob_info info)
{
  Stream_event *ev = info.event_cause ();
  if (ev && ev->in_event_class ("note-event"))
    {

      /*
	ideally, we'd do custos->set_parent (Y_AXIS, notehead),
	but since the note head lives on the other system, we can't

	So we copy the position from the note head pitch.  We
	don't look at the staff-position, since we can't be sure
	whether Clef_engraver already applied a vertical shift.
      */
      pitches_.push_back (*unsmob_pitch (ev->get_property ("pitch")));
    }
}

void
Custos_engraver::process_acknowledged ()
{
  if (scm_is_string (get_property ("whichBar")))
    custos_permitted_ = true;

  if (custos_permitted_)
    {
      for (vsize i = pitches_.size (); i--;)
	{
	  Item *c = create_custos ();

	  int p = pitches_[i].steps ();
	  SCM c0 = get_property ("middleCPosition");
	  if (scm_is_number (c0))
	    p += scm_to_int (c0);

	  c->set_property ("staff-position",
			   scm_from_int (p));
	}

      pitches_.clear ();
    }
}

Item *
Custos_engraver::create_custos ()
{
  Item *custos = make_item ("Custos", SCM_EOL);

  custodes_.push_back (custos);

  return custos;
}

void
Custos_engraver::finalize ()
{
  for (vsize i = custodes_.size (); i--;)
    custodes_[i]->suicide ();
  custodes_.clear ();
}

ADD_ACKNOWLEDGER (Custos_engraver, bar);
ADD_ACKNOWLEDGER (Custos_engraver, note_head);

ADD_TRANSLATOR (Custos_engraver,
		/* doc */
		"Engrave custodes.",

		/* create */
		"Custos ",

		/* read */
		"",

		/* write */
		""
		);
