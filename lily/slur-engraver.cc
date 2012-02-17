/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2012 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
  vector<Stream_event *> start_events_;
  vector<Stream_event *> stop_events_;
  vector<Grob *> slurs_;
  vector<Grob *> end_slurs_;
  vector<Grob_info> objects_to_acknowledge_;

  void set_melisma (bool);

protected:
  DECLARE_TRANSLATOR_LISTENER (slur);
  DECLARE_ACKNOWLEDGER (inline_accidental);
  DECLARE_ACKNOWLEDGER (fingering);
  DECLARE_ACKNOWLEDGER (note_column);
  DECLARE_ACKNOWLEDGER (script);
  DECLARE_ACKNOWLEDGER (dots);
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
}

IMPLEMENT_TRANSLATOR_LISTENER (Slur_engraver, slur);
void
Slur_engraver::listen_slur (Stream_event *ev)
{
  Direction d = to_dir (ev->get_property ("span-direction"));
  if (d == START)
    start_events_.push_back (ev);
  else if (d == STOP)
    stop_events_.push_back (ev);
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
  objects_to_acknowledge_.push_back (info);
}

void
Slur_engraver::acknowledge_inline_accidental (Grob_info info)
{
  acknowledge_extra_object (info);
}

void
Slur_engraver::acknowledge_dots (Grob_info info)
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
  for (vsize i = 0; i < slurs_.size (); i++)
    {
      slurs_[i]->warning (_ ("unterminated slur"));
      slurs_[i]->suicide ();
    }
}

void
Slur_engraver::process_music ()
{
  for (vsize i = 0; i < stop_events_.size (); i++)
    {
      Stream_event *ev = stop_events_[i];
      string id = robust_scm2string (ev->get_property ("spanner-id"), "");

      // Find the slur that is ended with this event (by checking the spanner-id)
      bool ended = false;
      SCM starter = SCM_BOOL_F;
      for (vsize j = slurs_.size (); j--;)
        {
          if (id == robust_scm2string (slurs_[j]->get_property ("spanner-id"), ""))
            {
	      // We end only one slur unless several ones have been
	      // caused by the same event, like with double slurs.
              if (!ended || scm_is_eq (starter,
				       slurs_[j]->get_property ("cause")))
		{
		  ended = true;
		  starter = slurs_[j]->get_property ("cause");
		  end_slurs_.push_back (slurs_[j]);
		  slurs_.erase (slurs_.begin () + j);
		}
            }
        }
      if (!ended)
        ev->origin ()->warning (_ ("cannot end slur"));
    }

  for (vsize i = start_events_.size (); i--;)
    {
      Stream_event *ev = start_events_[i];
      string id = robust_scm2string (ev->get_property ("spanner-id"), "");
      bool have_slur = false;
      // Check if we already have a slur with the same spanner-id.
      // In that case, don't create a new slur, but print a warning
      for (vsize j = 0; j < slurs_.size (); j++)
        have_slur = have_slur || (id == robust_scm2string (slurs_[j]->get_property ("spanner-id"), ""));

      if (have_slur)
        {
          // We already have a slur, so give a warning and completely ignore
          // the new slur.
          ev->origin ()->warning (_ ("already have slur"));
          start_events_.erase (start_events_.begin () + i);
        }
    }
  for (vsize i = start_events_.size (); i--;)
    {
      Stream_event *ev = start_events_[i];
      string id = robust_scm2string (ev->get_property ("spanner-id"), "");

          Grob *slur = make_spanner ("Slur", ev->self_scm ());
          Direction updown = to_dir (ev->get_property ("direction"));
          slur->set_property ("spanner-id", ly_string2scm (id));
          if (updown)
            set_grob_direction (slur, updown);
          slurs_.push_back (slur);

          if (to_boolean (get_property ("doubleSlurs")))
            {
              set_grob_direction (slur, DOWN);
              slur = make_spanner ("Slur", ev->self_scm ());
              slur->set_property ("spanner-id", ly_string2scm (id));
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

      if (!start_events_.size ())
        for (vsize i = 0; i < slurs_.size (); i++)
          Slur::add_extra_encompass (slurs_[i], g);
    }

  for (vsize i = 0; i < end_slurs_.size (); i++)
    {
      Spanner *s = dynamic_cast<Spanner *> (end_slurs_[i]);
      if (!s->get_bound (RIGHT))
        s->set_bound (RIGHT, unsmob_grob (get_property ("currentMusicalColumn")));
      announce_end_grob (s, SCM_EOL);
    }

  for (vsize i = 0; i < objects_to_acknowledge_.size (); i++)
    Slur::auxiliary_acknowledge_extra_object (objects_to_acknowledge_[i], slurs_, end_slurs_);

  objects_to_acknowledge_.clear ();
  end_slurs_.clear ();
  start_events_.clear ();
  stop_events_.clear ();
}

ADD_ACKNOWLEDGER (Slur_engraver, inline_accidental);
ADD_ACKNOWLEDGER (Slur_engraver, fingering);
ADD_ACKNOWLEDGER (Slur_engraver, note_column);
ADD_ACKNOWLEDGER (Slur_engraver, script);
ADD_ACKNOWLEDGER (Slur_engraver, text_script);
ADD_ACKNOWLEDGER (Slur_engraver, dots);
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
