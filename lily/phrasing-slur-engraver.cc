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

  This is largely similar to Slur_engraver. Check if fixes
  apply there too.

  (on principle, engravers don't use inheritance for code sharing)

 */

/*
  It is possible that a slur starts and ends on the same note.  At
  least, it is for phrasing slurs: a note can be both beginning and
  ending of a phrase.
*/
class Phrasing_slur_engraver : public Engraver
{
  vector<Stream_event *> start_events_;
  vector<Stream_event *> stop_events_;
  vector<Grob *> slurs_;
  vector<Grob *> end_slurs_;
  vector<Grob_info> objects_to_acknowledge_;

protected:
  DECLARE_TRANSLATOR_LISTENER (phrasing_slur);
  DECLARE_ACKNOWLEDGER (inline_accidental);
  DECLARE_ACKNOWLEDGER (fingering);
  DECLARE_ACKNOWLEDGER (note_column);
  DECLARE_ACKNOWLEDGER (slur);
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
  TRANSLATOR_DECLARATIONS (Phrasing_slur_engraver);
};

Phrasing_slur_engraver::Phrasing_slur_engraver ()
{
}

IMPLEMENT_TRANSLATOR_LISTENER (Phrasing_slur_engraver, phrasing_slur);
void
Phrasing_slur_engraver::listen_phrasing_slur (Stream_event *ev)
{
  Direction d = to_dir (ev->get_property ("span-direction"));
  if (d == START)
    start_events_.push_back (ev);
  else if (d == STOP)
    stop_events_.push_back (ev);
  else ev->origin ()->warning (_f ("direction of %s invalid: %d",
                                     "phrasing-slur-event", int (d)));
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
  objects_to_acknowledge_.push_back (info);
}

void
Phrasing_slur_engraver::acknowledge_inline_accidental (Grob_info info)
{
  acknowledge_extra_object (info);
}

void
Phrasing_slur_engraver::acknowledge_dots (Grob_info info)
{
  acknowledge_extra_object (info);
}

void
Phrasing_slur_engraver::acknowledge_fingering (Grob_info info)
{
  acknowledge_extra_object (info);
}

void
Phrasing_slur_engraver::acknowledge_tuplet_number (Grob_info info)
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
Phrasing_slur_engraver::acknowledge_text_script (Grob_info info)
{
  acknowledge_extra_object (info);
}

void
Phrasing_slur_engraver::acknowledge_tie (Grob_info info)
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
  for (vsize i = 0; i < slurs_.size (); i++)
    {
      slurs_[i]->warning (_ ("unterminated phrasing slur"));
      slurs_[i]->suicide ();
    }
}

void
Phrasing_slur_engraver::process_music ()
{
  for (vsize i = 0; i < stop_events_.size (); i++)
    {
      Stream_event *ev = stop_events_[i];
      string id = robust_scm2string (ev->get_property ("spanner-id"), "");

      // Find the slur that is ended with this event (by checking the spanner-id)
      bool ended = false;
      for (vsize j = slurs_.size (); j--;)
        {
          if (id == robust_scm2string (slurs_[j]->get_property ("spanner-id"), ""))
            {
              ended = true;
              end_slurs_.push_back (slurs_[j]);
              slurs_.erase (slurs_.begin () + j);
            }
        }
      if (!ended)
        ev->origin ()->warning (_ ("cannot end phrasing slur"));
    }

  for (vsize i = 0; i < start_events_.size (); i++)
    {
      Stream_event *ev = start_events_[i];
      string id = robust_scm2string (ev->get_property ("spanner-id"), "");
      bool have_slur = false;
      // Check if we already have a slur with the same spanner-id.
      // In that case, don't create a new slur, but print a warning
      for (vsize i = 0; i < slurs_.size (); i++)
        have_slur = have_slur || (id == robust_scm2string (slurs_[i]->get_property ("spanner-id"), ""));

      if (have_slur)
        ev->origin ()->warning (_ ("already have phrasing slur"));
      else
        {
          Grob *slur = make_spanner ("PhrasingSlur", ev->self_scm ());
          Direction updown = to_dir (ev->get_property ("direction"));
          slur->set_property ("spanner-id", ly_string2scm (id));
          if (updown)
            set_grob_direction (slur, updown);
          slurs_.push_back (slur);
        }
    }
}

void
Phrasing_slur_engraver::stop_translation_timestep ()
{
  for (vsize i = 0; i < objects_to_acknowledge_.size (); i++)
    Slur::auxiliary_acknowledge_extra_object (objects_to_acknowledge_[i], slurs_, end_slurs_);

  end_slurs_.clear ();
  start_events_.clear ();
  stop_events_.clear ();
  objects_to_acknowledge_.clear ();
}

ADD_ACKNOWLEDGER (Phrasing_slur_engraver, inline_accidental);
ADD_ACKNOWLEDGER (Phrasing_slur_engraver, fingering)
ADD_ACKNOWLEDGER (Phrasing_slur_engraver, note_column);
ADD_ACKNOWLEDGER (Phrasing_slur_engraver, slur);
ADD_ACKNOWLEDGER (Phrasing_slur_engraver, script);
ADD_ACKNOWLEDGER (Phrasing_slur_engraver, dots);
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
