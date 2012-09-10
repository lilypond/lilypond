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

#include <algorithm>

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

Slur_info::Slur_info (Grob *slur)
{
  slur_ = slur;
}

class Slur_engraver : public Engraver
{
  vector<Stream_event *> start_events_;
  vector<Stream_event *> stop_events_;
  vector<Slur_info> slur_infos_;
  vector<Slur_info> end_slur_infos_;
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
  DECLARE_END_ACKNOWLEDGER (tie);
  DECLARE_ACKNOWLEDGER (tuplet_number);

  void acknowledge_extra_object (Grob_info);
  void stop_translation_timestep ();
  void process_music ();

  virtual void finalize ();
  virtual void derived_mark () const;

public:
  TRANSLATOR_DECLARATIONS (Slur_engraver);
};

Slur_engraver::Slur_engraver ()
{
}

void
Slur_engraver::derived_mark () const
{
  for (vsize i = start_events_.size (); i--;)
    scm_gc_mark (start_events_[i]->self_scm ());
  for (vsize i = stop_events_.size (); i--;)
    scm_gc_mark (stop_events_[i]->self_scm ());
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
  /*
   * For every active slur, we create a slur stub.
   * As we do not yet know what vertical axis groups note columns belong to,
   * we create a stub for each note and then suicide duplicate stubs on
   * axis groups.
   * These slurs should be used ONLY to approximate cross-staff slurs
   * in vertical skylines.
   */

  Grob *e = info.grob ();
  for (vsize i = slur_infos_.size (); i--;)
    {
      Slur::add_column (slur_infos_[i].slur_, e);
      Grob *stub = make_spanner ("SlurStub", info.grob ()->self_scm ());
      slur_infos_[i].stubs_.push_back (stub);
    }
  for (vsize i = end_slur_infos_.size (); i--;)
    {
      Slur::add_column (end_slur_infos_[i].slur_, e);
      Grob *stub = make_spanner ("SlurStub", info.grob ()->self_scm ());
      end_slur_infos_[i].stubs_.push_back (stub);
    }
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
Slur_engraver::acknowledge_end_tie (Grob_info info)
{
  acknowledge_extra_object (info);
}

void
Slur_engraver::finalize ()
{
  for (vsize i = 0; i < slur_infos_.size (); i++)
    {
      slur_infos_[i].slur_->warning (_ ("unterminated slur"));
      slur_infos_[i].slur_->suicide ();
      for (vsize j = 0; j < slur_infos_[i].stubs_.size (); j++)
        slur_infos_[i].stubs_[j]->suicide ();
    }

  slur_infos_.clear ();
}

void
Slur_engraver::process_music ()
{
  for (vsize i = 0; i < stop_events_.size (); i++)
    {
      Stream_event *ev = stop_events_[i];
      string id = robust_scm2string (ev->get_property ("spanner-id"), "");

      // Find the slurs that are ended with this event (by checking the spanner-id)
      bool ended = false;
      for (vsize j = slur_infos_.size (); j--;)
        {
          if (id == robust_scm2string (slur_infos_[j].slur_->get_property ("spanner-id"), ""))
            {
              ended = true;
              end_slur_infos_.push_back (slur_infos_[j]);
              slur_infos_.erase (slur_infos_.begin () + j);
            }
        }
      if (ended)
        {
          // Ignore redundant stop events for this id
          for (vsize j = stop_events_.size (); --j > i;)
            {
              if (id == robust_scm2string (stop_events_[j]->get_property ("spanner-id"), ""))
                stop_events_.erase (stop_events_.begin () + j);
            }
        }
      else
        ev->origin ()->warning (_ ("cannot end slur"));
    }

  vsize old_slurs = slur_infos_.size ();
  for (vsize i = start_events_.size (); i--;)
    {
      Stream_event *ev = start_events_[i];
      string id = robust_scm2string (ev->get_property ("spanner-id"), "");
      Direction updown = to_dir (ev->get_property ("direction"));

      bool completed;
      for (vsize j = slur_infos_.size (); !(completed = (j-- == 0));)
        {
          // Check if we already have a slur with the same spanner-id.
          if (id == robust_scm2string (slur_infos_[j].slur_->get_property ("spanner-id"), ""))
            {
              if (j < old_slurs)
                {
                  // We already have an old slur, so give a warning
                  // and completely ignore the new slur.
                  ev->origin ()->warning (_ ("already have slur"));
                  start_events_.erase (start_events_.begin () + i);
                  break;
                }

              // If this slur event has no direction, it will not
              // contribute anything new to the existing slur(s), so
              // we can ignore it.

              if (!updown)
                break;

              Stream_event *c = unsmob_stream_event (slur_infos_[j].slur_->get_property ("cause"));

              if (!c)
                {
                  slur_infos_[j].slur_->programming_error ("slur without a cause");
                  continue;
                }

              Direction slur_dir = to_dir (c->get_property ("direction"));

              // If the existing slur does not have a direction yet,
              // we'd rather take the new one.

              if (!slur_dir)
                {
                  slur_infos_[j].slur_->suicide ();
                  for (vsize k = 0; k < slur_infos_[j].stubs_.size (); k++)
                    slur_infos_[j].stubs_[k]->suicide ();
                  slur_infos_.erase (slur_infos_.begin () + j);
                  continue;
                }

              // If the existing slur has the same direction as ours, drop ours

              if (slur_dir == updown)
                break;
            }
        }
      // If the loop completed, our slur is new
      if (completed)
        {
          Grob *slur = make_spanner ("Slur", ev->self_scm ());
          slur->set_property ("spanner-id", ly_string2scm (id));
          if (updown)
            set_grob_direction (slur, updown);
          slur_infos_.push_back (Slur_info (slur));

          if (to_boolean (get_property ("doubleSlurs")))
            {
              set_grob_direction (slur, DOWN);
              slur = make_spanner ("Slur", ev->self_scm ());
              slur->set_property ("spanner-id", ly_string2scm (id));
              set_grob_direction (slur, UP);
              slur_infos_.push_back (Slur_info (slur));
            }
        }
    }
  set_melisma (slur_infos_.size ());
}

void
Slur_engraver::stop_translation_timestep ()
{
  if (Grob *g = unsmob_grob (get_property ("currentCommandColumn")))
    {
      for (vsize i = 0; i < end_slur_infos_.size (); i++)
        Slur::add_extra_encompass (end_slur_infos_[i].slur_, g);

      if (!start_events_.size ())
        for (vsize i = 0; i < slur_infos_.size (); i++)
          Slur::add_extra_encompass (slur_infos_[i].slur_, g);
    }

  for (vsize i = 0; i < end_slur_infos_.size (); i++)
    {
      Spanner *s = dynamic_cast<Spanner *> (end_slur_infos_[i].slur_);
      if (!s->get_bound (RIGHT))
        s->set_bound (RIGHT, unsmob_grob (get_property ("currentMusicalColumn")));
      announce_end_grob (s, SCM_EOL);
    }

  for (vsize i = 0; i < objects_to_acknowledge_.size (); i++)
    Slur::auxiliary_acknowledge_extra_object (objects_to_acknowledge_[i], slur_infos_, end_slur_infos_);

  for (vsize i = 0; i < end_slur_infos_.size (); i++)
    {
      // There are likely SlurStubs we don't need. Get rid of them
      // and only keep one per VerticalAxisGroup.
      vector<Grob *> vags;
      vector<Grob *> stubs;
      for (vsize j = 0; j < end_slur_infos_[i].stubs_.size (); j++)
        {
          Grob *stub = end_slur_infos_[i].stubs_[j];
          Grob *vag = Grob::get_vertical_axis_group (stub);
          if (vag)
            {
              vector<Grob *>::const_iterator it =
                find (vags.begin (), vags.end (), vag);
              if (it != vags.end ())
                stub->suicide ();
              else
                {
                  vags.push_back (vag);
                  stubs.push_back (stub);
                }
            }
          else
            {
              end_slur_infos_[i].slur_->programming_error ("Cannot find vertical axis group for NoteColumn.");
              stub->suicide ();
            }
        }
      for (vsize j = 0; j < stubs.size (); j++)
        Slur::main_to_stub (end_slur_infos_[i].slur_, stubs[j]);
    }

  objects_to_acknowledge_.clear ();
  end_slur_infos_.clear ();
  start_events_.clear ();
  stop_events_.clear ();
}

ADD_ACKNOWLEDGER (Slur_engraver, inline_accidental);
ADD_ACKNOWLEDGER (Slur_engraver, fingering);
ADD_ACKNOWLEDGER (Slur_engraver, note_column);
ADD_ACKNOWLEDGER (Slur_engraver, script);
ADD_ACKNOWLEDGER (Slur_engraver, text_script);
ADD_ACKNOWLEDGER (Slur_engraver, dots);
ADD_END_ACKNOWLEDGER (Slur_engraver, tie);
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
