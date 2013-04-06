/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2013 Mike Solomon <mike@mikesolomon.org>

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
#include "slur-proto-engraver.hh"
#include "spanner.hh"
#include "stream-event.hh"
#include "warn.hh"

#include "translator.icc"

void
Slur_proto_engraver::derived_mark () const
{
  for (vsize i = start_events_.size (); i--;)
    scm_gc_mark (start_events_[i]->self_scm ());
  for (vsize i = stop_events_.size (); i--;)
    scm_gc_mark (stop_events_[i]->self_scm ());
}

void
Slur_proto_engraver::internal_listen_slur (Stream_event *ev)
{
  Direction d = to_dir (ev->get_property ("span-direction"));
  if (d == START)
    start_events_.push_back (ev);
  else if (d == STOP)
    stop_events_.push_back (ev);
  else ev->origin ()->warning (_f ("direction of %s invalid: %d",
                                     event_name_, int (d)));
}

void
Slur_proto_engraver::internal_listen_break_slur (Stream_event *ev)
{
  // if break_slur_ is set, we only keep events with direction
  if (break_slur_
      && robust_scm2dir (ev->get_property ("span-direction"), CENTER))
    break_slur_ = ev;
  else if (!break_slur_)
    break_slur_ = ev;
  else if (break_slur_
           && robust_scm2dir (break_slur_->get_property ("span-direction"), CENTER)
           && robust_scm2dir (ev->get_property ("span-direction"), CENTER))
    ev->origin ()->warning (_f ("cannot set break slur with two directions"));
}


void
Slur_proto_engraver::acknowledge_note_column (Grob_info info)
{
  Grob *e = info.grob ();
  for (vsize i = slurs_.size (); i--;)
    Slur::add_column (slurs_[i], e);
  for (vsize i = end_slurs_.size (); i--;)
    Slur::add_column (end_slurs_[i], e);
}

void
Slur_proto_engraver::acknowledge_extra_object (Grob_info info)
{
  objects_to_acknowledge_.push_back (info);
}

void
Slur_proto_engraver::acknowledge_inline_accidental (Grob_info info)
{
  acknowledge_extra_object (info);
}

void
Slur_proto_engraver::acknowledge_dots (Grob_info info)
{
  acknowledge_extra_object (info);
}

void
Slur_proto_engraver::acknowledge_fingering (Grob_info info)
{
  acknowledge_extra_object (info);
}

void
Slur_proto_engraver::acknowledge_tuplet_number (Grob_info info)
{
  acknowledge_extra_object (info);
}

void
Slur_proto_engraver::acknowledge_script (Grob_info info)
{
  if (!info.grob ()->internal_has_interface (ly_symbol2scm ("dynamic-interface")))
    acknowledge_extra_object (info);
}

void
Slur_proto_engraver::acknowledge_text_script (Grob_info info)
{
  acknowledge_extra_object (info);
}

void
Slur_proto_engraver::acknowledge_end_tie (Grob_info info)
{
  acknowledge_extra_object (info);
}

void
Slur_proto_engraver::finalize ()
{
  for (vsize i = 0; i < slurs_.size (); i++)
    {
      slurs_[i]->warning (_f ("unterminated %s", object_name_));
      slurs_[i]->suicide ();
    }
  slurs_.clear ();
}

void
Slur_proto_engraver::create_slur (string spanner_id, Stream_event *ev_cause, Grob *g_cause, Direction dir, bool left_broken)
{
  Grob *ccc = unsmob_grob (get_property ("currentCommandColumn"));
  SCM cause = ev_cause ? ev_cause->self_scm () : g_cause->self_scm ();
  Spanner *slur = make_spanner (grob_name_, cause);
  slur->set_property ("spanner-id", ly_string2scm (spanner_id));
  if (dir)
    set_grob_direction (slur, dir);
  if (left_broken)
    slur->set_bound (LEFT, ccc);
  slurs_.push_back (slur);
  if (double_property_name_
      && to_boolean (get_property (double_property_name_)))
    {
      set_grob_direction (slur, DOWN);
      slur = make_spanner (grob_name_, cause);
      slur->set_property ("spanner-id", ly_string2scm (spanner_id));
      set_grob_direction (slur, UP);
      if (left_broken)
        slur->set_bound (LEFT, ccc);
      slurs_.push_back (slur);
    }
  else if (g_cause && Slur::has_interface (g_cause) && left_broken)
    {
      g_cause->set_object ("other-half", slur->self_scm ());
      slur->set_object ("other-half", g_cause->self_scm ());
    }
}

bool
Slur_proto_engraver::can_create_slur (string id, vsize old_slurs, vsize *event_idx, Stream_event *ev)
{
  for (vsize j = slurs_.size (); j--;)
    {
      Grob *slur = slurs_[j];
      Direction updown = to_dir (ev->get_property ("direction"));

      // Check if we already have a slur with the same spanner-id.
      if (id == robust_scm2string (slur->get_property ("spanner-id"), ""))
        {
          if (j < old_slurs)
            {
              // We already have an old slur, so give a warning
              // and completely ignore the new slur.
              ev->origin ()->warning (_f ("already have %s", object_name_));
              if (event_idx)
                start_events_.erase (start_events_.begin () + (*event_idx));
              return false;
            }

          // If this slur event has no direction, it will not
          // contribute anything new to the existing slur(s), so
          // we can ignore it.

          if (!updown)
            return false;

          Stream_event *c = unsmob_stream_event (slur->get_property ("cause"));

          if (!c)
            {
              slur->programming_error (_f ("%s without a cause", object_name_));
              return true;
            }

          Direction slur_dir = to_dir (c->get_property ("direction"));

          // If the existing slur does not have a direction yet,
          // we'd rather take the new one.

          if (!slur_dir)
            {
              slur->suicide ();
              slurs_.erase (slurs_.begin () + j);
              return true;
            }

          // If the existing slur has the same direction as ours, drop ours

          if (slur_dir == updown)
            return false;
        }
    }
  return true;
}

bool
Slur_proto_engraver::try_to_end (Stream_event *ev)
{
  string id = robust_scm2string (ev->get_property ("spanner-id"), "");

  // Find the slurs that are ended with this event (by checking the spanner-id)
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
  return ended;
}

void
Slur_proto_engraver::break_slurs ()
{
  for (vsize i = slurs_.size (); i--;)
    {
      Grob *ccc = unsmob_grob (get_property ("currentCommandColumn"));
      Spanner *s = dynamic_cast<Spanner *> (slurs_[i]);
      s->set_bound (RIGHT, ccc);
      announce_end_grob (s, SCM_EOL);
      slurs_.erase (slurs_.begin () + i);
      SCM maybe_dir = s->get_property_data ("direction");
      Direction dir = is_direction (maybe_dir)
                      ? robust_scm2dir (maybe_dir, CENTER)
                      : CENTER;
      create_slur (robust_scm2string (s->get_property ("spanner-id"), ""),
                   0, s, dir, true);
    }
}

void
Slur_proto_engraver::process_music ()
{
  // break slurs that span over this column
  // if break_slur_'s direction is center
  if (break_slur_
      && robust_scm2dir (break_slur_->get_property ("span-direction"), CENTER) == CENTER
      && unsmob_grob (get_property ("currentCommandColumn")))
    break_slurs ();

  // create broken slurs starting at this column if break_slur_ is left
  vsize old_slurs = slurs_.size ();
  if (break_slur_
      && robust_scm2dir (break_slur_->get_property ("span-direction"), CENTER) == LEFT
      && can_create_slur ("", old_slurs, 0, break_slur_))
    create_slur ("", break_slur_, 0,
                 robust_scm2dir (break_slur_->get_property ("direction"), CENTER),
                 true);

  for (vsize i = 0; i < stop_events_.size (); i++)
    {
      string id = robust_scm2string (stop_events_[i]->get_property ("spanner-id"), "");
      bool ended = try_to_end (stop_events_[i]);
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
        stop_events_[i]->origin ()->warning (_f ("cannot end %s", object_name_));
    }
  old_slurs = slurs_.size ();

  for (vsize i = start_events_.size (); i--;)
    {
      Stream_event *ev = start_events_[i];
      string id = robust_scm2string (ev->get_property ("spanner-id"), "");
      Direction updown = to_dir (ev->get_property ("direction"));
      if (can_create_slur (id, old_slurs, &i, ev))
        create_slur (id, ev, 0, updown, false);
    }

  // if BreakSlurEvent with span-dir right, we end here
  if (break_slur_
      && robust_scm2dir (break_slur_->get_property ("span-direction"), CENTER) == RIGHT)
    (void) try_to_end (break_slur_);

  set_melisma (slurs_.size ());
}

void
Slur_proto_engraver::set_melisma (bool)
{
}

void
Slur_proto_engraver::stop_translation_timestep ()
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
      // if BreakSlurEvent with span-dir right,
      // we set right bound to current command column
      if (break_slur_
          && robust_scm2dir (break_slur_->get_property ("span-direction"), CENTER) == RIGHT)
        s->set_bound (RIGHT, unsmob_grob (get_property ("currentCommandColumn")));
      announce_end_grob (s, SCM_EOL);
    }

  for (vsize i = 0; i < objects_to_acknowledge_.size (); i++)
    Slur::auxiliary_acknowledge_extra_object (objects_to_acknowledge_[i], slurs_, end_slurs_);

  objects_to_acknowledge_.clear ();
  end_slurs_.clear ();
  start_events_.clear ();
  stop_events_.clear ();
  break_slur_ = 0;
}

// no ADD_ACKNOWLEDGER / ADD_TRANSLATOR macro calls
// since this class is abstract
