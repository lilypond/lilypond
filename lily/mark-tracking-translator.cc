/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2018--2022 Daniel Eble <nine.fierce.ballads@gmail.com>

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

#include "mark-tracking-translator.hh"

#include "context.hh"
#include "international.hh"
#include "stream-event.hh"
#include "warn.hh"

#include "translator.icc"

Mark_tracking_translator::Mark_tracking_translator (Context *c)
  : Translator (c)
{
}

void
Mark_tracking_translator::clear_events ()
{
  if (performance_ev_type_ != Event_type::none)
    {
      performance_ev_ = nullptr;
      performance_ev_type_ = Event_type::none;
      context ()->unset_property (
        ly_symbol2scm ("currentPerformanceMarkEvent"));
    }

  if (rehearsal_ev_type_ != Event_type::none)
    {
      rehearsal_ev_ = nullptr;
      rehearsal_ev_type_ = Event_type::none;
      context ()->unset_property (ly_symbol2scm ("currentRehearsalMarkEvent"));
    }
}

void
Mark_tracking_translator::set_performance_event (Event_type type,
                                                 Stream_event *ev)
{
  performance_ev_ = ev;
  performance_ev_type_ = type;
  set_property (context (), "currentPerformanceMarkEvent", ev->self_scm ());
}

void
Mark_tracking_translator::set_rehearsal_event (Event_type type,
                                               Stream_event *ev)
{
  rehearsal_ev_ = ev;
  rehearsal_ev_type_ = type;
  set_property (context (), "currentRehearsalMarkEvent", ev->self_scm ());
}

bool
Mark_tracking_translator::set_performance_event_once (Event_type type,
                                                      Stream_event *ev)
{
  if (!assign_event_once (performance_ev_, ev))
    return false;
  set_performance_event (type, ev);
  return true;
}

bool
Mark_tracking_translator::set_rehearsal_event_once (Event_type type,
                                                    Stream_event *ev)
{
  if (!assign_event_once (rehearsal_ev_, ev))
    return false;
  set_rehearsal_event (type, ev);
  return true;
}

void
Mark_tracking_translator::stop_translation_timestep ()
{
  // Initialize segnoMarkCount to indicate that we are no longer at the
  // beginning.
  if (first_time_)
    {
      set_property (context (), "codaMarkCount", to_scm (0));
      set_property (context (), "segnoMarkCount", to_scm (0));
    }

  // Update the counter for the chosen mark.  Those for segno and coda marks
  // are incremented at the end of the timestep so that there is no
  // inconsistency in value during iteration or translators' process_music ().
  //
  // The rehearsal mark count is handled differently to support its legacy
  // interface: the user may set the property directly rather than with \mark.
  switch (performance_ev_type_)
    {
    case Event_type::default_coda_mark:
    case Event_type::specific_coda_mark:
      {
        const auto label = get_coda_mark_label (context (), performance_ev_);
        if (label > 0)
          set_property (context (), "codaMarkCount", to_scm (label));
        break;
      }

    case Event_type::default_segno_mark:
    case Event_type::specific_segno_mark:
      {
        const auto label = get_segno_mark_label (context (), performance_ev_);
        if (label > 0)
          set_property (context (), "segnoMarkCount", to_scm (label));
        break;
      }

    default:
      break;
    }

  switch (rehearsal_ev_type_)
    {
    case Event_type::default_rehearsal_mark:
    case Event_type::specific_rehearsal_mark:
      {
        const auto label = get_rehearsal_mark_label (context (), rehearsal_ev_);
        if (label > 0)
          set_property (context (), "rehearsalMark", to_scm (label + 1));
        break;
      }

    default:
      break;
    }

  clear_events ();
  first_time_ = false;
}

size_t
Mark_tracking_translator::get_coda_mark_label (const Context *context,
                                               const Stream_event *ev)
{
  auto n = from_scm<size_t> (get_property (ev, "label"), 0);
  if (n < 1)
    {
      n = from_scm<size_t> (get_property (context, "codaMarkCount"), 0) + 1;
    }
  return n;
}

size_t
Mark_tracking_translator::get_rehearsal_mark_label (const Context *context,
                                                    const Stream_event *ev)
{
  auto n = from_scm<size_t> (get_property (ev, "label"), 0);
  if (n < 1)
    n = from_scm<size_t> (get_property (context, "rehearsalMark"), 0);
  return n;
}

size_t
Mark_tracking_translator::get_segno_mark_label (const Context *context,
                                                const Stream_event *ev)
{
  auto n = from_scm<size_t> (get_property (ev, "label"), 0);
  if (n < 1)
    n = from_scm<size_t> (get_property (context, "segnoMarkCount"), 0) + 1;
  return n;
}

void
Mark_tracking_translator::listen_ad_hoc_mark (Stream_event *ev)
{
  // Ad-hoc marks are not rehearsal marks, but they lead to the creation of
  // RehearsalMark grobs for backward compatibility, so this conflict check is
  // simple: complain about everything to incentivize using something
  // else, such as \sectionLabel, \jump, \textMark or \textEndMark.
  set_rehearsal_event_once (Event_type::ad_hoc_mark, ev);
}

void
Mark_tracking_translator::listen_coda_mark (Stream_event *ev)
{
  SCM label = get_property (ev, "label");
  if (!scm_is_integer (label)) // \codaMark \default
    {
      // Ignore a default coda mark at the beginning of a piece.  There is no
      // use case in mind here; this is merely for consistency with segni.
      if (!first_time_)
        {
          switch (performance_ev_type_)
            {
            // Silently ignore default coda mark events after we have any coda
            // mark event.
            case Event_type::default_coda_mark:
            case Event_type::specific_coda_mark:
              break;

            // Check others.
            default:
              set_performance_event_once (Event_type::default_coda_mark, ev);
              break;
            }
        }
    }
  else // a specific coda mark
    {
      switch (performance_ev_type_)
        {
        // Silently replace a default coda mark.
        case Event_type::default_coda_mark:
          set_performance_event (Event_type::specific_coda_mark, ev);
          break;

        // Check others.
        default:
          set_performance_event_once (Event_type::specific_coda_mark, ev);
          break;
        }
    }
}

void
Mark_tracking_translator::listen_rehearsal_mark (Stream_event *ev)
{
  SCM label = get_property (ev, "label");
  if (!scm_is_integer (label)) // \mark \default
    {
      // Silently ignore default rehearsal mark events after we have any
      // rehearsal mark.
      switch (rehearsal_ev_type_)
        {
        case Event_type::default_rehearsal_mark:
        case Event_type::specific_rehearsal_mark:
          break;

        default:
          set_rehearsal_event_once (Event_type::default_rehearsal_mark, ev);
          break;
        }
    }
  else // a specific mark
    {
      switch (rehearsal_ev_type_)
        {
        // Silently replace a default rehearsal mark.
        case Event_type::default_rehearsal_mark:
          set_rehearsal_event (Event_type::specific_rehearsal_mark, ev);
          break;

        // Check others.
        default:
          set_rehearsal_event_once (Event_type::specific_rehearsal_mark, ev);
          break;
        }
    }
}

void
Mark_tracking_translator::listen_section_label (Stream_event *ev)
{
  set_performance_event_once (Event_type::section_label, ev);
}

void
Mark_tracking_translator::listen_segno_mark (Stream_event *ev)
{
  SCM label = get_property (ev, "label");
  if (!scm_is_integer (label)) // \segnoMark \default
    {
      // Ignore a default segno at the beginning of a piece.
      if (!first_time_)
        {
          switch (performance_ev_type_)
            {
            // Silently ignore default segno events after we have any segno
            // event.
            case Event_type::default_segno_mark:
            case Event_type::specific_segno_mark:
              break;

            // Check others.
            default:
              set_performance_event_once (Event_type::default_segno_mark, ev);
              break;
            }
        }
    }
  else // a specific segno
    {
      switch (performance_ev_type_)
        {
        // Silently replace a default segno.
        case Event_type::default_segno_mark:
          set_performance_event (Event_type::specific_segno_mark, ev);
          break;

        // Check others.
        default:
          set_performance_event_once (Event_type::specific_segno_mark, ev);
          break;
        }
    }
}

void
Mark_tracking_translator::boot ()
{
  ADD_LISTENER (ad_hoc_mark);
  ADD_LISTENER (coda_mark);
  ADD_LISTENER (rehearsal_mark);
  ADD_LISTENER (section_label);
  ADD_LISTENER (segno_mark);
}

ADD_TRANSLATOR (Mark_tracking_translator,
                /* doc */ R"(

This translator chooses which marks @code{Mark_engraver} should engrave.

)",

                /* create */
                "",

                /* read */
                "codaMarkCount "
                "rehearsalMark "
                "segnoMarkCount ",

                /* write */
                "codaMarkCount "
                "currentPerformanceMarkEvent "
                "currentRehearsalMarkEvent "
                "rehearsalMark "
                "segnoMarkCount ");
