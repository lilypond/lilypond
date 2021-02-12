/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2018--2021 Daniel Eble <nine.fierce.ballads@gmail.com>

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
Mark_tracking_translator::clear_event ()
{
  if (event_type_ != Event_type::none)
    {
      event_ = nullptr;
      event_type_ = Event_type::none;
      context ()->unset_property (ly_symbol2scm ("currentMarkEvent"));
    }
}

void
Mark_tracking_translator::set_event (Event_type type, Stream_event *ev)
{
  event_ = ev;
  event_type_ = type;
  set_property (context (), "currentMarkEvent", ev->self_scm ());
}

bool
Mark_tracking_translator::set_event_once (Event_type type, Stream_event *ev)
{
  if (!ASSIGN_EVENT_ONCE (event_, ev))
    return false;
  set_event (type, ev);
  return true;
}

void
Mark_tracking_translator::stop_translation_timestep ()
{
  switch (event_type_)
    {
    case Event_type::default_rehearsal_mark:
    case Event_type::specific_rehearsal_mark:
      {
        // A sequential mark determines the sequence number for next time.
        const auto label = get_rehearsal_mark_label (context (), event_);
        if (label > 0)
          set_property (context (), "rehearsalMark", to_scm (label + 1));
        break;
      }

    default:
      break;
    }

  clear_event ();
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

void
Mark_tracking_translator::listen_ad_hoc_mark (Stream_event *ev)
{
  // Ad-hoc marks are not rehearsal marks (though they lead to the creation of
  // RehearsalMark grobs for backward compatibility), so this conflict check is
  // simple: complain about everything to incentivize using something else.
  set_event_once (Event_type::ad_hoc_mark, ev);
}

void
Mark_tracking_translator::listen_rehearsal_mark (Stream_event *ev)
{
  SCM label = get_property (ev, "label");
  if (!scm_is_integer (label)) // \mark \default
    {
      // Silently ignore default rehearsal mark events after we have any
      // rehearsal mark.
      switch (event_type_)
        {
        case Event_type::default_rehearsal_mark:
        case Event_type::specific_rehearsal_mark:
          break;

        default:
          set_event_once (Event_type::default_rehearsal_mark, ev);
          break;
        }
    }
  else // a specific mark
    {
      switch (event_type_)
        {
        // Silently replace a default rehearsal mark.
        case Event_type::default_rehearsal_mark:
          set_event (Event_type::specific_rehearsal_mark, ev);
          break;

        // Check others.
        default:
          set_event_once (Event_type::specific_rehearsal_mark, ev);
          break;
        }
    }
}

void
Mark_tracking_translator::boot ()
{
  ADD_LISTENER (Mark_tracking_translator, ad_hoc_mark);
  ADD_LISTENER (Mark_tracking_translator, rehearsal_mark);
}

ADD_TRANSLATOR (Mark_tracking_translator,
                /* doc */ R"(

This translator chooses which mark @code{Mark_engraver} should engrave.

)",

                /* create */
                "",

                /* read */
                "rehearsalMark ",

                /* write */
                "currentMarkEvent "
                "rehearsalMark "
);
