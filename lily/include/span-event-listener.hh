/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2022 Daniel Eble <nine.fierce.ballads@gmail.com>

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

#ifndef SPAN_EVENT_LISTENER_HH
#define SPAN_EVENT_LISTENER_HH

#include "drul-array.hh"
#include "stream-event.hh"

// Abstract base of span-event listeners.  These listeners record a pair of
// events based on the 'span-direction property: one for START and one for
// STOP.
class Span_event_listener
{
public:
  virtual ~Span_event_listener () = default;

  virtual void listen (Stream_event *) = 0;

  // Forget everything.
  void reset () { events_ = {}; }

  // Read the START event.
  const Stream_event *get_start () const { return events_.front (); }

  // Read the STOP event.
  const Stream_event *get_stop () const { return events_.back (); }

  // return either event, preferring the stop event
  const Stream_event *get_stop_or_start () const
  {
    if (auto *ev = get_stop ())
      return ev;
    else
      return get_start ();
  }

protected:
  template <bool once>
  void internal_listen (Stream_event *ev)
  {
    if (auto d = from_scm<Direction> (get_property (ev, "span-direction")))
      {
        if (once)
          assign_event_once (events_[d], ev);
        else
          events_[d] = ev;
      }
    else
      {
        ev->programming_error ("event span-direction is not set");
      }
  }

private:
  Drul_array<Stream_event *> events_;
};

// Record the first START event and the first STOP event.  Use
// assign_event_once () to issue a warning if incompatible events are received.
class Unique_span_event_listener final : public Span_event_listener
{
public:
  void listen (Stream_event *ev) override
  {
    constexpr bool once = true;
    internal_listen<once> (ev);
  }
};

// Record the most recent START and STOP events.
class Last_span_event_listener final : public Span_event_listener
{
public:
  void listen (Stream_event *ev) override
  {
    constexpr bool once = false;
    internal_listen<once> (ev);
  }
};

#endif // SPAN_EVENT_LISTENER_HH
