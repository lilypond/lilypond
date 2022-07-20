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

#ifndef SIMPLE_EVENT_LISTENER_HH
#define SIMPLE_EVENT_LISTENER_HH

class Stream_event;

// Record that the event has been heard.
class Boolean_event_listener final
{
public:
  void listen (Stream_event *) { heard_ = true; }

  bool heard () const { return heard_; }
  void reset () { heard_ = false; }
  void set_heard (bool b = true) { heard_ = b; }

private:
  bool heard_ = false;
};

#endif // SIMPLE_EVENT_LISTENER_HH
