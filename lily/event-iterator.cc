/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "event-iterator.hh"

#include "context.hh"
#include "duration.hh"
#include "input.hh"
#include "international.hh"
#include "music.hh"
#include "warn.hh"

Event_iterator::Event_iterator ()
{
}

void
Event_iterator::create_contexts ()
{
  descend_to_bottom_context ();
  Simple_music_iterator::create_contexts ();
}

void
Event_iterator::process (Moment m)
{
  if (!has_started ())
    report_event (get_music ());

  Simple_music_iterator::process (m);
}

IMPLEMENT_CTOR_CALLBACK (Event_iterator);
