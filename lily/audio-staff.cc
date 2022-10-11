/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "audio-staff.hh"

#include "midi-chunk.hh"
#include "midi-item.hh"
#include "midi-stream.hh"
#include "midi-walker.hh"

void
Audio_staff::add_audio_item (Audio_item *ai)
{
  audio_items_.push_back (ai);
}

Audio_staff::Audio_staff ()
  : percussion_ (false),
    merge_unisons_ (false)
{
}

void
Audio_staff::output (Midi_stream &midi_stream, int track, bool port,
                     Moment start_mom)
{
  Midi_track midi_track (track, port);

  Midi_walker i (this, &midi_track, moment_to_ticks (start_mom));
  for (; i.ok (); i++)
    i.process ();

  i.finalize (moment_to_ticks (end_mom_));

  midi_stream.write (midi_track);
}
