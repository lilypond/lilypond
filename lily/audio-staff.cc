/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2009 Jan Nieuwenhuizen <janneke@gnu.org>

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
#include "midi-stream.hh"
#include "midi-walker.hh"

void
Audio_staff::add_audio_item (Audio_item *l)
{
  audio_items_.push_back (l);
}

Audio_staff::Audio_staff ()
{
  channel_ = -1; 
}

void
Audio_staff::output (Midi_stream &midi_stream, int channel)
{
  Midi_track midi_track;
  midi_track.number_ = channel;

  Midi_walker i (this, &midi_track, channel);
  for (; i.ok (); i++)
    i.process ();

  i.finalize ();
  
  midi_stream.write (midi_track);
}

