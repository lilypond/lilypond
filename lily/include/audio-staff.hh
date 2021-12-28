/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2022 Jan Nieuwenhuizen <janneke@gnu.org>

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

#ifndef AUDIO_STAFF_HH
#define AUDIO_STAFF_HH

#include "lily-proto.hh"
#include "moment.hh"
#include "audio-element.hh"

#include <vector>

class Audio_staff : public Audio_element
{
public:
  void add_audio_item (Audio_item *ai);
  void output (Midi_stream &midi_stream_r, int track, bool port,
               Moment start_mom);

  Audio_staff ();

  Moment end_mom_;
  bool percussion_;
  bool merge_unisons_;
  std::vector<Audio_item *> audio_items_;
};

// Subtype to identify a staff that represents the "control track" of a MIDI
// sequence (created by Control_track_performer).
struct Audio_control_track_staff : public Audio_staff
{
};

#endif // AUDIO_STAFF_HH
