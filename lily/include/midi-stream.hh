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

#ifndef MIDI_STREAM_HH
#define MIDI_STREAM_HH

#include <cstdio>

#include <string.h>
#include "std-string.hh"
#include "lily-proto.hh"

class Midi_stream
{
public:
  Midi_stream (const std::string &file_name_string);
  ~Midi_stream ();

  void write (const std::string &);
  void write (Midi_chunk const &);

private:
  int out_file_;
  std::string tmp_file_name_;
  std::string dest_file_name_;
};

#endif // MIDI_STREAM_HH
