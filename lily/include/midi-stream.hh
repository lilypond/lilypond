/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2010 Jan Nieuwenhuizen <janneke@gnu.org>

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
using namespace std;

#include <string.h>
#include "std-string.hh"
#include "lily-proto.hh"

struct Midi_stream
{
  Midi_stream (string file_name_string);
  ~Midi_stream ();

  void write (string);
  void write (Midi_chunk const &);
  void open ();

  FILE *out_file_;
  string file_name_string_;
};

#endif // MIDI_STREAM_HH
