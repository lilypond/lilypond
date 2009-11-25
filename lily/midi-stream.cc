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

#include "midi-stream.hh"

#include <cerrno>
using namespace std;

#include "international.hh"
#include "main.hh"
#include "midi-chunk.hh"
#include "misc.hh"
#include "program-option.hh"
#include "stream.hh"
#include "string-convert.hh"
#include "warn.hh"

Midi_stream::Midi_stream (string file_name)
{
  file_name_string_ = file_name;
  out_file_ = fopen (file_name.c_str (), "wb");
  if (!out_file_)
    error (_f ("cannot open for write: %s: %s", file_name, strerror (errno)));
}

Midi_stream::~Midi_stream ()
{
  fclose (out_file_);
}

void
Midi_stream::write (string str)
{
  size_t sz = sizeof (Byte);
  size_t n = str.length ();
  size_t written = fwrite (str.data (), sz, n, out_file_);

  if (written != sz * n)
    warning (_f ("cannot write to file: `%s'", str.data ()));
}

void
Midi_stream::write (Midi_chunk const &midi)
{
  string str = midi.to_string ();

  return write (str);
}

