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

#include "midi-stream.hh"

#include "file-path.hh"
#include "international.hh"
#include "midi-chunk.hh"
#include "misc.hh"
#include "program-option.hh"
#include "string-convert.hh"
#include "warn.hh"

#include <cerrno>
#include <fcntl.h>

using std::string;

Midi_stream::Midi_stream (const string &file_name)
{
  dest_file_name_ = file_name;
  int tries = 10;

  int flags = O_WRONLY | O_CREAT | O_EXCL;
  // Need to open the file in binary mode for Windows to avoid translations, but
  // flag doesn't exist for other systems.
#ifdef O_BINARY
  flags |= O_BINARY;
#endif

  while (tries--)
    {
      tmp_file_name_
        = String_convert::form_string ("%s.%8x", file_name.c_str (), rand ());
      out_file_ = ::open (tmp_file_name_.c_str (), flags, 0666);
      if (out_file_ != -1)
        return;
    }

  error (_f ("cannot create temp file: %s: %s", tmp_file_name_.c_str (),
             strerror (errno)));
}

Midi_stream::~Midi_stream ()
{
  if (close (out_file_))
    {
      string msg (strerror (errno));
      error (_f ("error writing MIDI file: %s", msg.c_str ()));
    }

  if (!rename_file (tmp_file_name_.c_str (), dest_file_name_.c_str ()))
    {
      error (_f ("cannot rename `%s' to `%s'", tmp_file_name_.c_str (),
                 dest_file_name_.c_str ()));
    }
}

void
Midi_stream::write (const string &str)
{
  size_t count = str.length ();
  size_t written = ::write (out_file_, str.data (), count);

  if (written != count)
    error (_f ("cannot write to file: `%s': %s", tmp_file_name_.c_str ()),
           strerror (errno));
}

void
Midi_stream::write (Midi_chunk const &midi)
{
  string str = midi.to_string ();

  return write (str);
}
