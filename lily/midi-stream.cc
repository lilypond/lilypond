/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2020 Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "international.hh"
#include "main.hh"
#include "midi-chunk.hh"
#include "misc.hh"
#include "program-option.hh"
#include "string-convert.hh"
#include "warn.hh"

#include <cerrno>

using std::string;

Midi_stream::Midi_stream (const string &file_name)
{
  tmp_file_name_ = String_convert::form_string ("%s.%06d", file_name.c_str (),
                                                rand () % 1000000);
  dest_file_name_ = file_name;
  out_file_ = fopen (tmp_file_name_.c_str (), "wbx");
  if (!out_file_)
    error (_f ("cannot open for write: %s: %s", tmp_file_name_.c_str (),
               strerror (errno)));
}

Midi_stream::~Midi_stream ()
{
  fclose (out_file_);

#ifdef __MINGW32__
  // Windows rename will not overwrite existing destinations.
  unlink (dest_file_name_.c_str ());
#endif
  if (rename (tmp_file_name_.c_str (), dest_file_name_.c_str ()))
    {
      error (_f ("cannot rename `%s' to `%s'", tmp_file_name_.c_str (),
                 dest_file_name_.c_str ()));
    }
}

void
Midi_stream::write (const string &str)
{
  size_t sz = sizeof (Byte);
  size_t n = str.length ();
  size_t written = fwrite (str.data (), sz, n, out_file_);

  if (written != sz * n)
    warning (_f ("cannot write to file: `%s': %s", tmp_file_name_.c_str ()),
             strerror (errno));
}

void
Midi_stream::write (Midi_chunk const &midi)
{
  string str = midi.to_string ();

  return write (str);
}
