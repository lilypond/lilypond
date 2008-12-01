/*
  midi-stream.cc -- implement Midi_stream

  source file of the GNU LilyPond music typesetter

  (c) 1997--2007 Jan Nieuwenhuizen <janneke@gnu.org>
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

