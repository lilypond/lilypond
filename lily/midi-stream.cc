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
#include "midi-item.hh"
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
    error (_f ("can't open for write: %s: %s", file_name, strerror (errno)));
}

Midi_stream::~Midi_stream ()
{
  fclose (out_file_);
}

Midi_stream &
Midi_stream::operator << (string str)
{
  size_t sz = sizeof (Byte);
  size_t n = str.length ();
  size_t written = fwrite (str.data (), sz, n, out_file_);

  if (written != sz * n)
    warning (_ ("can't write to file: `%s'"));

  return *this;
}

Midi_stream &
Midi_stream::operator << (Midi_item const &midi_c_r)
{
  string str = midi_c_r.to_string ();

  // ugh, should have separate debugging output with Midi*::print routines
  if (do_midi_debugging_global)
    {
      str = String_convert::bin2hex (str) + "\n";
      for (ssize i = str.find ("0a"); i != NPOS; i = str.find ("0a"))
	{
	  str[i] = '\n';
	  str[i + 1] = '\t';
	}
    }

  return operator << (str);
}

Midi_stream &
Midi_stream::operator << (int i)
{
  // output binary string ourselves
  *this << Midi_item::i2varint_string (i);
  return *this;
}

