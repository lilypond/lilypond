/*
  midi-stream.cc -- implement Midi_stream

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "midi-stream.hh"

#include "stream.hh"
#include "string-convert.hh"
#include "main.hh"
#include "misc.hh"
#include "midi-item.hh"
#include "warn.hh"
#include "scm-option.hh"

Midi_stream::Midi_stream (String file_name)
{
  file_name_string_ = file_name;
  out_file_ = fopen (file_name.to_str0(), "wb");
}

Midi_stream::~Midi_stream ()
{
  fclose (out_file_);
}

Midi_stream&
Midi_stream::operator << (String str)
{
  size_t sz = sizeof (Byte);
  size_t n = str.length ();
  size_t written = fwrite (str.get_bytes (),
			   sz, n, out_file_);

  if (written != sz * n)
    warning (_ ("could not write file: `%s'"));

  return *this;
}

Midi_stream&
Midi_stream::operator << (Midi_item const& midi_c_r)
{
  String str = midi_c_r.to_string ();

  // ugh, should have separate debugging output with Midi*::print routines
  if (midi_debug_global_b)
    {
      str = String_convert::bin2hex (str) + "\n";
      for (int i = str.index ("0a"); i >= 0; i = str.index ("0a"))
	{
	  str[i] = '\n';
	  str[i + 1] = '\t';
	}
    }

  return operator << (str);
}

Midi_stream&
Midi_stream::operator << (int i)
{
  // output binary string ourselves
  *this << Midi_item::i2varint_string (i);
  return *this;
}

