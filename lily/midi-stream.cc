/*
  midi-stream.cc -- implement Midi_stream

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Jan Nieuwenhuizen <janneke@gnu.org>
*/


#include "stream.hh"
#include "string.hh"
#include "string-convert.hh"
#include "main.hh"
#include "misc.hh"
#include "midi-item.hh"
#include "midi-stream.hh"
#include "warn.hh"
#include "scm-option.hh"

Midi_stream::Midi_stream (String filename)
{
  filename_string_ = filename;
  out_file_ = fopen (filename.to_str0(), "wb");
}

Midi_stream::~Midi_stream ()
{
  fclose (out_file_);
}

Midi_stream&
Midi_stream::operator << (String str)
{
  Byte * b = str.get_bytes ();
  for (int sz = str.length (); sz--;)
    {
      fputc (*b, out_file_);
      b++;
    }
  return *this;
}

Midi_stream&
Midi_stream::operator << (Midi_item const& midi_c_r)
{
  String str = midi_c_r.string ();


  if (midi_debug_global_b)
    {
     str = String_convert::bin2hex (str) + "\n";
    // ugh, should have separate debugging output with Midi*::print routines
    int i = str.index ("0a");
    while (i >= 0)
      {
        str[i] = '\n';
        str[i + 1] = '\t';
    	i = str.index ("0a");
      }
    }
  else
    {
      Byte * b = str.get_bytes ();
      for (int sz = str.length (); sz--;)
	{
	  fputc (*b, out_file_);
	  b++;
	}
    }
  
  return *this;
}

Midi_stream&
Midi_stream::operator << (int i)
{
  // output binary string ourselves
  *this << Midi_item::i2varint_string (i);
  return *this;
}

