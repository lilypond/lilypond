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
#include "debug.hh"
#include "scm-option.hh"

Midi_stream::Midi_stream (String filename)
{
  filename_str_ = filename;
#if __GCC__ > 2
  os_p_ = open_file_stream (filename, ios::out|ios::bin);
#else
  os_p_ = open_file_stream (filename, ios::out|ios::binary);
#endif
}

Midi_stream::~Midi_stream ()
{
  close_file_stream (os_p_);
}

Midi_stream&
Midi_stream::operator << (String str)
{
  Byte * b = str.byte_l();
  for (int sz = str.length_i (); sz--;)
    *os_p_ << *b;
  return *this;
}

Midi_stream&
Midi_stream::operator << (Midi_item const& midi_c_r)
{
  String str = midi_c_r.str ();


  if (midi_debug_global_b)
    {
     str = String_convert::bin2hex_str (str) + "\n";
    // ugh, should have separate debugging output with Midi*::print routines
    int i = str.index_i ("0a");
    while (i >= 0)
      {
        str[i] = '\n';
        str[i + 1] = '\t';
    	i = str.index_i ("0a");
      }
    }

  Byte * b = str.byte_l();
  for (int sz = str.length_i (); sz--;)
    *os_p_ << *b;
  
  return *this;
}

Midi_stream&
Midi_stream::operator << (int i)
{
  // output binary string ourselves
  *this << Midi_item::i2varint_str (i);
  return *this;
}

