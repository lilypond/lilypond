//
// midi-stream.cc
//
// source file of the GNU LilyPond music typesetter
//
// (c)  1997--1999 Jan Nieuwenhuizen <janneke@gnu.org>

#include <fstream.h>
#include "string.hh"
#include "string-convert.hh"
#include "main.hh"
#include "misc.hh"
#include "midi-item.hh"
#include "midi-stream.hh"
#include "debug.hh"

Midi_stream::Midi_stream (String filename_str)
{
  filename_str_ = filename_str;
  os_p_ = 0;
  open ();
}

Midi_stream::~Midi_stream ()
{
  *os_p_ << flush;		// ugh. Share with tex_stream.
  if (!*os_p_)
    {
      warning (_ ("Error syncing file (disk full?)"));
      exit_status_i_ = 1;
    }
  delete os_p_;
}

Midi_stream&
Midi_stream::operator << (String str)
{
  *os_p_ << str;
  return *this;
}

Midi_stream&
Midi_stream::operator << (Midi_item const& midi_c_r)
{
//    *this <<midi_c_r.str (); 
  String str = midi_c_r.str ();
  if (check_debug && !lily_monitor->silent_b ("Midistrings")) 
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

  *os_p_ << str;
  return *this;
}

Midi_stream&
Midi_stream::operator << (int i)
{
  // output binary string ourselves
  *this << Midi_item::i2varint_str (i);
  return *this;
}

void
Midi_stream::open ()
{
  os_p_ = new ofstream (filename_str_.ch_C (),ios::out|ios::bin);
  if (!*os_p_)
    error (_f ("Can't open file: `%s'", filename_str_));
}
