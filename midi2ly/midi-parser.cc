/*
  midi-parser.cc -- implement Midi_parser[_info]

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include <assert.h>
#include "string-convert.hh"
#include "source-file.hh"
#include "midi2ly-global.hh"
#include "midi-parser.hh"

Midi_parser_info::Midi_parser_info ()
{
  division_1_i_ = 0;
  format_i_ = 0;
  tracks_i_ = 0;
  errorlevel_i_ = 0;
  byte_L_ = 0;
  end_byte_L_ = 0;
  score_l_ = 0;
}

Midi_parser::Midi_parser ()
{
  info_l_ = 0;
}

int
Midi_parser::exit (String str)
{
  error (str);
  ::exit (1);
  return 0;
}

void
Midi_parser::error (String str)
{
  ::message (message (str));
}

int
Midi_parser::get_i (int n)
{
  assert (n <= (int)sizeof(int));
  return String_convert::bin2_i (get_str (n));
}

unsigned
Midi_parser::get_u (int n)
{
  assert (n <= (int)sizeof(int));
  return String_convert::bin2_i (get_str (n));
}

String
Midi_parser::get_str (int n)
{
  assert (n >= 0);
  if (!n)
    warning (_ ("zero length string encountered"));

  Byte const* p = forward_byte_L (n);
  return String (p, n);
}

int
Midi_parser::get_var_i ()
{
  int var_i = 0;

  while (1)
    {
      Byte byte = next_byte ();
      var_i <<= 7;
      var_i += byte & 0x7f;
      if (!(byte & 0x80))
	return var_i;
    }
  exit ("get_var_i:");
  return 0;
}

String
Midi_parser::message (String str)
{
  return String ("midi2ly: ")
    + info_l_->source_l_->name_str () + ": "
    + String_convert::i2dec_str (info_l_->source_l_->line_i ((char const*)info_l_->byte_L_), 0, 0) + ": "
    + str + "\n"
    + info_l_->source_l_->error_str ((char const*)info_l_->byte_L_);
}

void
Midi_parser::warning (String str)
{
  ::message (message (String (_ ("warning: ")) + str));
}
