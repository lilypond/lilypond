/*
  source-file.cc -- implement Source_file

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Jan Nieuwenhuizen <jan@digicash.com>
  & Han-Wen Nienhuys <hanwen@stack.nl>
*/


#include <assert.h>
#include <strstream.h>

#include "string.hh"
#include "proto.hh"
#include "plist.hh"
#include "warn.hh"
#include "windhoos-suck-suck-suck-thank-you-cygnus.hh"
#include "source-file.hh"
#include "simple-file-storage.hh"

Source_file::Source_file (String filename_str)
{
  name_str_ = filename_str;
  istream_p_ = 0;
  storage_p_ = new Simple_file_storage (filename_str);
}

istream*
Source_file::istream_l ()
{
  /*
    if (!name_str_.length_i ())
      return &cin;
    */

  if (!istream_p_)
    {
      if (length_i ()) // can-t this be done without such a hack?
	istream_p_ = new istrstream (ch_C (), length_i ());
      else
	{
	  istream_p_ = new istrstream ("", 0);
	  istream_p_->set (ios::eofbit);
	}
    }
  return istream_p_;
}

String
Source_file::file_line_no_str (char const *context_ch_C) const
{
  if  (!ch_C ())
    return _ ("(unknown)");
  else
    return name_str () + ": "
      + String (line_i (context_ch_C));
}

String
Source_file::name_str () const
{
  return name_str_;
}

Source_file::~Source_file ()
{
  delete istream_p_;
  istream_p_ = 0;
  delete storage_p_;
}

String
Source_file::error_str (char const* pos_ch_C) const
{
  char const* data_ch_C = ch_C ();
  char const * eof_C_ = data_ch_C + length_i ();
  if (!in_b (pos_ch_C))
    return _ ("(position unknown)");


  if (pos_ch_C == eof_C_)
    pos_ch_C --;
  char const* begin_ch_C = pos_ch_C;
  while (begin_ch_C > data_ch_C)
    if (*--begin_ch_C == '\n')
      {
	begin_ch_C++;
	break;
      }

  char const* end_ch_C = pos_ch_C;
  while (end_ch_C < eof_C_)
    if (*end_ch_C++ == '\n')
      {
	end_ch_C--;
	break;
      }

  //    String (char const* p, int length) is missing!?
  String line_str ((Byte const*)begin_ch_C, end_ch_C - begin_ch_C);

  int error_col_i = 0;
  char const* scan_ch_C = begin_ch_C;
  while (scan_ch_C < pos_ch_C)
    if (*scan_ch_C++ == '\t')
      error_col_i = (error_col_i / 8 + 1) * 8;
    else
      error_col_i++;

  String str = line_str.left_str (pos_ch_C - begin_ch_C)
    + String ('\n')
    + String (' ', error_col_i)
    + line_str.cut (pos_ch_C - begin_ch_C, INT_MAX); // String::mid should take 0 arg..
  return str;
}

bool
Source_file::in_b (char const* pos_ch_C) const
{
  return (pos_ch_C && (pos_ch_C >= ch_C ()) && (pos_ch_C <= ch_C () + length_i ()));
}


int
Source_file::line_i (char const* pos_ch_C) const
{
  if (!in_b (pos_ch_C))
    return 0;

  int i = 1;
  char const* scan_ch_C = ch_C ();
  if (!scan_ch_C)
    return 0;

  while (scan_ch_C < pos_ch_C)
    if (*scan_ch_C++ == '\n')
      i++;
  return i;
}

int
Source_file::length_i () const
{
  return storage_p_->length_i ();
}

char const *
Source_file::ch_C () const
{
  return storage_p_->ch_C ();
}
