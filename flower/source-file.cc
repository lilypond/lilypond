/*
  source-file.cc -- implement Source_file

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Jan Nieuwenhuizen <janneke@gnu.org>
  & Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include <assert.h>
#if HAVE_SSTREAM
#include <sstream>
#else
#include <strstream.h>
#define istringstream(x) istrstream(x, length_i ()) 
#endif

#include "string.hh"
#include "flower-proto.hh"
#include "warn.hh"
#include "source-file.hh"
#include "simple-file-storage.hh"
#include "string-storage.hh"

Source_file::Source_file (String filename_str)
{
  name_str_ = filename_str;
  istream_p_ = 0;
  storage_p_ = new Simple_file_storage (filename_str);
  pos_ch_C_ = ch_C ();
}

Source_file::Source_file (String name_str, String data_str)
{
  name_str_ = name_str;
  istream_p_ = 0;
  storage_p_ = new String_storage (data_str);
  pos_ch_C_ = ch_C ();
}

std::istream*
Source_file::istream_l ()
{
  /*
    if (!name_str_.length_i ())
      return &cin;
    */

  if (!istream_p_)
    {
      if (length_i ()) // can-t this be done without such a hack?
	istream_p_ = new std::istringstream (ch_C ());
      else
	{
	  istream_p_ = new std::istringstream ("");
	  istream_p_->setstate (std::ios::eofbit);
	  //	  istream_p_->set (ios::eofbit);
	}
    }
  return istream_p_;
}

String
Source_file::file_line_column_str (char const *context_ch_C) const
{
  if (!ch_C ())
    return " (" + _ ("position unknown") + ")";
  else
    return name_str () + ":" + to_str (line_i (context_ch_C))
      + ":" + to_str (char_i (context_ch_C));
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

Slice
Source_file::line_slice (char const* pos_ch_C) const
{
  if (!in_b (pos_ch_C))
    return Slice (0,0);

  char const* data_ch_C = ch_C ();
  char const * eof_C_ = data_ch_C + length_i ();

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

  return Slice (begin_ch_C - data_ch_C, end_ch_C - data_ch_C);
}

String
Source_file::line_str (char const* pos_ch_C) const
{
  if (!in_b (pos_ch_C))
    return "";

  Slice line = line_slice (pos_ch_C);
  char const* data_ch_C = ch_C ();
  return String ((Byte const*)data_ch_C + line[LEFT], line.length ());
}

int
Source_file::char_i (char const* pos_ch_C) const
{
  if (!in_b (pos_ch_C))
    return 0;

  char const* data_ch_C = ch_C ();
  return pos_ch_C - (line_slice (pos_ch_C)[SMALLER] + data_ch_C);
}

int
Source_file::column_i (char const* pos_ch_C) const
{
  if (!in_b (pos_ch_C))
    return 0;

  int ch_i = char_i (pos_ch_C);
  String line = line_str (pos_ch_C);

  int col_i = 0;
  for (int i = 0; i < ch_i; i++)
    if (line[i] == '\t')
      col_i = (col_i / 8 + 1) * 8;
    else
      col_i++;

  return col_i;
}

String
Source_file::error_str (char const* pos_ch_C) const
{
  if (!in_b (pos_ch_C))
    return " (" + _ ("position unknown") + ")";

  int ch_i = char_i (pos_ch_C);
  String line = line_str (pos_ch_C);
  String context = line.left_str (ch_i)
    + to_str ('\n')
    + to_str (' ', column_i (pos_ch_C))
    + line.cut_str (ch_i, INT_MAX);

  return context;
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

void
Source_file::set_pos (char const * pos_ch_C)
{
  if (in_b (pos_ch_C))
    pos_ch_C_ = pos_ch_C;
  else
    error (error_str (pos_ch_C) + "invalid pos");
}

char const*
Source_file::seek_ch_C (int n)
{
  char const* new_ch_C = ch_C () + n;
  if (n < 0)
    new_ch_C += length_i ();
  if (in_b (new_ch_C))
    pos_ch_C_ = new_ch_C;
  else
    error (error_str (new_ch_C) + "seek past eof");

  return pos_ch_C_;
}

char const*
Source_file::forward_ch_C (int n)
{
  char const* old_pos_C = pos_ch_C_;
  char const* new_ch_C = pos_ch_C_ + n;
  if (in_b (new_ch_C))
    pos_ch_C_ = new_ch_C;
  else
    error (error_str (new_ch_C)  + "forward past eof");

  return old_pos_C;
}

String
Source_file::get_str (int n)
{
  String str ((Byte const*)forward_ch_C (n), n);
  return str;
}
