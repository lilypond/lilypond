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
#define istringstream(x) istrstream(x, length ()) 
#endif

#include "string.hh"
#include "flower-proto.hh"
#include "warn.hh"
#include "source-file.hh"
#include "simple-file-storage.hh"
#include "string-storage.hh"

Source_file::Source_file (String filename_string)
{
  name_string_ = filename_string;
  istream_ = 0;
  storage_ = new Simple_file_storage (filename_string);
  pos_str0_ = to_str0 ();
}

Source_file::Source_file (String name_string, String data_string)
{
  name_string_ = name_string;
  istream_ = 0;
  storage_ = new String_storage (data_string);
  pos_str0_ = to_str0 ();
}

std::istream*
Source_file::get_istream ()
{
  /*
    if (!name_string_.length ())
      return &cin;
    */

  if (!istream_)
    {
      if (length ()) // can-t this be done without such a hack?
	istream_ = new std::istringstream (to_str0 ());
      else
	{
	  istream_ = new std::istringstream ("");
	  istream_->setstate (std::ios::eofbit);
	  //	  istream_->set (ios::eofbit);
	}
    }
  return istream_;
}

String
Source_file::file_line_column_string (char const *context_str0) const
{
  if (!to_str0 ())
    return " (" + _ ("position unknown") + ")";
  else
    return name_string () + ":" + to_string (get_line (context_str0))
      + ":" + to_string (get_char (context_str0));
}

String
Source_file::name_string () const
{
  return name_string_;
}

Source_file::~Source_file ()
{
  delete istream_;
  istream_ = 0;
  delete storage_;
}

Slice
Source_file::line_slice (char const* pos_str0) const
{
  if (!in_b (pos_str0))
    return Slice (0,0);

  char const* data_str0 = to_str0 ();
  char const * eof_C_ = data_str0 + length ();

  if (pos_str0 == eof_C_)
    pos_str0 --;
  char const* begin_str0 = pos_str0;
  while (begin_str0 > data_str0)
    if (*--begin_str0 == '\n')
      {
	begin_str0++;
	break;
      }

  char const* end_str0 = pos_str0;
  while (end_str0 < eof_C_)
    if (*end_str0++ == '\n')
      {
	end_str0--;
	break;
      }

  return Slice (begin_str0 - data_str0, end_str0 - data_str0);
}

String
Source_file::line_string (char const* pos_str0) const
{
  if (!in_b (pos_str0))
    return "";

  Slice line = line_slice (pos_str0);
  char const* data_str0 = to_str0 ();
  return String ((Byte const*)data_str0 + line[LEFT], line.length ());
}

int
Source_file::get_char (char const* pos_str0) const
{
  if (!in_b (pos_str0))
    return 0;

  char const* data_str0 = to_str0 ();
  return pos_str0 - (line_slice (pos_str0)[SMALLER] + data_str0);
}

int
Source_file::get_column (char const* pos_str0) const
{
  if (!in_b (pos_str0))
    return 0;

  int ch_i = get_char (pos_str0);
  String line = line_string (pos_str0);

  int col_i = 0;
  for (int i = 0; i < ch_i; i++)
    if (line[i] == '\t')
      col_i = (col_i / 8 + 1) * 8;
    else
      col_i++;

  return col_i;
}

String
Source_file::error_string (char const* pos_str0) const
{
  if (!in_b (pos_str0))
    return " (" + _ ("position unknown") + ")";

  int ch_i = get_char (pos_str0);
  String line = line_string (pos_str0);
  String context = line.left_string (ch_i)
    + to_string ('\n')
    + to_string (' ', get_column (pos_str0))
    + line.cut_string (ch_i, INT_MAX);

  return context;
}

bool
Source_file::in_b (char const* pos_str0) const
{
  return (pos_str0 && (pos_str0 >= to_str0 ()) && (pos_str0 <= to_str0 () + length ()));
}

int
Source_file::get_line (char const* pos_str0) const
{
  if (!in_b (pos_str0))
    return 0;

  int i = 1;
  char const* scan_str0 = to_str0 ();
  if (!scan_str0)
    return 0;

  while (scan_str0 < pos_str0)
    if (*scan_str0++ == '\n')
      i++;
  return i;
}

int
Source_file::length () const
{
  return storage_->length ();
}

char const *
Source_file::to_str0 () const
{
  return storage_->to_str0 ();
}

void
Source_file::set_pos (char const * pos_str0)
{
  if (in_b (pos_str0))
    pos_str0_ = pos_str0;
  else
    error (error_string (pos_str0) + "invalid pos");
}

char const*
Source_file::seek_str0 (int n)
{
  char const* new_str0 = to_str0 () + n;
  if (n < 0)
    new_str0 += length ();
  if (in_b (new_str0))
    pos_str0_ = new_str0;
  else
    error (error_string (new_str0) + "seek past eof");

  return pos_str0_;
}

char const*
Source_file::forward_str0 (int n)
{
  char const* old_pos = pos_str0_;
  char const* new_str0 = pos_str0_ + n;
  if (in_b (new_str0))
    pos_str0_ = new_str0;
  else
    error (error_string (new_str0)  + "forward past eof");

  return old_pos;
}

String
Source_file::get_string (int n)
{
  String str = String ((Byte const*)forward_str0 (n), n);
  return str;
}
