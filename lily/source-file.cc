/*
  source-file.cc -- implement Source_file

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Jan Nieuwenhuizen <janneke@gnu.org>
  & Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <stdio.h>
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
#include "array.hh"

void
Source_file::load_stdin ()
{
  length_ = 0;

  int c;
  Array<char> chs;		// ugh.
  while ((c = fgetc (stdin)) != EOF)
    chs.push (c);

  length_ = chs.size ();
  contents_str0_ = chs.remove_array ();
}



char *
gulp_file (String fn, int* len)
{
  /*
    let's hope that "b" opens anything binary, and does not apply
    CR/LF translation
    */
  FILE * f =  fopen (fn.to_str0 (), "rb");

  if (!f)
    {
      warning (_f ("can't open file: `%s'", fn.to_str0 ()));
      return 0;
    }

  int ret = fseek (f, 0, SEEK_END);

  *len = ftell (f);
  rewind (f);
  char *  str = new char[*len+1];
  str[*len] = 0;
  ret = fread (str, sizeof (char), *len, f);

  if (ret!=*len)
    warning (_f ("Huh?  Got %d, expected %d characters", ret, *len));

  fclose (f);


  return str;
}



Source_file::Source_file (String filename, String data)
{
  name_string_ = "";
  istream_ = 0;
  contents_str0_ = data.get_copy_str0();
  length_ = data.length();
  pos_str0_ = to_str0 ();
  init_port();
}




Source_file::Source_file (String filename_string)
{
  name_string_ = filename_string;
  istream_ = 0;
  contents_str0_ = 0;

  if (filename_string == "-")
    load_stdin ();
  else
    contents_str0_ = gulp_file (filename_string, &length_);
  
  pos_str0_ = to_str0 ();

  init_port();

  for (int i = 0; i < length_; i++)
    if (contents_str0_[i] == '\n')
      newline_locations_.push (contents_str0_ + i);
}

void
Source_file::init_port ()
{
  SCM str =scm_makfrom0str (contents_str0_);
  
  str_port_ = scm_mkstrport (SCM_INUM0, str, SCM_OPN | SCM_RDNG,
			     __FUNCTION__);
  scm_set_port_filename_x (str_port_,
			   scm_makfrom0str (name_string_.get_str0()));
}

int
Source_file::tell () const
{
  return pos_str0_  - contents_str0_; 
}

std::istream*
Source_file::get_istream ()
{
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
  delete[] contents_str0_;
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

  int lo=0;
  int hi = newline_locations_.size();
  
  binary_search_bounds (newline_locations_,
			pos_str0, 
			Link_array<char>::default_compare,
			&lo, &hi);
  
  return lo;
}

int
Source_file::length () const
{
  return length_;
}

char const *
Source_file::to_str0 () const
{
  return contents_str0_;
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
