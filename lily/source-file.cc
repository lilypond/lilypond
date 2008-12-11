/*
  source-file.cc -- implement Source_file

  source file of the GNU LilyPond music typesetter

  (c) 1997--2008 Jan Nieuwenhuizen <janneke@gnu.org>
  Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#if GCC_MAJOR < 4
#define _GLIBCXX_HAVE_MBSTATE_T
#include <wchar.h>
#endif /* GCC_MAJOR < 4 */

#include "source-file.hh"

#include "config.hh"

#if HAVE_UTF8_WCHAR_H
#include <utf8/wchar.h>  /* mbrtowc */
#else /* !HAVE_UTF8_WCHAR_H */
#include <cwchar> /* mbrtowc */
#endif /* HAVE_UTF8_WCHAR_H */

#include <cstdio>

#if HAVE_SSTREAM
#include <sstream>
#else
#include <strstream>
#define istringstream(x) istrstream (x, length ())
#endif
using namespace std;

#include "file-name-map.hh"
#include "international.hh"
#include "warn.hh"

void
Source_file::load_stdin ()
{
  characters_.clear ();
  int c;
  while ((c = fgetc (stdin)) != EOF)
    characters_.push_back (c);
}

/*
  return contents of FILENAME. *Not 0-terminated!* 
 */
vector<char>
gulp_file (string filename, int desired_size)
{
  /* "b" must ensure to open literally, avoiding text (CR/LF)
     conversions.  */
  FILE *f = fopen (filename.c_str (), "rb");
  if (!f)
    {
      warning (_f ("cannot open file: `%s'", filename.c_str ()));

      vector<char> cxx_arr;
      return cxx_arr;
    }

  fseek (f, 0, SEEK_END);
  int real_size = ftell (f);
  int read_count = real_size;

  if (desired_size > 0)
    read_count = min (read_count, desired_size);
  
  rewind (f);

  char *str = new char[read_count + 1];
  str[read_count] = 0;

  int bytes_read = fread (str, sizeof (char), read_count, f);
  if (bytes_read != read_count)
    warning (_f ("expected to read %d characters, got %d", bytes_read,
		 read_count));
  fclose (f);
  int filesize = bytes_read;

  vector<char> cxx_arr;
  cxx_arr.resize (filesize);

  copy (str, str + filesize, cxx_arr.begin ());
  
  delete[] str;
  return cxx_arr;
}

void
Source_file::init ()
{
  istream_ = 0;
  line_offset_ = 0;
  str_port_ = SCM_EOL;
  self_scm_ = SCM_EOL;
  smobify_self ();
}

Source_file::Source_file (string filename, string data)
{
  init ();
  
  name_ = filename;

  characters_.resize (data.length ());
  copy (data.begin (), data.end (), characters_.begin ());

  characters_.push_back (0);
  
  init_port ();

  for (vsize i = 0; i < characters_.size (); i++)
    if (characters_[i] == '\n')
      newline_locations_.push_back (&characters_[0] + i);
}

Source_file::Source_file (string filename_string)
{
  init ();
  
  name_ = filename_string;

  if (filename_string == "-")
    load_stdin ();
  else
    {
      characters_ = gulp_file (filename_string, -1);
    }

  characters_.push_back (0);

  init_port ();

  for (vsize i = 0; i < characters_.size (); i++)
    if (characters_[i] == '\n')
      newline_locations_.push_back (&characters_[0] + i);
}

void
Source_file::init_port ()
{
  SCM str = scm_from_locale_string (c_str ());
  str_port_ = scm_mkstrport (SCM_INUM0, str, SCM_OPN | SCM_RDNG, __FUNCTION__);
  scm_set_port_filename_x (str_port_, ly_string2scm (name_));
}


istream *
Source_file::get_istream ()
{
  if (!istream_)
    {
      if (length ()) // can-t this be done without such a hack?
	istream_ = new istringstream (c_str ());
      else
	{
	  istream_ = new istringstream ("");
	  istream_->setstate (ios::eofbit);
	  //	  istream_->set (ios::eofbit);
	}
    }
  return istream_;
}

string
Source_file::file_line_column_string (char const *context_str0) const
{
  if (!c_str ())
    return " (" + _ ("position unknown") + ")";
  else
    {
      int l, ch, col;
      get_counts (context_str0, &l, &ch, &col);

      return name_string () + ":" + to_string (l)
	+ ":" + to_string (col);
    }
}

string
Source_file::quote_input (char const *pos_str0) const
{
  if (!contains (pos_str0))
    return " (" + _ ("position unknown") + ")";

  int l, ch, col;
  get_counts (pos_str0, &l, &ch, &col);
  string line = line_string (pos_str0);
  string context = line.substr (0, ch)
    + to_string ('\n')
    + to_string (' ', col)
    + line.substr (ch, line.length ()-ch);
  return context;
}

string
Source_file::name_string () const
{
  return map_file_name (name_);
}

Source_file::~Source_file ()
{
  delete istream_;
}

Slice
Source_file::line_slice (char const *pos_str0) const
{
  if (!contains (pos_str0))
    return Slice (0, 0);

  char const *data_str0 = c_str ();
  char const *eof_C_ = data_str0 + length ();

  if (pos_str0 == eof_C_)
    pos_str0--;
  char const *begin_str0 = pos_str0;
  while (begin_str0 > data_str0)
    if (*--begin_str0 == '\n')
      {
	begin_str0++;
	break;
      }

  char const *end_str0 = pos_str0;
  while (end_str0 < eof_C_)
    if (*end_str0++ == '\n')
      {
	end_str0--;
	break;
      }

  return Slice (begin_str0 - data_str0, end_str0 - data_str0);
}

string
Source_file::line_string (char const *pos_str0) const
{
  if (!contains (pos_str0))
    return "";

  Slice line = line_slice (pos_str0);
  char const *data_str0 = c_str ();
  return string (data_str0 + line[LEFT], line.length ());
}

void
Source_file::get_counts (char const *pos_str0,
			 int *line_number,
			 int *line_char,
			 int *column) const
{
  *line_number = 0;
  *line_char = 0;
  *column = 0;
    
  if (!contains (pos_str0))
    return;

  *line_number = get_line (pos_str0);

  Slice line = line_slice (pos_str0);
  char const *data = c_str ();
  char const *line_start = (char const *)data + line[LEFT];

  ssize left = (char const *) pos_str0 - line_start;
  string line_begin (line_start, left);
  char const *line_chars = line_begin.c_str ();

  *column = 0;
  *line_char = 0;

  mbstate_t state;

  /* Initialize the state.  */
  memset (&state, '\0', sizeof (state));

  while (left > 0)
    {
      /*
	FIXME, this is apparently locale dependent.
      */
#if HAVE_MBRTOWC
      wchar_t multibyte[2];
      size_t thislen = mbrtowc (multibyte, line_chars, left, &state);
#else
      size_t thislen = 1;
#endif /* !HAVE_MBRTOWC */

      /* Stop converting at invalid character;
	 this can mean we have read just the first part
	 of a valid character.  */
      if (thislen == (size_t) -1)
	break;

      /* We want to handle embedded NUL bytes
	 but the return value is 0.  Correct this.  */
      if (thislen == 0)
	thislen = 1;

      if (thislen == 1 && line_chars[0] == '\t')
	(*column) = (*column / 8 + 1) * 8;
      else
	(*column)++;

      (*line_char)++;
      /* Advance past this character. */
      line_chars += thislen;
      left -= thislen;
    }
}

bool
Source_file::contains (char const *pos_str0) const
{
  return (pos_str0 && (pos_str0 >= c_str ()) && (pos_str0 <= c_str () + length ()));
}

int
Source_file::get_line (char const *pos_str0) const
{
  if (!contains (pos_str0))
    return 0;

  if (!newline_locations_.size ())
    return 1;

  /* this will find the '\n' character at the end of our line */
  vsize lo = lower_bound (newline_locations_,
			  pos_str0,
			  less<char const*> ());

  /* the return value will be indexed from 1 */
  return lo + 1 + line_offset_;
}

void
Source_file::set_line (char const *pos_str0, int line)
{
  int current_line = get_line (pos_str0);
  line_offset_ += line - current_line;

  assert (line == get_line (pos_str0));
}

int
Source_file::length () const
{
  return characters_.size ();
}

char const *
Source_file::c_str () const
{
  return &characters_[0];
}

SCM
Source_file::get_port () const
{
  return str_port_;
}

/****************************************************************/

#include "ly-smobs.icc"

IMPLEMENT_SMOBS (Source_file);
IMPLEMENT_DEFAULT_EQUAL_P (Source_file);
IMPLEMENT_TYPE_P (Source_file, "ly:source-file?");

SCM
Source_file::mark_smob (SCM smob)
{
  Source_file *sc = (Source_file *) SCM_CELL_WORD_1 (smob);

  return sc->str_port_;
}


int
Source_file::print_smob (SCM smob, SCM port, scm_print_state *)
{
  Source_file *sc = (Source_file *) SCM_CELL_WORD_1 (smob);

  scm_puts ("#<Source_file ", port);
  scm_puts (sc->name_.c_str (), port);

  /* Do not print properties, that is too much hassle.  */
  scm_puts (" >", port);
  return 1;
}

