/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Jan Nieuwenhuizen <janneke@gnu.org>
  Han-Wen Nienhuys <hanwen@xs4all.nl>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "config.hh"

#include "source-file.hh"

#include "international.hh"
#include "misc.hh"
#include "warn.hh"
#include "lily-imports.hh"

#include <algorithm>
#include <cstdio>
#include <sstream>

using std::istream;
using std::istringstream;
using std::string;

void
Source_file::load_stdin ()
{
  data_.clear ();
  int c;
  while ((c = fgetc (stdin)) != EOF)
    data_.push_back (static_cast<char> (c));
}

/*
  return contents of FILENAME.
 */
string
gulp_file (const string &filename, size_t desired_size)
{
  /* "b" must ensure to open literally, avoiding text (CR/LF)
     conversions.  */
  FILE *f = fopen (filename.c_str (), "rb"); // TODO: RAII
  if (!f)
    {
      warning (_f ("cannot open file: `%s'", filename.c_str ()));
      return {};
    }

  fseek (f, 0, SEEK_END);
  const auto real_size = ftell (f);
  if (real_size < 0)
    {
      warning (_f ("failed to get file size: `%s'", filename.c_str ()));
      fclose (f);
      return {};
    }
  size_t read_count = real_size;

  if (desired_size > 0)
    read_count = std::min (read_count, desired_size);

  rewind (f);

  string dest (read_count, 0);
  size_t bytes_read = fread (&dest[0], sizeof (char), read_count, f);
  if (bytes_read < read_count)
    {
      warning (_f ("expected to read %zu characters, got %zu", read_count,
                   bytes_read));
      dest.resize (bytes_read);
    }
  fclose (f);
  return dest;
}

void
Source_file::init ()
{
  istream_ = 0;
  line_offset_ = 0;
  smobify_self ();
}

Source_file::Source_file (const string &filename, const string &data)
{
  init ();

  name_ = filename;

  data_ = data;

  init_newlines ();
}

void
Source_file::init_newlines ()
{
  for (vsize i = 0; i < data_.size (); i++)
    if (data_[i] == '\n')
      newline_locations_.push_back (&data_[0] + i);
}

Source_file::Source_file (const string &filename_string)
{
  init ();

  name_ = filename_string;

  if (filename_string == "-")
    load_stdin ();
  else
    data_ = gulp_file (filename_string, -1);

  init_newlines ();
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
          istream_->setstate (std::ios::eofbit);
          //      istream_->set (std::ios::eofbit);
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
      ssize_t l, ch, col, offset;
      get_counts (context_str0, &l, &ch, &col, &offset);

      return name_string () + ":" + std::to_string (l) + ":"
             + std::to_string (col + 1);
    }
}

string
Source_file::quote_input (char const *pos_str0) const
{
  if (!contains (pos_str0))
    return " (" + _ ("position unknown") + ")";

  ssize_t l, ch, col, offset;
  get_counts (pos_str0, &l, &ch, &col, &offset);
  string line = line_string (pos_str0);
  string context = line.substr (0, offset);
  context += '\n';
  if (col > 0)
    context += string (col, ' ');
  context += line.substr (offset, line.length () - offset);
  return context;
}

string
Source_file::name_string () const
{
  return name_;
}

Source_file::~Source_file ()
{
  delete istream_;
}

Source_file::SourceSlice
Source_file::line_slice (char const *pos_str0) const
{
  if (!contains (pos_str0))
    return SourceSlice (0, 0);

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

  return SourceSlice (begin_str0 - data_str0, end_str0 - data_str0);
}

string
Source_file::line_string (char const *pos_str0) const
{
  if (!contains (pos_str0))
    return "";

  SourceSlice line = line_slice (pos_str0);
  char const *data_str0 = c_str ();
  return string (data_str0 + line[LEFT], line.length ());
}

void
Source_file::get_counts (char const *pos_str0, ssize_t *line_number,
                         ssize_t *line_char, ssize_t *column,
                         ssize_t *line_byte_offset) const
{
  // Initialize arguments to defaults, needed if pos_str0 is not in source
  *line_number = 0;
  *line_char = 0;
  *column = 0;
  *line_byte_offset = 0;

  if (!contains (pos_str0))
    return;

  *line_number = get_line (pos_str0);

  SourceSlice line = line_slice (pos_str0);
  char const *data = c_str ();
  char const *line_start = data + line[LEFT];

  ssize left = pos_str0 - line_start;
  *line_byte_offset = left;

  // TODO: copying into line_begin looks pointless and wasteful
  string line_begin (line_start, left);
  char const *line_chars = line_begin.c_str ();

  for (; left > 0; --left, ++line_chars)
    {
      // Skip UTF-8 continuation bytes.  This is simplistic but
      // robust, and we warn against non-UTF-8 input in the lexer
      // already.  In the case of non-UTF-8 or of this function being
      // called in mid-character, the results are somewhat arbitrary,
      // but there is no really sane definition anyway.
      if ((*line_chars & 0xc0) == 0x80)
        continue;

      if (*line_chars == '\t')
        (*column) = (*column / 8 + 1) * 8;
      else
        (*column)++;

      (*line_char)++;
    }
}

bool
Source_file::contains (char const *pos_str0) const
{
  return (pos_str0 && (pos_str0 >= c_str ())
          && (pos_str0 <= c_str () + length ()));
}

ssize_t
Source_file::get_line (char const *pos_str0) const
{
  if (!contains (pos_str0))
    return 0;

  if (!newline_locations_.size ())
    return 1 + line_offset_;

  /* this will find the '\n' character at the end of our line */
  auto lo = std::lower_bound (newline_locations_.begin (),
                              newline_locations_.end (), pos_str0);

  /* the return value will be indexed from 1 */
  return (lo - newline_locations_.begin ()) + 1 + line_offset_;
}

void
Source_file::set_line (char const *pos_str0, ssize_t line)
{
  if (pos_str0)
    {
      auto current_line = get_line (pos_str0);
      line_offset_ += line - current_line;

      assert (line == get_line (pos_str0));
    }
  else
    line_offset_ = line;
}

size_t
Source_file::length () const
{
  return data_.size ();
}

char const *
Source_file::c_str () const
{
  return data_.c_str ();
}

/****************************************************************/

const char *const Source_file::type_p_name_ = "ly:source-file?";

int
Source_file::print_smob (SCM port, scm_print_state *) const
{
  scm_puts ("#<Source_file ", port);
  scm_puts (name_.c_str (), port);

  /* Do not print properties, that is too much hassle.  */
  scm_puts (" >", port);
  return 1;
}
