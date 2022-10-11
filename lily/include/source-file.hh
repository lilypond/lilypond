/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2022 Jan Nieuwenhuizen <janneke@gnu.org>

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

#ifndef SOURCE_FILE_HH
#define SOURCE_FILE_HH

#include "lily-proto.hh"
#include "smobs.hh"

#include <iostream>
#include <vector>

// Keeps an input file in memory. All locations (for error reporting)
// directly point into the data kept here.
class Source_file : public Smob<Source_file>
{
public:
  int print_smob (SCM, scm_print_state *) const;
  static const char *const type_p_name_;
  virtual ~Source_file ();

private:
  std::vector<char const *> newline_locations_;
  std::istream *istream_;

  std::string data_;

  void load_stdin ();
  void init ();
  void init_newlines ();

  typedef Interval_t<vsize> SourceSlice;

public:
  Source_file (const std::string &fn);
  Source_file (const std::string &, const std::string &);

  char const *c_str () const;
  std::string quote_input (char const *pos_str0) const;
  std::istream *get_istream ();
  bool contains (char const *pos_str0) const;
  size_t length () const;
  ssize_t get_line (char const *pos_str0) const;
  void set_line (char const *pos_str0, ssize_t line);
  std::string name_string () const;
  std::string file_line_column_string (char const *str0) const;

  SourceSlice line_slice (char const *pos_str0) const;
  std::string line_string (char const *pos_str0) const;
  void get_counts (char const *pos_str0, ssize_t *, ssize_t *, ssize_t *,
                   ssize_t *) const;

  std::string name_;

protected:
  ssize_t line_offset_;
};

std::string gulp_file (const std::string &fn, size_t desired_size);

#endif /* SOURCE_FILE_HH */
