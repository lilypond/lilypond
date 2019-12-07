/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2019 Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "std-vector.hh"
#include "lily-proto.hh"
#include "smobs.hh"

#include <iostream>
using namespace std;

/**
   class for reading and mapping a file.
*/

class Source_file : public Smob<Source_file>
{
public:
  int print_smob (SCM, scm_print_state *) const;
  SCM mark_smob () const;
  static const char * const type_p_name_;
  virtual ~Source_file ();
private:
  vector<char const *> newline_locations_;
  istream *istream_;
  vector<char> characters_;
  SCM str_port_;

  void load_stdin ();
  void init_port ();
  void init ();

public:
  Source_file (const string &fn);
  Source_file (const string&, const string&);

  char const *c_str () const;
  string quote_input (char const *pos_str0) const;
  istream *get_istream ();
  bool contains (char const *pos_str0) const;
  size_t length () const;
  ssize_t get_line (char const *pos_str0) const;
  void set_line (char const *pos_str0, ssize_t line);
  string name_string () const;
  string file_line_column_string (char const *str0) const;

  Slice line_slice (char const *pos_str0) const;
  string line_string (char const *pos_str0) const;
  void get_counts (char const *pos_str0,
                   ssize_t *, ssize_t *, ssize_t *, ssize_t *) const;

  SCM get_port () const;
  string name_;

protected:
  ssize_t line_offset_;
};

vector<char> gulp_file (const string &fn, size_t desired_size);

#endif /* SOURCE_FILE_HH */
