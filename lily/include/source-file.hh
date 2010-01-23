/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2010 Jan Nieuwenhuizen <janneke@gnu.org>

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

   duplicates a lot of Data_file and Text_stream.
   should look at including Data_file's functionality:
   get_line (), get_word () here.
*/

class Source_file
{
  vector<char const*> newline_locations_;
  istream *istream_;
  vector<char> characters_;
  SCM str_port_;

  void load_stdin ();
  void init_port ();
  void init ();
  
  DECLARE_SMOBS (Source_file);
public:
  Source_file (string fn);
  Source_file (string, string);

  char const *c_str () const;
  virtual string quote_input (char const *pos_str0) const;
  istream *get_istream ();
  bool contains (char const *pos_str0) const;
  int length () const;
  virtual int get_line (char const *pos_str0) const;
  void set_line (char const *pos_str0, int i);
  string name_string () const;
  string file_line_column_string (char const *str0) const;

public:
  Slice line_slice (char const *pos_str0) const;
  string line_string (char const *pos_str0) const;
  void get_counts (char const *pos_str0, int *, int *, int *, int *) const;
  
  SCM get_port () const;
  string name_;

protected:
  int line_offset_;
};

vector<char> gulp_file (string fn, int desired);

#endif /* SOURCE_FILE_HH */

