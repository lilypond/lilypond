/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef INPUT_HH
#define INPUT_HH

#include "lily-proto.hh"

/**
   Base class for anything that records its poisition in the parse file.
*/
class Input
{
  char const *start_;
  char const *end_;
  Source_file *source_file_;
public:
  Source_file *get_source_file () const;
  char const *start () const;
  char const *end () const;

  void set (Source_file *, char const *, char const *);
  void warning (string) const;
  void programming_error (string) const;
  void non_fatal_error (string) const;
  void error (string) const;
  void message (string) const;
  void set_spot (Input const &);
  void step_forward ();
  void set_location (Input const &, Input const &);

  Input spot () const;

  string location_string () const;
  string line_number_string () const;
  string file_string ()const;

  int line_number ()const;
  int column_number ()const;
  int end_line_number ()const;
  int end_column_number ()const;

  void get_counts (int *, int *, int *, int *) const;

  Input (Input const &i);
  Input ();
};


#include "smobs.hh"

SCM make_input (Input spot);
Input *unsmob_input (SCM);

extern Input dummy_input_global;

#endif // INPUT_HH
