/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "smobs.hh"

/**
   Base class for anything that records its position in the parse file.
*/
class Input : public Simple_smob<Input>
{
  char const *start_ = nullptr;
  char const *end_ = nullptr;
  Source_file *source_file_ = nullptr;

public:
  static const char *const type_p_name_;
  int print_smob (SCM, scm_print_state *) const;
  static SCM equal_p (SCM, SCM);
  SCM mark_smob () const;
  Source_file *get_source_file () const;
  char const *start () const { return start_; }
  char const *end () const { return end_; }
  size_t size () const { return end_ - start_; }

  void set (Source_file *, char const *, char const *);
  void error (const std::string &) const;
  void programming_error (const std::string &) const;
  void non_fatal_error (const std::string &) const;
  void warning (const std::string &) const;
  void message (const std::string &) const;
  void debug_output (const std::string &) const;
  void set_spot (Input const &);
  void step_forward ();
  void set_location (Input const &, Input const &);

  Input spot () const;

  std::string location_string () const;
  std::string line_number_string () const;
  std::string file_string () const;

  ssize_t line_number () const;
  ssize_t column_number () const;
  ssize_t end_line_number () const;
  ssize_t end_column_number () const;

  void get_counts (ssize_t *, ssize_t *, ssize_t *, ssize_t *) const;

  Input &operator= (Input const &) = default;
  Input (Input const &i) = default;
  Input () = default;

  friend std::string source_location (const Input &input) // for logging
  {
    return input.message_location ();
  }

protected:
  std::string message_location () const;
  std::string message_string (const std::string &msg) const;
};

extern Input dummy_input_global;

// The parser calls syntax functions with a lot of arguments
SCM with_location (SCM loc, SCM proc);
SCM with_location (SCM loc, SCM proc, SCM);
SCM with_location (SCM loc, SCM proc, SCM, SCM);
SCM with_location (SCM loc, SCM proc, SCM, SCM, SCM);
SCM with_location (SCM loc, SCM proc, SCM, SCM, SCM, SCM);
SCM with_location (SCM loc, SCM proc, SCM, SCM, SCM, SCM, SCM);

#endif // INPUT_HH
