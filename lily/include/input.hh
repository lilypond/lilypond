/*
  input.hh -- declare Input

  source file of the LilyPond music typesetter

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef INPUT_HH
#define INPUT_HH

#include "flower-proto.hh"
#include "std-string.hh"

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
  void warning (std::string) const; // should use member func?
  void non_fatal_error (std::string) const;
  void error (std::string) const;
  void message (std::string) const;
  void set_spot (Input const &);
  void step_forward ();
  void set_location (Input const &, Input const &);
  Input spot () const;
  std::string location_string () const;
  std::string line_number_string () const;
  std::string file_string ()const;

  int line_number ()const;
  int column_number ()const;
  int end_line_number ()const;
  int end_column_number ()const;

  void get_counts (int *line, int *char_count, int *col) const;

  Input (Input const &i);
  Input ();
};

#endif // INPUT_HH
