/*
  input.hh -- declare Input

  source file of the LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef INPUT_HH
#define INPUT_HH

#include "flower-proto.hh"

/**
  Base class for anything that records its poisition in the parse file.
 */
class Input {
public:
  char const *start_;
  char const *end_;
  Source_file * source_file_;
    
  void warning (String) const; // should use member func?
  void non_fatal_error (String) const;
  void error (String) const;
  void message (String) const;
  void set_spot (Input const &);
  void step_forward ();
  void set_location (Input const &, Input const &);
  Input spot () const;
  String location_string () const;
  String line_number_string () const;


  String file_string ()const;
  int line_number ()const;
  int column_number ()const;
  int end_line_number ()const;
  int end_column_number ()const;

  
  Input (Input const &i);
  Input ();
};

#endif // INPUT_HH
