/*
  input.hh -- declare Input

  source file of the LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef INPUT_HH
#define INPUT_HH

#include "flower-proto.hh"

/**
  Base class for anything that records its poisition in the parse file.
 */
class Input {
public:
  char const *defined_str0_ ;
  Source_file * source_file_;
    
  void warning (String) const; // should use member func?
  void non_fatal_error (String) const;
  void error (String) const;
  void message (String) const;
  void set_spot (Input const &);
  Input spot () const;
  String location_string () const;
  String line_number_string () const;


  String file_string ()const;
  int line_number ()const;
  int column_number ()const;

  
  Input (Source_file*, char const*);
  Input ();
};

#endif // INPUT_HH
