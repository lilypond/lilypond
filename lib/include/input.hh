/*
  input.hh -- declare Input

  source file of the LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef INPUT_HH
#define INPUT_HH

#include "proto.hh"

/**
  Base class for anything that records its poisition in the parse file.
 */
class Input {
  char const *defined_ch_C_ ;
  Source_file * source_file_l_;
public:
    
  void warning (String) const; // should use member func?
  void non_fatal_error (String) const;
  void error (String) const;
  void message (String) const;
  void set_spot (Input const &);
  Input spot () const;
  String location_str () const;
  Input (Source_file*, char const*);
  Input ();
};

#endif // INPUT_HH
