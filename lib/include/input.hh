/*
  input.hh -- declare Input

  source file of the LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
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
    void error (String) const;
    void message (String) const;
    void set_spot (Input const &);
    
    String location_str () const;
    Input (Source_file*, char const*);
    Input ();
    Input (Input const& );
};

#endif // INPUT_HH
