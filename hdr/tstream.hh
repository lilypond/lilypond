#ifndef TSTREAM__HH
#define TSTREAM__HH

#include <iostream.h>
#include "string.hh"

/// TeX output
struct Tex_stream {
    bool outputting_comment;
    ostream *os;
    int nest_level;
    
    /// open a file for writing
    Tex_stream(String filename);
    void header();
    /// delegate conversion to string class.
    Tex_stream &operator<<(String);

    /// close the file
    ~Tex_stream();
};
/**
  Use this class for writing to a TeX file.
  It counts braces to prevent nesting errors, and
  it will add a comment sign before each newline.
  */
#endif
