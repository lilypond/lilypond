#ifndef TSTREAM__HH
#define TSTREAM__HH

#include <iostream.h>
#include "string.hh"


/** TeX output.
  Use this class for writing to a TeX file.
  It counts braces to prevent nesting errors, and
  it will add a comment sign before each newline.
  */
class Tex_stream {
    void break_line();
public:
    bool outputting_comment;
    ostream *os;
    int nest_level;
    /// to check linelen in output. TeX has limits.
    int line_len_i_;
    
    /// open a file for writing
    Tex_stream (String filename);
    void header();

    /// delegate conversion to scalar class
    Tex_stream &operator <<(Scalar);

    /// close the file
    ~Tex_stream();
private:
    Tex_stream (Tex_stream const&);
};
#endif
