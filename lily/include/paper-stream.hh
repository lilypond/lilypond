#ifndef PAPER_STREAM_HH
#define PAPER_STREAM_HH

#include <iostream.h>
#include "string.hh"

/** Paper output
  Baseclass for writing to a PostScript or TeX file.
  It counts braces to prevent nesting errors, and
  it will add a comment sign before each newline.
 */

class Paper_stream
{
public:
    bool outputting_comment;
    ostream *os;
    int nest_level;
    /// to check linelen in output. TeX has limits.
    int line_len_i_;
    
    /// open a file for writing
    Paper_stream (String filename);
    virtual void header () = 0;

    /// delegate conversion to scalar class
    virtual Paper_stream &operator <<(Scalar);

    /// close the file
    virtual ~Paper_stream();

private:
    Paper_stream (Paper_stream const&);
    void break_line();
};

#endif // PAPER_STREAM_HH
