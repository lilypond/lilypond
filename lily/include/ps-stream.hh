#ifndef PS_STREAM_HH
#define PS_STREAM_HH

#include "paper-stream.hh"

/** PS output.
  Use this class for writing to a PS file.
  It counts braces to prevent nesting errors, and
  it will add a comment sign before each newline.
  */
class Ps_stream : public Paper_stream
{
public:
    Ps_stream (String filename);
    virtual ~Ps_stream();

    virtual void header();
    virtual Paper_stream &operator <<(Scalar);
};

#endif // PS_STREAM_HH
