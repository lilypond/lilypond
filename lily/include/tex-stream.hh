#ifndef TEX_STREAM_HH
#define TEX_STREAM_HH

#include "paper-stream.hh"

/** TeX output.
  Use this class for writing to a TeX file.
  It counts braces to prevent nesting errors, and
  it will add a comment sign before each newline.
  */
class Tex_stream : public Paper_stream
{
public:
    Tex_stream (String filename);
    virtual ~Tex_stream();

    virtual void header();
    virtual Paper_stream &operator <<(Scalar);
};

#endif // TEX_STREAM_HH
