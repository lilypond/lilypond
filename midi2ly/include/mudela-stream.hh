//
//  mudela-stream.hh -- part of LilyPond
//
//  copyright 1997 Jan Nieuwenhuizen <janneke@gnu.org>

// should i be named Mudela_stream?

#ifndef MUDELA_STREAM_HH
#define MUDELA_STREAM_HH

#include "mi2mu-proto.hh"
#include "string.hh"
#include "scalar.hh"

/// Lily output
class Mudela_stream {
public:    
    Mudela_stream (String filename_str);
    ~Mudela_stream();

    Mudela_stream& operator << (Scalar s);
    Mudela_stream& operator << (Mudela_item& mudela_item_r);

private:
    void handle_pending_indent();
    void header();
    void open();
    void output (String str);
    void output_wrapped (String str);

    ostream* os_p_;
    String filename_str_;
    int indent_i_;
    int column_i_;
    int pending_indent_i_;
    int wrap_column_i_;
    bool comment_mode_b_;
};

#endif // MUDELA_STREAM_HH

