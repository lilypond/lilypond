//
//  lilypond-stream.hh -- part of LilyPond
//
//  (c) 1997--2001 Jan Nieuwenhuizen <janneke@gnu.org>

// should i be named Lilypond_stream?

#ifndef LILYPOND_STREAM_HH
#define LILYPOND_STREAM_HH

#include "midi2ly-proto.hh"
#include "string.hh"
//#include "scalar.hh"

/// Lily output
class Lilypond_stream {
public:    
    Lilypond_stream (String filename_str);
    ~Lilypond_stream();

    Lilypond_stream& operator << (char c);
    Lilypond_stream& operator << (String s);
    Lilypond_stream& operator << (Lilypond_item& lilypond_item_r);

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

#endif // LILYPOND_STREAM_HH

