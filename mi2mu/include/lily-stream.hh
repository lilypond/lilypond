//
//  lily-stream.hh -- part of LilyPond
//
//  copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

// should i be named Mudela_stream?

#ifndef LILY_STREAM_HH
#define LILY_STREAM_HH

/// Lily output
class Lily_stream {
public:    
    Lily_stream( String filename_str );
    ~Lily_stream();

    Lily_stream& operator <<( String str );
    Lily_stream& operator <<( Midi_event& midi_event_r );

private:
    void header();
    void open();
    void output( String str );
    void output_wrapped( String str );

    ostream* os_p_;
    String filename_str_;
    int indent_i_;
    int column_i_;
    int wrap_column_i_;
    bool comment_mode_b_;
};

#endif // LILY_STREAM_HH

