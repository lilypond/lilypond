//
//  lily-stream.hh -- part of LilyPond
//
//  copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

// should i be named Mudela_stream?

#ifndef LILY_STREAM_HH
#define LILY_STREAM_HH

/// Lily output
struct Lily_stream {
    ostream* os_p_;
    String filename_str_;
    int indent_i_;
    int column_i_;
    int wrap_column_i_;
    bool comment_mode_bo_;
    
    Lily_stream( String filename_str );
    ~Lily_stream();

    Lily_stream& operator <<( String str );
    Lily_stream& operator <<( Midi_event& midi_event_r );

    void check_comment( String str );
    void header();
    void indent();
    void newline();
    void open();
    void tnedni();
};

#endif // LILY_STREAM_HH

