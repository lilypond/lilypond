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
    
    Lily_stream( String filename_str );
    ~Lily_stream();

    Lily_stream& operator <<( String str );
    Lily_stream& operator <<( Midi_event& midi_event_r );

    void header();
    void open();
};

#endif // LILY_STREAM_HH

