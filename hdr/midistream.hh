//
//  midistream.hh -- part of LilyPond
//
//  copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#ifndef MIDI_STREAM_HH
#define MIDI_STREAM_HH

#include <iostream.h>
#include "string.hh"

/// Midi output
struct Midi_stream {
    ostream* os_p_;
    String filename_str_;
    int tempo_i_;
    int tracks_i_;
    
    Midi_stream( String filename_str, int tracks_i, int tempo_i );
    ~Midi_stream();

    Midi_stream& operator <<( String str );
    Midi_stream& operator <<( Midi_item& mitem_r );
    Midi_stream& operator <<( int i );

    void header();
    void open();

//private:
//    Midi_stream(Midi_stream const&);
};
#endif // MIDI_STREAM_HH //
