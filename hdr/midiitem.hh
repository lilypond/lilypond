//
//  midiitem.hh -- part of LilyPond
//
//  copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#ifndef MIDI_ITEM_HH
#define MIDI_ITEM_HH
#include "string.hh"
#include "proto.hh"

struct Midi_item {
    /* *************** */
    static String i2varint_str( int i );
    virtual void output_midi( Midi_stream& midi_stream_r );
    virtual String str() = 0;
};

struct Midi_note : public Midi_item {
    /* *************** */
    int const c0_pitch_i_c_ = 60;
    Byte dynamic_byte_;

    /**
      Generate a note-event on a channel pitch.

      @param #melreq_l# is the pitch. 
     */
    Midi_note( Melodic_req* melreq_l, int channel_i, bool on_b );

    virtual String str();

    int channel_i_;
    int on_b_;
    int pitch_i_;
};

struct Midi_duration : public Midi_item {
    /* *************** */
    Midi_duration( Real seconds_f );

    virtual String str();

    Real seconds_f_;
};

struct Midi_chunk : Midi_item {
    /* *************** */
    Midi_chunk();

    void add( String str );
    void set( String header_str, String data_str, String footer_str );
    virtual String str();

private:
    String data_str_;
    String footer_str_;
    String header_str_;
};

struct Midi_header : Midi_chunk {
    /* *************** */
    Midi_header( int format_i, int tracks_i, int clocks_per_4_i );
};

struct Midi_tempo : Midi_item {
    /* *************** */
    Midi_tempo( int tempo_i );

    virtual String str();

    int tempo_i_;
};

struct Midi_track : Midi_chunk {
    /* *************** */
    int number_i_;
    Midi_track( int number_i );

    void add( int delta_time_i, String event );
//	void add( Moment delta_time_moment, Midi_item& mitem_r );
    void add( Moment delta_time_moment, Midi_item* mitem_l );
};

#endif // MIDI_ITEM_HH //

