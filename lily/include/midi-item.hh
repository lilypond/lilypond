/*
  midi-item.hh -- declare Midi items

  (c) 1997 Jan Nieuwenhuizen <jan@digicash.com>
 */

#ifndef MIDI_ITEM_HH
#define MIDI_ITEM_HH

#include "string.hh"
#include "lily-proto.hh"

/**
  Any piece of midi information.

  Maybe use base classes for RIFF files?
 */
struct Midi_item {
    Midi_item( Audio_item* audio_item_l ); 
    static String i2varint_str( int i );
    void output( Midi_stream* midi_stream_l ) const;
    virtual String str() const = 0;

    Audio_item* audio_item_l_;
    int channel_i_;

private:
    Midi_item( Midi_item const& );
    Midi_item& operator =( Midi_item const& );
};

struct Midi_key : public Midi_item {
    Midi_key( Audio_item* audio_item_l );
	
    virtual String str() const;
};

/**
  Change instrument event
 */
struct Midi_instrument : public Midi_item {
    Midi_instrument( int channel_i, String instrument_str );

    virtual String str() const;
    String instrument_str_;
};
                                      
struct Midi_note : public Midi_item {
    /**
      Generate a note-event on a channel.

      @param #melreq_l# is the pitch. 
     */
    Midi_note( Audio_item* audio_item_l ); 

    virtual String str() const;

    /* *************** */
    int const c0_pitch_i_c_ = 60;
    bool on_b_;
    Byte dynamic_byte_;
};

struct Midi_duration : public Midi_item {
    Midi_duration( Real seconds_f );

    virtual String str() const;
    /* *************** */
    Real seconds_f_;
};

struct Midi_chunk : Midi_item {
    Midi_chunk();

    void add( String str );
    void set( String header_str, String data_str, String footer_str );
    virtual String str() const;
private:
    String data_str_;
    String footer_str_;
    String header_str_;
};

struct Midi_header : Midi_chunk {
    /* *************** */
    Midi_header( int format_i, int tracks_i, int clocks_per_4_i );
};

struct Midi_text : Midi_item {
    enum Type { 
	TEXT = 1, COPYRIGHT, TRACK_NAME, INSTRUMENT_NAME, LYRIC, 
	MARKER, CUE_POINT
    };

    Midi_text( Audio_item* audio_item_l );
    Midi_text( Midi_text::Type type, String text_str );

    virtual String str() const;

    Type type_;
    String text_str_;
};

struct Midi_tempo : Midi_item {
    Midi_tempo( Audio_item* audio_item_l ); 
    Midi_tempo( int per_minute_4_i );

    virtual String str() const;

    int per_minute_4_i_;
};

struct Midi_meter : Midi_item {
    Midi_meter( Audio_item* audio_item_l ); 

    virtual String str() const;
    int clocks_per_1_i_;
};

struct Midi_track : Midi_chunk {
    int number_i_;
    /* *************** */
    Midi_track();

    void add( int delta_time_i, String event );
    void add( Moment delta_time_moment, Midi_item* mitem_l );
};

#endif // MIDI_ITEM_HH
