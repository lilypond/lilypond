//
//  midiitem.hh -- part of GNU LilyPond
//
//  copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#ifndef MIDI_ITEM_HH
#define MIDI_ITEM_HH
#include "string.hh"
#include "lily-proto.hh"

struct Midi_item {
    static String i2varint_str( int i );
    virtual void output_midi( Midi_stream& midi_stream_r ) const;
    virtual String str() const = 0;
    NAME_MEMBERS();
};

struct Midi_key : public Midi_item {
    Midi_key( int accidentals_i, int minor_i );
	
    virtual String str() const;
    NAME_MEMBERS();
    /* *************** */
    int accidentals_i_;
    int minor_i_;
};

struct Midi_note : public Midi_item {
    /**
      Generate a note-event on a channel pitch.

      @param #melreq_l# is the pitch. 
     */
    Midi_note( Melodic_req* melreq_l, int channel_i, bool on_b );
    NAME_MEMBERS();
    virtual String str() const;

    /* *************** */
    int const c0_pitch_i_c_ = 60;
    Byte dynamic_byte_;

    int channel_i_;
    int on_b_;
    int pitch_i_;
};

struct Midi_duration : public Midi_item {
    Midi_duration( Real seconds_f );

    virtual String str() const;
    NAME_MEMBERS();
    /* *************** */
    Real seconds_f_;
};

struct Midi_chunk : Midi_item {
    Midi_chunk();

    void add( String str );
    void set( String header_str, String data_str, String footer_str );
    virtual String str() const;
    NAME_MEMBERS();
private:
    String data_str_;
    String footer_str_;
    String header_str_;
};

struct Midi_header : Midi_chunk {
    /* *************** */
    Midi_header( int format_i, int tracks_i, int clocks_per_4_i );
    NAME_MEMBERS();
};

struct Midi_text : Midi_item {
    
    enum Type { 
	TEXT = 1, COPYRIGHT, TRACK_NAME, INSTRUMENT_NAME, LYRIC, 
	MARKER, CUE_POINT
    };
    Midi_text( Midi_text::Type type, String text_str );
    NAME_MEMBERS();
    virtual String str() const;
    /* *************** */
    Type type_;
    String text_str_;
    
};

struct Midi_tempo : Midi_item {
    Midi_tempo( int per_minute_4_i );
    NAME_MEMBERS();

    virtual String str() const;
    /* *************** */

    int per_minute_4_i_;
};

struct Midi_time : Midi_item {

    Midi_time( int num_i, int den_i, int clocks_per_1_i );
    NAME_MEMBERS();

    virtual String str() const;

    /* *************** */
    int num_i_;
    int den_i_;
    int clocks_per_1_i_;
};

struct Midi_track : Midi_chunk {
    int number_i_;
    /* *************** */
    Midi_track( );
    NAME_MEMBERS();

    void add( int delta_time_i, String event );
//	void add( Moment delta_time_moment, Midi_item& mitem_r );
    void add( Moment delta_time_moment, Midi_item* mitem_l );
};

#endif // MIDI_ITEM_HH //

