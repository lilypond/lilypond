/*
  audio-item.hh -- declare Audio_items

  (c) 1996, 1997 Jan Nieuwenhuizen <jan@digicash.com>
 */

#ifndef AUDIO_ITEM_HH
#define AUDIO_ITEM_HH

#include "lily-proto.hh"
#include "string.hh"

/**
  Any piece of audio information.
  We need virtual constructors, 
  let's try decentralised factory for specific audio implemenations.
 */
struct Audio_item {
    Audio_item( Request* req_l );

    /// Create a midi-item from myself.
    virtual Midi_item* midi_item_p() = 0;

#if 0
    /// Not implemented. Anyone?
    virtual Niff_item* niff_item_p() = 0;

    /// Not implemented. Anyone?
    virtual Cscore_item* score_item_p() = 0;
#endif
    
    Audio_column* audio_column_l_;
    Request* req_l_;

private:
    Audio_item( Audio_item const& );
    Audio_item& operator=( Audio_item const& );
};

struct Audio_key : public Audio_item {
    Audio_key( Request* req_l );

    virtual Midi_item* midi_item_p();
};

#if 0
struct Audio_instrument : public Audio_item {
    Audio_instrument( Request* req_l );
};
#endif
                                      
struct Audio_note : public Audio_item {
    Audio_note( Request* req_l, bool on_b );
    virtual Midi_item* midi_item_p();
    bool on_b_;
};

struct Audio_text : Audio_item {
    enum Type { 
	TEXT = 1, COPYRIGHT, TRACK_NAME, INSTRUMENT_NAME, LYRIC, 
	MARKER, CUE_POINT
    };

    Audio_text( Audio_text::Type type, String text_str );
    virtual Midi_item* midi_item_p();

    Type type_;
    String text_str_;
};

struct Audio_tempo : Audio_item {
    Audio_tempo( int per_minute_4_i );
    virtual Midi_item* midi_item_p();

    int per_minute_4_i_;
};

struct Audio_meter : Audio_item {
    Audio_meter( Request* req_l );
    virtual Midi_item* midi_item_p();
};

#endif // AUDIO_ITEM_HH

