/*
  audio-item.hh -- declare Audio_items

  (c) 1996,  1997--1998 Jan Nieuwenhuizen <janneke@gnu.org>
 */

#ifndef AUDIO_ITEM_HH
#define AUDIO_ITEM_HH

#include "lily-proto.hh"
#include "string.hh"
#include "audio-element.hh"

/**
  Any piece of audio information.
  We need virtual constructors, 
  let's try decentralised factory for specific audio implemenations.

  TODO:

  virtual Niff_item* niff_item_p() = 0;
  
  virtual CSound_item* score_item_p() = 0;
 */
struct Audio_item : public Audio_element {
  Audio_item (Request* req_l);

  /// Create a midi-item from myself.
  virtual Midi_item* midi_item_p() = 0;

  Audio_column* audio_column_l_;
  /*
    THIS SUX. This ties the output system to the input system.  Bad move.
   */
  Request* req_l_;

      
protected:
  virtual void do_print () const;
  
private:
  Audio_item (Audio_item const&);
  Audio_item& operator=( Audio_item const&);
};

struct Audio_key : public Audio_item {
  Audio_key (Request* req_l);
  
  virtual Midi_item* midi_item_p();
};

struct Audio_instrument : public Audio_item {
  Audio_instrument (String instrument_str);
  virtual Midi_item* midi_item_p();
  String str_;
    
};
                                      
struct Audio_note : public Audio_item {
  
  Audio_note (Request* req_l, int transposing_i = 0);
  virtual Midi_item* midi_item_p();
  int transposing_i_;
};

struct Audio_text : Audio_item {
  enum Type { 
    TEXT = 1, COPYRIGHT, TRACK_NAME, INSTRUMENT_NAME, LYRIC, 
    MARKER, CUE_POINT
  };
  
  Audio_text (Audio_text::Type type, String text_str);
  virtual Midi_item* midi_item_p();

  Type type_;
  String text_str_;
};

struct Audio_tempo : Audio_item {
  Audio_tempo (int per_minute_4_i);
  virtual Midi_item* midi_item_p();
  
  int per_minute_4_i_;
};

struct Audio_time_signature : Audio_item {
  Audio_time_signature (Request* req_l);
  virtual Midi_item* midi_item_p();
  
};

#endif // AUDIO_ITEM_HH

