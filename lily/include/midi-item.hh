/*
  midi-item.hh -- declare Midi items

  (c)  1997--1998 Jan Nieuwenhuizen <janneke@gnu.org>
 */

#ifndef MIDI_ITEM_HH
#define MIDI_ITEM_HH

#include "string.hh"
#include "lily-proto.hh"
#include "proto.hh"
#include "plist.hh"
#include "virtual-methods.hh"
#include "moment.hh"

/**
  Any piece of midi information.

  Maybe use base classes for RIFF files?
 */
struct Midi_item {
  DECLARE_MY_RUNTIME_TYPEINFO;
  Midi_item (Audio_item* audio_item_l); 
  virtual ~Midi_item ();
  static String i2varint_str (int i);
  virtual String str () const = 0;

  Audio_item* audio_item_l_;
  int channel_i_;

private:
  Midi_item (Midi_item const&);
  Midi_item& operator = ( Midi_item const&);
};

/**
  timed MIDI event
 */
struct Midi_event
{
  Midi_event (Moment delta_mom, Midi_item* mitem_l);
  ~Midi_event ();
  Moment delta_mom_;
  Midi_item* mitem_p_;
  String str () const;
};

/**
  variable sized MIDI data
 */
struct Midi_chunk : Midi_item {
  DECLARE_MY_RUNTIME_TYPEINFO;
  Midi_chunk ();

  void set (String header_str, String data_str, String footer_str);
  virtual String str () const;
  virtual String data_str () const;

private:
  String data_str_;
  String footer_str_;
  String header_str_;
};

struct Midi_duration : public Midi_item {
  DECLARE_MY_RUNTIME_TYPEINFO;
  Midi_duration (Real seconds_f);

  virtual String str () const;
  Real seconds_f_;
};

struct Midi_header : Midi_chunk {
  DECLARE_MY_RUNTIME_TYPEINFO;

  Midi_header (int format_i, int tracks_i, int clocks_per_4_i);
};

/**
  Change instrument event
 */
struct Midi_instrument : public Midi_item {
  DECLARE_MY_RUNTIME_TYPEINFO;
  Midi_instrument (int channel_i, String instrument_str);

  virtual String str () const;
  String instrument_str_;
};
                                      

struct Midi_key : public Midi_item {
  DECLARE_MY_RUNTIME_TYPEINFO;
  Midi_key (Audio_item* audio_item_l);
	
  virtual String str () const;
};

struct Midi_time_signature : Midi_item {

  DECLARE_MY_RUNTIME_TYPEINFO;
  Midi_time_signature (Audio_item* audio_item_l); 
  
  virtual String str () const;
  int clocks_per_1_i_;
};

/**
  Turn a note on (blond).
 */
struct Midi_note : public Midi_item {
  DECLARE_MY_RUNTIME_TYPEINFO;
  Midi_note (Audio_item* audio_item_l); 

  Moment duration () const;
  int pitch_i () const;
  virtual String str () const;

  static int const c0_pitch_i_c_ = 60;
  Byte dynamic_byte_;
};

/**
  Turn a note off (dark).
 */
struct Midi_note_off : public Midi_item {
  DECLARE_MY_RUNTIME_TYPEINFO;
  Midi_note_off (Midi_note*); 

  int pitch_i () const;
  virtual String str () const;

  Byte aftertouch_byte_;
};

struct Midi_text : Midi_item {
  DECLARE_MY_RUNTIME_TYPEINFO;
    
  enum Type { 
    TEXT = 1, COPYRIGHT, TRACK_NAME, INSTRUMENT_NAME, LYRIC, 
    MARKER, CUE_POINT
  };
  Midi_text (Midi_text::Type type, String text_str);
  Midi_text (Audio_item* audio_item_l);
    
  virtual String str () const;

  Type type_;
  String text_str_;
};

struct Midi_tempo : Midi_item {
  DECLARE_MY_RUNTIME_TYPEINFO;
  Midi_tempo (int per_minute_4_i);
  Midi_tempo (Audio_item* audio_item_l); 
  
  virtual String str () const;

  int per_minute_4_i_;
};

struct Midi_track : Midi_chunk {
  DECLARE_MY_RUNTIME_TYPEINFO;
  int number_i_;
  Pointer_list<Midi_event*> event_p_list_;
    
  Midi_track ();
  ~Midi_track ();

  void add (Moment delta_time_mom, Midi_item* mitem_l);
  virtual String data_str () const;

private:
  // copy trap
  Midi_track (Midi_track const&);
};

#endif // MIDI_ITEM_HH
