/*
  midi-item.hh -- declare Midi items

  (c)  1997--2001 Jan Nieuwenhuizen <janneke@gnu.org>
 */

#ifndef MIDI_ITEM_HH
#define MIDI_ITEM_HH

#include "cons.hh"
#include "string.hh"
#include "lily-proto.hh"
#include "moment.hh"
#include "audio-item.hh"

/**
  Any piece of midi information.

  Maybe use base classes for RIFF files?
 */
class Midi_item
{
public:
  Midi_item ();
  virtual ~Midi_item ();

  /// factory
  static Midi_item* midi_p (Audio_item* a);

  static String i2varint_str (int i);

  virtual String str () const = 0;

  int channel_i_;
};

/**
  timed MIDI event
 */
class Midi_event
{
public:
  Midi_event (Moment delta_mom, Midi_item* midi_l);

  Moment delta_mom_;
  Midi_item* midi_p_;
  String str () const;
};

/**
  variable sized MIDI data
 */
class Midi_chunk : public Midi_item
{
public:
  void set (String header_str, String data_str, String footer_str);
  virtual String str () const;
  virtual String data_str () const;

private:
  String data_str_;
  String footer_str_;
  String header_str_;
};

class Midi_duration : public Midi_item
{
public:
  Midi_duration (Real seconds_f);

  virtual String str () const;
  Real seconds_f_;
};

class Midi_header : public Midi_chunk
{
public:
  Midi_header (int format_i, int tracks_i, int clocks_per_4_i);
};

/**
  Change instrument event
 */
class Midi_instrument : public Midi_item
{
public:
  Midi_instrument (Audio_instrument*);

  virtual String str () const;

  Audio_instrument* audio_l_;
};
                                      

class Midi_key : public Midi_item
{
public:
  Midi_key (Audio_key*);
	
  virtual String str () const;

  Audio_key* audio_l_;
};

class Midi_time_signature : public Midi_item
{
public:
  Midi_time_signature (Audio_time_signature*);
  
  virtual String str () const;

  Audio_time_signature* audio_l_;
  int clocks_per_1_i_;
};

/**
  Turn a note on.
 */
class Midi_note : public Midi_item
{
public:
  Midi_note (Audio_note*);

  Moment length_mom () const;
  int pitch_i () const;
  virtual String str () const;

  Audio_note* audio_l_;

  static int const c0_pitch_i_c_ = 60;
  Byte dynamic_byte_;
};

/**
  Turn a note off 
 */
class Midi_note_off : public Midi_note
{
public:
  Midi_note_off (Midi_note*); 

  virtual String str () const;

  Midi_note* on_l_;
  Byte aftertouch_byte_;
};

class Midi_text : public Midi_item
{
public:
  enum Type { 
    TEXT = 1, COPYRIGHT, TRACK_NAME, INSTRUMENT_NAME, LYRIC, 
    MARKER, CUE_POINT
  };

  Midi_text (Audio_text*);
    
  virtual String str () const;

  Audio_text* audio_l_;
};

class Midi_dynamic : public Midi_item
{
public:
  Midi_dynamic (Audio_dynamic*);
  
  virtual String str () const;

  Audio_dynamic* audio_l_;
};

class Midi_piano_pedal : public Midi_item
{
public:
  Midi_piano_pedal (Audio_piano_pedal*);
  
  virtual String str () const;

  Audio_piano_pedal* audio_l_;
};

class Midi_tempo : public Midi_item
{
public:
  Midi_tempo (Audio_tempo*);
  
  virtual String str () const;

  Audio_tempo* audio_l_;
};

class Midi_track : public Midi_chunk
{
public:
  int number_i_;
  Cons_list<Midi_event> event_p_list_;
  
  Midi_track ();

  void add (Moment delta_time_mom, Midi_item* midi_l);
  virtual String data_str () const;
};

#endif // MIDI_ITEM_HH
