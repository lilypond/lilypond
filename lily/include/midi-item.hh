/*
  midi-item.hh -- declare Midi items

  (c)  1997--2002 Jan Nieuwenhuizen <janneke@gnu.org>
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
  static Midi_item* get_midi (Audio_item* a);

  static String i2varint_string (int i);

  virtual String string () const = 0;

  int channel_;
};

/**
  timed MIDI event
 */
class Midi_event
{
public:
  Midi_event (Moment delta_mom, Midi_item* midi);

  Moment delta_mom_;
  Midi_item* midi_;
  String string () const;
};

/**
  variable sized MIDI data
 */
class Midi_chunk : public Midi_item
{
public:
  void set (String header_string, String data_string, String footer_string);
  virtual String string () const;
  virtual String data_string () const;

private:
  String data_string_;
  String footer_string_;
  String header_string_;
};

class Midi_duration : public Midi_item
{
public:
  Midi_duration (Real seconds_f);

  virtual String string () const;
  Real seconds_;
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

  virtual String string () const;

  Audio_instrument* audio_;
};
                                      

class Midi_key : public Midi_item
{
public:
  Midi_key (Audio_key*);
	
  virtual String string () const;

  Audio_key* audio_;
};

class Midi_time_signature : public Midi_item
{
public:
  Midi_time_signature (Audio_time_signature*);
  
  virtual String string () const;

  Audio_time_signature* audio_;
  int clocks_per_1_;
};

/**
  Turn a note on.
 */
class Midi_note : public Midi_item
{
public:
  Midi_note (Audio_note*);

  Moment get_length () const;
  int get_pitch () const;
  virtual String string () const;

  Audio_note* audio_;

  static int const c0_pitch_i_ = 60;
  Byte dynamic_byte_;
};

/**
  Turn a note off 
 */
class Midi_note_off : public Midi_note
{
public:
  Midi_note_off (Midi_note*); 

  virtual String string () const;

  Midi_note* on_;
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
    
  virtual String string () const;

  Audio_text* audio_;
};

class Midi_dynamic : public Midi_item
{
public:
  Midi_dynamic (Audio_dynamic*);
  
  virtual String string () const;

  Audio_dynamic* audio_;
};

class Midi_piano_pedal : public Midi_item
{
public:
  Midi_piano_pedal (Audio_piano_pedal*);
  
  virtual String string () const;

  Audio_piano_pedal* audio_;
};

class Midi_tempo : public Midi_item
{
public:
  Midi_tempo (Audio_tempo*);
  
  virtual String string () const;

  Audio_tempo* audio_;
};

class Midi_track : public Midi_chunk
{
public:
  int number_;

  /*
    Compensate for starting grace notes.
   */
  Cons_list<Midi_event> event_p_list_;
  
  Midi_track ();

  void add (Moment delta_time_mom, Midi_item* midi);
  virtual String data_string () const;
};

#endif // MIDI_ITEM_HH
