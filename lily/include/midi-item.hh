/*
  midi-item.hh -- declare Midi items

  (c) 1997--2007 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef MIDI_ITEM_HH
#define MIDI_ITEM_HH

#include "audio-item.hh"
#include "std-vector.hh"

/**
   Any piece of midi information.

   Maybe use base classes for RIFF files?
*/
class Midi_item
{
public:
  DECLARE_CLASSNAME(Midi_item);
  Midi_item ();
  virtual ~Midi_item ();
  virtual char const *name () const;

  static Midi_item *get_midi (Audio_item *a);

  static string i2varint_string (int i);

  virtual string to_string () const = 0;
};

class Midi_channel_item : public Midi_item
{
public:
  int channel_;
  DECLARE_CLASSNAME(Midi_channel_item);
  Midi_channel_item ();
  virtual const char *name () const { return "Midi_channel_item"; }
  virtual ~Midi_channel_item ();
};

/**
   timed MIDI event
*/
class Midi_event
{
public:
  Midi_event (int delta, Midi_item *midi);

  int delta_ticks_;
  Midi_item *midi_;
  string to_string () const;
};

/**
   variable sized MIDI data
*/
class Midi_chunk : public Midi_item
{
public:
  void set (string header_string, string data_string, string footer_string);
  virtual string to_string () const;
  virtual string data_string () const;
  DECLARE_CLASSNAME(Midi_chunk);

private:
  string data_string_;
  string footer_string_;
  string header_string_;
};

class Midi_duration : public Midi_item
{
public:
  Midi_duration (Real seconds_f);

  virtual string to_string () const;
  Real seconds_;
};

class Midi_header : public Midi_chunk
{
public:
    DECLARE_CLASSNAME(Midi_header);

  Midi_header (int format_i, int tracks_i, int clocks_per_4_i);
};

/**
   Change instrument event
*/
class Midi_instrument : public Midi_channel_item
{
public:
  Midi_instrument (Audio_instrument *);

  DECLARE_CLASSNAME(Midi_instrument);
  virtual string to_string () const;

  Audio_instrument *audio_;
};

class Midi_key : public Midi_item
{
public:
  Midi_key (Audio_key *);
  DECLARE_CLASSNAME(Midi_key);

  virtual string to_string () const;

  Audio_key *audio_;
};

class Midi_time_signature : public Midi_item
{
public:
  Midi_time_signature (Audio_time_signature *);
  DECLARE_CLASSNAME(Midi_time_signature);

  virtual string to_string () const;

  Audio_time_signature *audio_;
  int clocks_per_1_;
};

/**
   Turn a note on.
*/
class Midi_note : public Midi_channel_item
{
public:
  Midi_note (Audio_note *);
  DECLARE_CLASSNAME(Midi_note);

  int get_pitch () const;
  int get_fine_tuning () const;
  virtual string to_string () const;

  Audio_note *audio_;

  
  static int const c0_pitch_ = 60;
  Byte dynamic_byte_;
};

/**
   Turn a note off
*/
class Midi_note_off : public Midi_note
{
public:
  Midi_note_off (Midi_note *);
  DECLARE_CLASSNAME(Midi_note_off);

  virtual string to_string () const;

  Midi_note *on_;
  Byte aftertouch_byte_;
};

class Midi_text : public Midi_item
{
public:
  enum Type
    {
      TEXT = 1, COPYRIGHT, TRACK_NAME, INSTRUMENT_NAME, LYRIC,
      MARKER, CUE_POINT
    };
  DECLARE_CLASSNAME(Midi_text);

  Midi_text (Audio_text *);

  virtual string to_string () const;

  Audio_text *audio_;
};

class Midi_dynamic : public Midi_channel_item
{
public:
  Midi_dynamic (Audio_dynamic *);
  DECLARE_CLASSNAME(Midi_dynamic);

  virtual string to_string () const;

  Audio_dynamic *audio_;
};

class Midi_piano_pedal : public Midi_channel_item
{
public:
  Midi_piano_pedal (Audio_piano_pedal *);
  DECLARE_CLASSNAME(Midi_piano_pedal);

  virtual string to_string () const;

  Audio_piano_pedal *audio_;
};

class Midi_tempo : public Midi_item
{
public:
  Midi_tempo (Audio_tempo *);
  DECLARE_CLASSNAME(Midi_tempo);

  virtual string to_string () const;

  Audio_tempo *audio_;
};

class Midi_track : public Midi_chunk
{
public:
  int number_;
  DECLARE_CLASSNAME(Midi_track);

  /*
    Compensate for starting grace notes.
  */
  vector<Midi_event*> events_;

  Midi_track ();
  ~Midi_track ();

  void add (int, Midi_item *midi);
  virtual string data_string () const;
};

#endif // MIDI_ITEM_HH
