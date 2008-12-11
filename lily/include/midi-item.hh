/*
  midi-item.hh -- declare Midi items

  (c) 1997--2008 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef MIDI_ITEM_HH
#define MIDI_ITEM_HH

#include "audio-item.hh"
#include "std-vector.hh"

string int2midi_varint_string (int i);
  
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


  virtual string to_string () const = 0;
};

class Midi_channel_item : public Midi_item
{
public:
  int channel_;
  DECLARE_CLASSNAME(Midi_channel_item);
  Midi_channel_item ();
};

class Midi_duration : public Midi_item
{
public:
  Midi_duration (Real seconds_f);

  virtual string to_string () const;
  Real seconds_;
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

class Midi_note : public Midi_channel_item
{
public:
  Midi_note (Audio_note *);
  DECLARE_CLASSNAME(Midi_note);

  int get_semitone_pitch () const;
  int get_fine_tuning () const;
  virtual string to_string () const;

  Audio_note *audio_;

  
  static int const c0_pitch_ = 60;
  Byte dynamic_byte_;
};

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



#endif // MIDI_ITEM_HH
