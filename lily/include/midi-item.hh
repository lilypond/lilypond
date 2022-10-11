/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Jan Nieuwenhuizen <janneke@gnu.org>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef MIDI_ITEM_HH
#define MIDI_ITEM_HH

#include "audio-item.hh"

std::string int2midi_varint_string (int i);

/**
   Any piece of midi information.

   Maybe use base classes for RIFF files?
*/
class Midi_item
{
public:
  VIRTUAL_CLASS_NAME (Midi_item);
  Midi_item ();
  virtual ~Midi_item ();
  virtual char const *name () const;

  static Midi_item *get_midi (Audio_item *a);

  virtual std::string to_string () const = 0;
};

class Midi_end_of_track : public Midi_item
{
public:
  std::string to_string () const override
  {
    // the literal std::string's terminating null is part of the MIDI command
    return std::string ("\xff\x2f", 3);
  }
};

class Midi_channel_item : public Midi_item
{
public:
  virtual ~Midi_channel_item ();
  int channel_;
  OVERRIDE_CLASS_NAME (Midi_channel_item);
  Midi_channel_item (Audio_item *ai);
};

class Midi_duration : public Midi_item
{
public:
  Midi_duration (Real seconds_f);

  std::string to_string () const override;
  Real seconds_;
};

/**
   MIDI control change
*/
class Midi_control_change : public Midi_channel_item
{
public:
  OVERRIDE_CLASS_NAME (Midi_control_change);
  Midi_control_change (Audio_control_change *ai);
  virtual ~Midi_control_change ();
  std::string to_string () const override;

  Audio_control_change *audio_;
};

/**
   Change instrument event
*/
class Midi_instrument : public Midi_channel_item
{
public:
  Midi_instrument (Audio_instrument *);

  OVERRIDE_CLASS_NAME (Midi_instrument);
  std::string to_string () const override;

  Audio_instrument *audio_;
};

class Midi_key : public Midi_item
{
public:
  Midi_key (Audio_key *);
  OVERRIDE_CLASS_NAME (Midi_key);

  std::string to_string () const override;

  Audio_key *audio_;
};

class Midi_time_signature : public Midi_item
{
public:
  Midi_time_signature (Audio_time_signature *);
  OVERRIDE_CLASS_NAME (Midi_time_signature);

  std::string to_string () const override;

  Audio_time_signature *audio_;
  int clocks_per_1_;
};

class Midi_note : public Midi_channel_item
{
public:
  Midi_note (Audio_note *);
  OVERRIDE_CLASS_NAME (Midi_note);

  int get_semitone_pitch () const;
  int get_fine_tuning () const;
  std::string to_string () const override;

  Audio_note *audio_;

  static int const c0_pitch_ = 60;
  Byte dynamic_byte_;
};

class Midi_note_off : public Midi_note
{
public:
  Midi_note_off (Midi_note *);
  OVERRIDE_CLASS_NAME (Midi_note_off);

  std::string to_string () const override;

  Midi_note *on_;
  Byte aftertouch_byte_;
};

class Midi_text : public Midi_item
{
public:
  enum Type
  {
    TEXT = 1,
    COPYRIGHT,
    TRACK_NAME,
    INSTRUMENT_NAME,
    LYRIC,
    MARKER,
    CUE_POINT
  };
  OVERRIDE_CLASS_NAME (Midi_text);

  Midi_text (Audio_text *);

  std::string to_string () const override;

  Audio_text *audio_;
};

class Midi_piano_pedal : public Midi_channel_item
{
public:
  Midi_piano_pedal (Audio_piano_pedal *);
  OVERRIDE_CLASS_NAME (Midi_piano_pedal);

  std::string to_string () const override;

  Audio_piano_pedal *audio_;
};

class Midi_tempo : public Midi_item
{
public:
  Midi_tempo (Audio_tempo *);
  OVERRIDE_CLASS_NAME (Midi_tempo);

  std::string to_string () const override;

  Audio_tempo *audio_;
};

#endif // MIDI_ITEM_HH
