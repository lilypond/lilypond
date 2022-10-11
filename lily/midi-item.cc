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

#include "midi-item.hh"

#include "audio-column.hh"
#include "duration.hh"
#include "international.hh"
#include "midi-stream.hh"
#include "misc.hh"
#include "piano-pedal.hh"
#include "program-option.hh"
#include "string-convert.hh"
#include "warn.hh"
#include "lily-imports.hh"

using std::string;

#define PITCH_WHEEL_CENTER 0x2000
#define PITCH_WHEEL_SEMITONE 0X1000

Midi_item *
Midi_item::get_midi (Audio_item *a)
{
  if (Audio_key *i = dynamic_cast<Audio_key *> (a))
    return new Midi_key (i);
  else if (Audio_instrument *i = dynamic_cast<Audio_instrument *> (a))
    return i->str_.length () ? new Midi_instrument (i) : 0;
  else if (Audio_note *i = dynamic_cast<Audio_note *> (a))
    return new Midi_note (i);
  else if (Audio_piano_pedal *i = dynamic_cast<Audio_piano_pedal *> (a))
    return new Midi_piano_pedal (i);
  else if (Audio_tempo *i = dynamic_cast<Audio_tempo *> (a))
    return new Midi_tempo (i);
  else if (Audio_time_signature *i = dynamic_cast<Audio_time_signature *> (a))
    return new Midi_time_signature (i);
  else if (Audio_text *i = dynamic_cast<Audio_text *> (a))
    return new Midi_text (i);
  else if (Audio_control_change *i = dynamic_cast<Audio_control_change *> (a))
    return new Midi_control_change (i);
  else
    assert (0);

  return 0;
}

Midi_duration::Midi_duration (Real seconds_f)
{
  seconds_ = seconds_f;
}

string
Midi_duration::to_string () const
{
  return string ("<duration: ") + std::to_string (seconds_) + ">";
}

Midi_instrument::Midi_instrument (Audio_instrument *a)
  : Midi_channel_item (a),
    audio_ (a)
{
  audio_->str_ = String_convert::to_lower (audio_->str_);
}

string
Midi_instrument::to_string () const
{
  Byte program_byte = 0;
  bool found = false;

  SCM program = Lily::midi_program (ly_symbol2scm (audio_->str_));
  found = (scm_is_true (program));
  if (found)
    program_byte = static_cast<Byte> (from_scm<int> (program));
  else
    warning (_f ("no such MIDI instrument: `%s'", audio_->str_.c_str ()));

  string str (1, static_cast<char> (
                   0xc0 + channel_)); //YIKES! FIXME : Should be track. -rz
  str += program_byte;
  return str;
}

Midi_item::Midi_item ()
{
}

Midi_channel_item::Midi_channel_item (Audio_item *ai)
  : channel_ (ai->channel_)
{
}

Midi_control_change::Midi_control_change (Audio_control_change *ai)
  : Midi_channel_item (ai),
    audio_ (ai)
{
}

Midi_item::~Midi_item ()
{
}

Midi_channel_item::~Midi_channel_item ()
{
}

Midi_control_change::~Midi_control_change ()
{
}

string
int2midi_varint_string (int i)
{
  int buffer = i & 0x7f;
  while ((i >>= 7) > 0)
    {
      buffer <<= 8;
      buffer |= 0x80;
      buffer += (i & 0x7f);
    }

  string str;
  while (1)
    {
      str += static_cast<char> (buffer);
      if (buffer & 0x80)
        buffer >>= 8;
      else
        break;
    }
  return str;
}

Midi_key::Midi_key (Audio_key *a)
  : audio_ (a)
{
}

string
Midi_key::to_string () const
{
  uint8_t str[] = {0xff, 0x59, 0x02, uint8_t (audio_->accidentals_),
                   uint8_t (audio_->major_ ? 0 : 1)};

  return string (reinterpret_cast<char *> (str), sizeof (str));
}

Midi_time_signature::Midi_time_signature (Audio_time_signature *a)
  : audio_ (a)
{
}

string
Midi_time_signature::to_string () const
{
  int num = abs (audio_->beats_);
  if (num > 255)
    {
      warning (_ ("Time signature with more than 255 beats.  Truncating"));
      num = 255;
    }

  int den = audio_->one_beat_;

  uint8_t out[] = {0xff,
                   0x58,
                   0x04,
                   uint8_t (num),
                   uint8_t (intlog2 (den)),
                   uint8_t (audio_->base_moment_clocks_),
                   8};
  return string (reinterpret_cast<char *> (out), sizeof (out));
}

Midi_note::Midi_note (Audio_note *a)
  : Midi_channel_item (a),
    audio_ (a),
    dynamic_byte_ (std::min (
      std::max (
        Byte ((a->dynamic_
                 ? a->dynamic_->get_volume (a->audio_column_->when ()) * 0x7f
                 : 0x5a)
              + a->extra_velocity_),
        Byte (0)),
      Byte (0x7f)))
{
}

int
Midi_note::get_fine_tuning () const
{
  Rational tune
    = (audio_->pitch_.tone_pitch () + audio_->transposing_.tone_pitch ())
      * Rational (2);
  tune -= Rational (get_semitone_pitch ());

  tune *= PITCH_WHEEL_SEMITONE;
  return static_cast<int> (static_cast<double> (tune));
}

int
Midi_note::get_semitone_pitch () const
{
  double tune = double (
    (audio_->pitch_.tone_pitch () + audio_->transposing_.tone_pitch ())
    * Rational (2));
  return int (rint (tune));
}

string
Midi_note::to_string () const
{
  const auto status_byte = static_cast<Byte> (0x90 + channel_);
  string str = "";

  // print warning if fine tuning was needed, HJJ
  if (get_fine_tuning () != 0)
    {
      int finetune = PITCH_WHEEL_CENTER + get_fine_tuning ();

      str += static_cast<char> (0xE0 + channel_);
      str += static_cast<char> (finetune & 0x7F);
      str += static_cast<char> (finetune >> 7);
      str += static_cast<char> (0x00);
    }

  str += status_byte;
  str += static_cast<char> (get_semitone_pitch () + c0_pitch_);
  str += dynamic_byte_;

  return str;
}

Midi_note_off::Midi_note_off (Midi_note *n)
  : Midi_note (n->audio_)
{
  on_ = n;
  channel_ = n->channel_;

  // use note_on with velocity=0 instead of note_off
  aftertouch_byte_ = 0;
}

string
Midi_note_off::to_string () const
{
  const auto status_byte = static_cast<Byte> (0x90 + channel_);

  string str (1, status_byte);
  str += static_cast<char> (get_semitone_pitch () + Midi_note::c0_pitch_);
  str += aftertouch_byte_;

  if (get_fine_tuning () != 0)
    {
      // Move pitch wheel back to the central position.
      str += static_cast<char> (0x00);
      str += static_cast<char> (0xE0 + channel_);
      str += static_cast<char> (PITCH_WHEEL_CENTER & 0x7F);
      str += static_cast<char> (PITCH_WHEEL_CENTER >> 7);
    }

  return str;
}

Midi_piano_pedal::Midi_piano_pedal (Audio_piano_pedal *a)
  : Midi_channel_item (a),
    audio_ (a)
{
}

string
Midi_piano_pedal::to_string () const
{
  const auto status_byte = static_cast<Byte> (0xB0 + channel_);
  string str (1, status_byte);

  if (audio_->type_ == SOSTENUTO)
    str += static_cast<char> (0x42);
  else if (audio_->type_ == SUSTAIN)
    str += static_cast<char> (0x40);
  else if (audio_->type_ == UNA_CORDA)
    str += static_cast<char> (0x43);

  const char pedal = (audio_->dir_ == LEFT) * 0x7f;
  str += pedal;
  return str;
}

Midi_tempo::Midi_tempo (Audio_tempo *a)
  : audio_ (a)
{
}

string
Midi_tempo::to_string () const
{
  uint32_t useconds_per_4 = 60 * 1000000 / audio_->per_minute_4_;
  uint8_t out[] = {0xff, 0x51, 0x03};
  return string (reinterpret_cast<char *> (out), sizeof (out))
         + String_convert::be_u24 (useconds_per_4);
}

Midi_text::Midi_text (Audio_text *a)
  : audio_ (a)
{
}

string
Midi_text::to_string () const
{
  uint8_t text_code[] = {0xff, audio_->type_};
  string str (reinterpret_cast<char *> (text_code), sizeof (text_code));
  str += int2midi_varint_string (int (audio_->text_string_.length ()));
  str += audio_->text_string_;
  return str;
}

string
Midi_control_change::to_string () const
{
  const auto status_byte = static_cast<Byte> (0xB0 + channel_);
  string str (1, status_byte);
  str += static_cast<char> (audio_->control_);
  str += static_cast<char> (audio_->value_);
  return str;
}

char const *
Midi_item::name () const
{
  return class_name ();
}
