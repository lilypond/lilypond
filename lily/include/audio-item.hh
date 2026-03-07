/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2026 Jan Nieuwenhuizen <janneke@gnu.org>

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

#ifndef AUDIO_ITEM_HH
#define AUDIO_ITEM_HH

#include "audio-element.hh"

#include "drul-array.hh"
#include "lily-guile.hh"
#include "moment.hh"
#include "piano-pedal.hh"
#include "pitch.hh"
#include "rational.hh"

#include <string>

class Audio_item : public Audio_element
{
public:
  Audio_column *audio_column_;
  int channel_;

  Audio_item ();
  Audio_column *get_column () const;

  virtual void render ();

private:
  Audio_item (Audio_item const &);
  Audio_item &operator= (Audio_item const &);
};

// Audio_span_dynamic is open at the end of the interval, so the volume
// grows/diminshes toward a target, but whether it reaches it depends on the
// next Audio_span_dynamic in the performance.  For example, a crescendo
// notated as mf < p is represented as [mf < x) [p ...) i.e. growth to some
// volume louder than mf followed by an abrupt change to p.
class Audio_span_dynamic final : public Audio_element
{
public:
  static const Real MINIMUM_VOLUME;
  static const Real MAXIMUM_VOLUME;
  static const Real DEFAULT_VOLUME;

private:
  Moment start_moment_;
  Real start_volume_;
  Real duration_; // = target moment - start moment
  Real gain_;     // = target volume - start volume

public:
  Moment get_start_moment () const { return start_moment_; }
  Real get_start_volume () const { return start_volume_; }
  Real get_duration () const { return duration_; }
  void set_end_moment (Moment);
  void set_volume (Real start, Real target);
  Real get_volume (Moment) const;
  Audio_span_dynamic (Moment mom, Real volume);
};

class Audio_key final : public Audio_item
{
public:
  Audio_key (int acc, bool major);

  int accidentals_;
  bool major_;
};

class Audio_instrument final : public Audio_item
{
public:
  explicit Audio_instrument (std::string instrument_string);

  std::string str_;
};

class Audio_note final : public Audio_item
{
public:
  Audio_note (Pitch p, Moment m, bool tie_event, Pitch transposition,
              int velocity);

  // with tieWaitForNote, there might be a skip between the tied notes!
  void tie_to (Audio_note *, Moment skip = 0);
  Audio_note *tie_head ();
  std::string to_string () const;

  Pitch pitch_;
  Moment length_mom_;
  Pitch transposing_;
  Audio_span_dynamic *dynamic_;
  int extra_velocity_;

  Audio_note *tied_;
  bool tie_event_;
};

class Audio_piano_pedal final : public Audio_item
{
public:
  Pedal_type type_;
  Direction dir_;
};

class Audio_text final : public Audio_item
{
public:
  enum Type : uint8_t
  {
    TEXT = 1,
    COPYRIGHT,
    TRACK_NAME,
    INSTRUMENT_NAME,
    LYRIC,
    MARKER,
    CUE_POINT
  };

  Audio_text (Audio_text::Type type, const std::string &text_string)
    : type_ (type),
      text_string_ (text_string)
  {
  }

  Audio_text (Audio_text::Type type, SCM markup)
    : Audio_text (type, markup_to_string (markup))
  {
  }

  static std::string markup_to_string (SCM markup);

  Type type_;
  std::string text_string_;
};

// Audio_span_tempo represents an interval of a piecewise-linear tempo
// function.  It is open at the end of the interval: the tempo grows or
// diminishes toward a target, but whether it reaches the target depends on the
// next Audio_span_tempo in the performance.
//
// This stores its life span measured within the full timeline of the score to
// handle skipTypesetting: we don't want tempo values computed for any
// subinterval to "take up the slack" when another subinterval is omitted from
// the output.
class Audio_span_tempo final : public Audio_element
{
public:
  static inline const Rational DEFAULT_WPM = 60 / 4;

private:
  Drul_array<Moment> life_span_; // a right-open interval
  Rational start_wpm_ = DEFAULT_WPM;
  Real duration_ = 0; // = target moment - start moment
  Real gain_ = 0;     // = target wpm - start wpm

public:
  Audio_span_tempo (Moment start, Rational initial_wpm);
  Moment get_start_moment () const { return life_span_.front (); }
  Rational get_start_wpm () const { return start_wpm_; }
  void set_end_moment (Moment);
  void set_end_wpm (Rational);
  // return the average tempo over the given right-open interval
  Rational calc_average_wpm (const Drul_array<Moment> &interval) const;
};

class Audio_time_signature final : public Audio_item
{
public:
  Audio_time_signature (Rational num, Rational den, int beat_base_clocks)
    : num_ (num),
      den_ (den),
      beat_base_clocks_ (beat_base_clocks)
  {
  }

  Rational num_;
  Rational den_;
  int beat_base_clocks_;
};

// Audio_tempo represents an instantaneous change in tempo, which is
// appropriate for MIDI, but it also stores the time of the next change to
// support computing a playback-time-preserving average tempo from a more
// descriptive model.
//
// This stores its life span measured within the full timeline of the score to
// handle skipTypesetting: we don't want tempo values computed for any
// subinterval of the model function to "take up the slack" when another
// subinterval is left unused.
class Audio_tempo final : public Audio_item
{
private:
  Drul_array<Moment> life_span_; // a right-open interval
  Audio_span_tempo *span_tempo_ = nullptr;

public:
  explicit Audio_tempo (Audio_span_tempo *span_tempo, Moment start)
    : life_span_ {start, Moment::infinity ()},
      span_tempo_ (span_tempo)
  {
  }

  Moment get_start_moment () const { return life_span_.front (); }
  Moment get_end_moment () const { return life_span_.back (); }

  bool has_end_moment () const
  {
    return life_span_.back () < Moment::infinity ();
  }

  void set_end_moment (Moment end) { life_span_.back () = end; }

  Rational calc_wpm () const
  {
    return span_tempo_->calc_average_wpm (life_span_);
  }
};

class Audio_control_change final : public Audio_item
{
public:
  Audio_control_change (int control, int value);

  int control_;
  int value_;
};

int moment_to_ticks (Moment);
Real moment_to_real (Moment);

#endif // AUDIO_ITEM_HH
