/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2026 Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "audio-item.hh"

#include "drul-array.hh"
#include "audio-column.hh"
#include "international.hh"
#include "lily-imports.hh"
#include "midi-item.hh"
#include "text-interface.hh"
#include "warn.hh"

Audio_instrument::Audio_instrument (std::string instrument_string)
{
  str_ = instrument_string;
}

void
Audio_item::render ()
{
}

Audio_column *
Audio_item::get_column () const
{
  return audio_column_;
}

Audio_item::Audio_item ()
  : audio_column_ (0),
    channel_ (0)
{
}

Audio_note::Audio_note (Pitch p, Moment m, bool tie_event, Pitch transposing,
                        int velocity)
  : pitch_ (p),
    length_mom_ (m),
    transposing_ (transposing),
    dynamic_ (0),
    extra_velocity_ (velocity),
    tied_ (0),
    tie_event_ (tie_event)
{
}

void
Audio_note::tie_to (Audio_note *t, Moment skip)
{
  tied_ = t;
  Audio_note *first = tie_head ();
  // Add the skip to the tied note and the length of the appended note
  // to the full duration of the tie...
  first->length_mom_ += skip + length_mom_;
  length_mom_ = 0;
}

Audio_note *
Audio_note::tie_head ()
{
  Audio_note *first = this;
  while (first->tied_)
    first = first->tied_;
  return first;
}

std::string
Audio_note::to_string () const
{
  std::string s = "#<Audio_note pitch ";
  s += pitch_.to_string ();
  s += " len ";
  s += length_mom_.to_string ();
  if (tied_)
    {
      s += " tied to " + tied_->to_string ();
    }
  if (tie_event_)
    {
      s += " tie_event";
    }
  s += ">";
  return s;
}

Audio_key::Audio_key (int acc, bool major)
{
  accidentals_ = acc;
  major_ = major;
}

const Real Audio_span_dynamic::MINIMUM_VOLUME = 0.0;
const Real Audio_span_dynamic::MAXIMUM_VOLUME = 1.0;
const Real Audio_span_dynamic::DEFAULT_VOLUME = 90.0 / 127.0;

Audio_span_dynamic::Audio_span_dynamic (Moment mom, Real volume)
  : start_moment_ (mom),
    duration_ (0)
{
  set_volume (volume, volume);
}

Real
moment_to_real (Moment m)
{
  return static_cast<Real> (m.main_part_ + Rational (9, 40) * m.grace_part_);
}

int
moment_to_ticks (Moment m)
{
  return int (moment_to_real (m) * 384 * 4);
}

void
Audio_span_dynamic::set_end_moment (Moment mom)
{
  if (mom < start_moment_)
    {
      programming_error (to_string_f ("end moment (%s) < start moment (%s)",
                                      mom.to_string ().c_str (),
                                      start_moment_.to_string ().c_str ()));
      mom = start_moment_;
    }

  duration_ = moment_to_real (mom - start_moment_);
}

void
Audio_span_dynamic::set_volume (Real start, Real target)
{
  if (!(start >= 0))
    {
      programming_error (to_string_f ("invalid start volume: %f", start));
      start = DEFAULT_VOLUME;
    }

  if (!(target >= 0))
    {
      programming_error (to_string_f ("invalid target volume: %f", target));
      target = start;
    }

  start_volume_ = start;
  gain_ = target - start;
}

Real
Audio_span_dynamic::get_volume (Moment mom) const
{
  const Real when = moment_to_real (mom - start_moment_);

  if (when <= 0)
    {
      if (when < 0)
        {
          programming_error (to_string_f (
            "asked to compute volume at %f for "
            "dynamic span of duration %f starting at %s",
            when, duration_, start_moment_.to_string ().c_str ()));
        }
      return start_volume_;
    }

  if (when >= duration_)
    {
      programming_error (
        to_string_f ("asked to compute volume at +%f for "
                     "dynamic span of duration %f starting at %s",
                     when, duration_, start_moment_.to_string ().c_str ()));
      return start_volume_ + gain_;
    }

  return start_volume_ + gain_ * (when / duration_);
}

Audio_span_tempo::Audio_span_tempo (Moment mom, Rational wpm)
  : life_span_ {mom, mom},
    start_wpm_ (wpm)
{
  if (!isfinite (start_wpm_) || (start_wpm_ < 0))
    {
      programming_error (to_string_f ("invalid start tempo: %s",
                                      to_string (start_wpm_).c_str ()));
      start_wpm_ = DEFAULT_WPM;
    }
}

void
Audio_span_tempo::set_end_moment (Moment end_mom)
{
  const auto &start_mom = life_span_.front ();
  if (end_mom < start_mom)
    {
      programming_error (to_string_f ("end moment (%s) < start moment (%s)",
                                      to_string (end_mom).c_str (),
                                      to_string (start_mom).c_str ()));
      end_mom = start_mom;
    }

  life_span_.back () = end_mom;
  duration_ = moment_to_real (end_mom - start_mom);
}

void
Audio_span_tempo::set_end_wpm (Rational target)
{
  gain_ = static_cast<Real> (target) - static_cast<Real> (start_wpm_);
}

Rational
Audio_span_tempo::calc_average_wpm (
  const Drul_array<Moment> &interval_mom) const
{
  // In a MIDI file, tempo is a piecewise constant function.  The interface of
  // this function is tailored to that: the caller chooses the intervals and
  // this function computes the average tempo for a given interval.
  //
  // It is highly desirable for the playback time not to depend on the caller's
  // choice of intervals.

  if (!(life_span_.front () <= interval_mom.front ())
      || !(interval_mom.back () <= life_span_.back ()))
    {
      programming_error (
        to_string_f ("asked to compute tempo over [%s, %s), "
                     "which is outside tempo span [%s, %s)",
                     to_string (interval_mom.front ()).c_str (),
                     to_string (interval_mom.back ()).c_str (),
                     to_string (life_span_.front ()).c_str (),
                     to_string (life_span_.back ()).c_str ()));
      return start_wpm_;
    }

  // Convert the Moment endpoints to scalars relative to the life span.
  const auto start
    = moment_to_real (interval_mom.front () - life_span_.front ());
  const auto end = moment_to_real (interval_mom.back () - life_span_.front ());
  if (!(0 <= start) || !(end <= duration_) || !(start < end))
    {
      programming_error (
        to_string_f ("asked to compute tempo over [%f, %f) relative to "
                     "tempo span [%s, %s), which has duration %f",
                     start, end, to_string (life_span_.front ()).c_str (),
                     to_string (life_span_.back ()).c_str (), duration_));
      return start_wpm_;
    }

  // We choose to model gradual tempo changes as linear in tempo.  It seems
  // likely that there are better models of how people perform gradual tempo
  // changes.
  auto instant_wpm = [&] (Real when) {
    return static_cast<Real> (start_wpm_) + gain_ * (when / duration_);
  };
  const auto tl = instant_wpm (start);
  const auto tr = instant_wpm (end);

  // Given the linear model, the logarithmic mean tempo yields equal playback
  // time for the piecewise constant function in MIDI.
  const auto num = tr - tl;
  const auto den = log (tr / tl); // == log (tr) - log (tl)
  if ((num == 0) || (den == 0))
    {
      return start_wpm_;
    }
  return Rational (num / den);
}

std::string
Audio_text::markup_to_string (SCM markup)
{
  if (Text_interface::is_markup (markup))
    {
      markup = Lily::markup_to_string (markup);
    }

  return from_scm (markup, "");
}

Audio_control_change::Audio_control_change (int control, int value)
  : control_ (control),
    value_ (value)
{
}
