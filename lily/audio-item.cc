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

#include "audio-item.hh"

#include "audio-column.hh"
#include "international.hh"
#include "lily-imports.hh"
#include "midi-item.hh"
#include "text-interface.hh"

using std::string;

Audio_instrument::Audio_instrument (string instrument_string)
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

string
Audio_note::to_string () const
{
  string s = "#<Audio_note pitch ";
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
      programming_error (_f ("end moment (%s) < start moment (%s)",
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
      programming_error (_f ("invalid start volume: %f", start));
      start = DEFAULT_VOLUME;
    }

  if (!(target >= 0))
    {
      programming_error (_f ("invalid target volume: %f", target));
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
        programming_error (_f ("asked to compute volume at %f for dynamic span "
                               "of duration %f starting at %s",
                               when, duration_,
                               start_moment_.to_string ().c_str ()));
      return start_volume_;
    }

  if (when >= duration_)
    {
      programming_error (_f ("asked to compute volume at +%f for dynamic span "
                             "of duration %f starting at %s",
                             when, duration_,
                             start_moment_.to_string ().c_str ()));
      return start_volume_ + gain_;
    }

  return start_volume_ + gain_ * (when / duration_);
}

Audio_tempo::Audio_tempo (int per_minute_4)
{
  per_minute_4_ = per_minute_4;
}

Audio_time_signature::Audio_time_signature (int beats, int one_beat,
                                            int base_moment_clocks)
{
  beats_ = beats;
  one_beat_ = one_beat;
  base_moment_clocks_ = base_moment_clocks;
}

std::string
Audio_text::markup_to_string (SCM markup)
{
  if (Text_interface::is_markup (markup))
    {
      markup = Lily::markup_to_string (markup);
    }

  return robust_scm2string (markup, "");
}

Audio_control_change::Audio_control_change (int control, int value)
  : control_ (control),
    value_ (value)
{
}
