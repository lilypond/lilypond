/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2023 Jan Nieuwenhuizen <janneke@gnu.org>

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
#include "international.hh"
#include "lily-imports.hh"
#include "ly-scm-list.hh"
#include "performer.hh"
#include "protected-scm.hh"
#include "rational.hh"

#include <algorithm>

class Time_signature_performer final : public Performer
{
  Audio_time_signature *audio_ = nullptr;
  SCM last_time_fraction_ = SCM_BOOL_F;
  Stream_event *event_ = nullptr;

protected:
  SCM calc_common_beat (SCM beat_structure);
  void derived_mark () const override;
  void stop_translation_timestep ();
  void process_music ();

public:
  TRANSLATOR_DECLARATIONS (Time_signature_performer);
  void listen_reference_time_signature (Stream_event *);
};

void
Time_signature_performer::derived_mark () const
{
  scm_gc_mark (last_time_fraction_);
}

Time_signature_performer::Time_signature_performer (Context *c)
  : Performer (c)
{
}

void
Time_signature_performer::listen_reference_time_signature (Stream_event *ev)
{
  event_ = ev;
}

SCM
Time_signature_performer::calc_common_beat (SCM beat_structure)
{
  SCM common_beat = to_scm (0);

  for (SCM beat : as_ly_scm_list (beat_structure))
    {
      if (scm_is_integer (beat))
        {
          // This handles the usual cases, e.g. \time 6/8 with structure (3 3).
          common_beat = scm_gcd (common_beat, beat);
        }
      else
        {
          // When any beat has a fractional part, just use the beat base as the
          // metronome period.  Example: Time signature 2½/4, yielding beatBase
          // 1/4 and beatStructure (1 1 1/2).
          //
          // The closest approximation of the fraction that can be encoded in
          // MIDI is 5/8, but we distinguish it from 2½/4 by setting the
          // metronome period to a quarter note rather than an eighth note.
          return to_scm (1);
        }
    }

  return common_beat;
}

void
Time_signature_performer::process_music ()
{
  if (audio_)
    return;

  // TODO: For a strictly alternating time signature, it would likely be better
  // to insert a change for each component, though some users might prefer that
  // to be optional.  (Any components with subdivided numerators would still
  // need to have their numerators totaled.)
  SCM fr
    = Lily::time_signature_2_fraction (get_property (this, "timeSignature"));

  // If there is a \time event, we emit the time signature even if it
  // is the same as previously.  Midi may need it in some cases.
  // In particular:
  //
  // TODO: when a \partial command runs out, the time signature should
  // get reemitted at the start of the next bar in order to have MIDI
  // devices resynchronise to the meter.  \partial has no viable
  // representation in Midi.
  if (scm_is_pair (fr) && (event_ || !ly_is_equal (fr, last_time_fraction_)))
    {
      last_time_fraction_ = fr;
      const auto beat_base
        = from_scm (get_property (this, "beatBase"), Rational (1, 4));
      Rational beat_base_clocks = 96 * beat_base;
      SCM common_beat
        = calc_common_beat (get_property (this, "beatStructure"));
      if (is_scm<Rational> (common_beat)
          && scm_is_false (scm_zero_p (common_beat)))
        beat_base_clocks *= from_scm<Rational> (common_beat);
      if (beat_base_clocks.denominator () != 1
          || beat_base_clocks.numerator () < 1
          || beat_base_clocks.numerator () > 255)
        {
          const auto &msg = _ ("bad beatBase/beatStructure"
                               " for MIDI time signature");
          if (event_)
            event_->warning (msg);
          else
            warning (msg);

          // Use a quarter note, 24 MIDI clocks
          beat_base_clocks = 24;
        }

      audio_ = new Audio_time_signature (
        from_scm<Rational> (scm_car (fr)), // numerator
        from_scm<Rational> (scm_cdr (fr)), // denominator
        static_cast<int> (beat_base_clocks.numerator ()));
      Audio_element_info info (audio_, event_);
      announce_element (info);
    }
}

void
Time_signature_performer::stop_translation_timestep ()
{
  audio_ = nullptr;
  event_ = nullptr;
}

#include "translator.icc"

void
Time_signature_performer::boot ()
{
  ADD_LISTENER (reference_time_signature);
}

ADD_TRANSLATOR (Time_signature_performer,
                /* doc */
                R"(
Creates a MIDI time signature whenever @code{timeSignature} changes or
a @code{\time} command is issued.
                )",

                /* create */
                R"(

                )",

                /* read */
                R"(
timeSignature
                )",

                /* write */
                R"(

                )");
