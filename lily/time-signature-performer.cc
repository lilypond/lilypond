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
#include "international.hh"
#include "performer.hh"
#include "protected-scm.hh"

class Time_signature_performer : public Performer
{
  Audio_time_signature *audio_ = nullptr;
  SCM last_time_fraction_ = SCM_BOOL_F;
  Stream_event *event_ = nullptr;

protected:
  void derived_mark () const override;
  void stop_translation_timestep ();
  void process_music ();

public:
  TRANSLATOR_DECLARATIONS (Time_signature_performer);
  void listen_time_signature (Stream_event *);
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
Time_signature_performer::listen_time_signature (Stream_event *ev)
{
  event_ = ev;
}

void
Time_signature_performer::process_music ()
{
  if (audio_)
    return;

  SCM fr = get_property (this, "timeSignatureFraction");
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
      int b = from_scm<int> (scm_car (fr));
      int o = from_scm<int> (scm_cdr (fr));
      static const Moment quarter = Moment (Rational (1, 4));
      const auto base_moment
        = from_scm (get_property (this, "baseMoment"), quarter);
      Rational base_moment_clocks = 96 * base_moment.main_part_;
      SCM common_beat = SCM_INUM0;
      for (SCM p = get_property (this, "beatStructure"); scm_is_pair (p);
           p = scm_cdr (p))
        common_beat = scm_gcd (common_beat, scm_car (p));
      if (is_scm<Rational> (common_beat)
          && scm_is_false (scm_zero_p (common_beat)))
        base_moment_clocks *= from_scm<Rational> (common_beat);
      if (base_moment_clocks.denominator () != 1
          || base_moment_clocks.numerator () < 1
          || base_moment_clocks.numerator () > 255)
        {
          const auto &msg = _ ("bad baseMoment/beatStructure"
                               " for MIDI time signature");
          if (event_)
            event_->warning (msg);
          else
            warning (msg);

          // Use a quarter note, 24 MIDI clocks
          base_moment_clocks = 24;
        }

      audio_ = new Audio_time_signature (
        b, o, static_cast<int> (base_moment_clocks.numerator ()));
      Audio_element_info info (audio_, 0);
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
  ADD_LISTENER (time_signature);
}

ADD_TRANSLATOR (Time_signature_performer,
                /* doc */
                R"(
Creates a MIDI time signature whenever @code{timeSignatureFraction} changes or
a @code{\time} command is issued.
                )",

                /* create */
                R"(

                )",

                /* read */
                R"(
timeSignatureFraction
                )",

                /* write */
                R"(

                )");
