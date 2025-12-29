/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2023 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "engraver-group.hh"

#include "item.hh"
#include "international.hh"
#include "lily-guile.hh"
#include "lily-imports.hh"
#include "misc.hh"
#include "moment.hh"
#include "stream-event.hh"
#include "warn.hh"

#include "translator.icc"

/**
   generate time_signatures.
*/
class Time_signature_engraver final : public Engraver
{
  Item *time_signature_ = nullptr;
  SCM last_spec_ = SCM_EOL;
  Stream_event *event_ = nullptr;
  Stream_event *local_event_ = nullptr;

protected:
  void derived_mark () const override;
  void stop_translation_timestep ();
  void process_music ();

public:
  TRANSLATOR_DECLARATIONS (Time_signature_engraver);
  void listen_polymetric_time_signature (Stream_event *);
  void listen_reference_time_signature (Stream_event *);
};

void
Time_signature_engraver::derived_mark () const
{
  scm_gc_mark (last_spec_);
}

Time_signature_engraver::Time_signature_engraver (Context *c)
  : Engraver (c)
{
}

void
Time_signature_engraver::listen_polymetric_time_signature (Stream_event *ev)
{
  // When this engraver is operating in Staff (as usual), an event from the
  // following code is not pertinent:
  //
  //     \context Voice \polymetric \time ...
  //
  // We don't promote this use, so we don't expect to see it routinely, but
  // users could discover it and use it to set voice-specific beaming without
  // changing the printed time signature.
  //
  // Ignoring events that do not correspond to the value of timeSignature in
  // this context should mitigate the problem.  Is there any better solution?
  SCM event_val = get_property (ev, "time-signature");
  SCM context_val = get_property (this, "timeSignature");
  if (ly_is_equal (context_val, event_val))
    {
      local_event_ = ev;
    }
}

void
Time_signature_engraver::listen_reference_time_signature (Stream_event *ev)
{
  event_ = ev;
}

void
Time_signature_engraver::process_music ()
{
  if (time_signature_)
    return;

  SCM spec = get_property (this, "timeSignature");
  if (!scm_is_eq (last_spec_, spec)
      && (scm_is_pair (spec) || scm_is_false (spec)))
    {
      auto ev = local_event_ ? local_event_ : event_;
      time_signature_ = make_item ("TimeSignature", ev ? to_scm (ev) : SCM_EOL);

      // check value before setting to respect overrides
      SCM tsig_sym = ly_symbol2scm ("time-signature");
      if (scm_is_null (get_property (time_signature_, tsig_sym)))
        {
          set_property (time_signature_, tsig_sym, spec);
        }

      if (scm_is_null (last_spec_))
        {
          set_property (time_signature_, "break-visibility",
                        get_property (this, "initialTimeSignatureVisibility"));
        }

      last_spec_ = spec;
    }
}

void
Time_signature_engraver::stop_translation_timestep ()
{
  if (time_signature_ && (event_ || local_event_))
    {
      // Avoid measure_position (context ()) here because its result is
      // normalized to be >= 0 always.
      Moment *mp = unsmob<Moment> (get_property (this, "measurePosition"));
      if (mp && (mp->main_part_ > Rational (0))
          && !from_scm<bool> (get_property (this, "partialBusy")))
        time_signature_->warning (
          _ ("mid-measure time signature without \\partial"));
    }

  time_signature_ = nullptr;
  event_ = nullptr;
  local_event_ = nullptr;
}

void
Time_signature_engraver::boot ()
{
  ADD_LISTENER (polymetric_time_signature);
  ADD_LISTENER (reference_time_signature);
}

ADD_TRANSLATOR (Time_signature_engraver,
                /* doc */
                R"(
Create a @iref{TimeSignature} whenever @code{timeSignature} changes.
                )",

                /* create */
                R"(
TimeSignature
                )",

                /* read */
                R"(
initialTimeSignatureVisibility
partialBusy
timeSignature
                )",

                /* write */
                R"(

                )");
