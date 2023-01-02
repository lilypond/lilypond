/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "timing-translator.hh"

#include "global-context.hh"
#include "international.hh"
#include "lily-imports.hh"
#include "moment.hh"
#include "translator-group.hh"
#include "warn.hh"

void
Timing_translator::listen_alternative (Stream_event *ev)
{
  // Use alternative bar numbers for the outermost volta brackets.
  const auto depth = from_scm<size_t> (get_property (ev, "volta-depth"), 0);
  if (depth != 1)
    return;

  // It is common to have the same repeat structure in multiple voices, so we
  // ignore simultaneous events; but it might not be a bad thing to add some
  // consistency checks here if they could catch some kinds of user error.
  if (alt_event_)
    return;

  alt_event_ = ev;
}

void
Timing_translator::listen_bar (Stream_event *ev)
{
  // To mimic the previous implementation, we always set whichBar.
  set_property (context (), "whichBar", get_property (ev, "bar-type"));
}

void
Timing_translator::listen_fine (Stream_event *ev)
{
  assign_event_once (fine_event_, ev);
}

void
Timing_translator::process_music ()
{
  if (alt_event_)
    {
      // Which alternative is this?
      // LEFT: starting the first alternative
      // CENTER: starting a latter alternative
      // RIGHT: ending the last alternative
      const auto alt_dir
        = from_scm (get_property (alt_event_, "alternative-dir"), CENTER);

      switch (alt_dir)
        {
        case LEFT:
          {
            // Use a consistent numbering algorithm for the full set of
            // alternatives by changing it only on the first alternative.
            alt_reset_enabled_ = scm_is_symbol (
              get_property (this, "alternativeNumberingStyle"));
            if (alt_reset_enabled_)
              {
                alt_starting_bar_number_
                  = from_scm (get_property (this, "currentBarNumber"), 0);
              }

            alt_number_ = 1;
            break;
          }

        case CENTER:
          if (alt_reset_enabled_)
            {
              set_property (context (), "currentBarNumber",
                            to_scm (alt_starting_bar_number_));
            }
          alt_number_ += alt_number_increment_;
          break;

        default:
          assert (false);
          // fallthrough

        case RIGHT:
          alt_number_ = 0;
          break;
        }

      if (alt_dir < RIGHT)
        {
          set_property (context (), "alternativeNumber", to_scm (alt_number_));
          // will need to add this on the next alternative
          alt_number_increment_
            = scm_ilength (get_property (alt_event_, "volta-numbers"));
        }
      else
        {
          set_property (context (), "alternativeNumber", SCM_EOL);
        }
    }
}

void
Timing_translator::stop_translation_timestep ()
{
  if (from_scm<bool> (get_property (this, "timing"))
      && !from_scm<bool> (get_property (this, "skipBars")))
    {
      const auto barleft
        = Moment (measure_length (context ())) - measure_position (context ());

      if (barleft > Moment (0))
        {
          auto nextmom = now_mom () + barleft;
          nextmom.grace_part_ = 0;
          find_global_context ()->add_moment_to_process (nextmom);
        }
    }

  set_property (context (), "measureStartNow", SCM_EOL);

  alt_event_ = nullptr;
}

void
Timing_translator::initialize ()
{
  SCM timing_context_name = ly_symbol2scm ("Timing");
  auto *timing = find_context_above (context (), timing_context_name);
  if (timing != context ())
    {
      context ()->add_alias (timing_context_name);

      if (!timing)
        {
          programming_error ("Can't find Timing context template");
          timing = context ();
        }
    }

  SCM barnumber = get_property (timing, "currentBarNumber");
  if (!scm_is_integer (barnumber))
    barnumber = to_scm (1);
  set_property (context (), "currentBarNumber", barnumber);
  set_property (context (), "internalBarNumber", barnumber);

  SCM timeSignatureFraction = get_property (timing, "timeSignatureFraction");

  if (!scm_is_pair (timeSignatureFraction))
    {
      programming_error ("missing timeSignatureFraction");
      timeSignatureFraction = scm_cons (to_scm (4), to_scm (4));
    }
  set_property (context (), "timeSignatureFraction", timeSignatureFraction);

  SCM measureLength = get_property (timing, "measureLength");

  if (!unsmob<Moment> (measureLength))
    {
      measureLength = Moment (from_scm<Rational> (
                                scm_divide (scm_car (timeSignatureFraction),
                                            scm_cdr (timeSignatureFraction))))
                        .smobbed_copy ();
    }
  set_property (context (), "measureLength", measureLength);
  set_property (context (), "measurePosition", now_mom ().smobbed_copy ());

  SCM timeSignatureSettings = get_property (timing, "timeSignatureSettings");
  if (!scm_is_pair (timeSignatureSettings))
    {
      programming_error ("missing timeSignatureSettings");
      timeSignatureSettings = Lily::default_time_signature_settings;
    }
  set_property (context (), "timeSignatureSettings", timeSignatureSettings);

  SCM beamExceptions = get_property (timing, "beamExceptions");
  if (!scm_is_pair (beamExceptions))
    {
      beamExceptions
        = Lily::beam_exceptions (timeSignatureFraction, timeSignatureSettings);
    }
  set_property (context (), "beamExceptions", beamExceptions);

  SCM baseMoment = get_property (timing, "baseMoment");
  if (!unsmob<Moment> (baseMoment))
    {
      baseMoment = Moment (from_scm<Rational> (Lily::base_length (
                             timeSignatureFraction, timeSignatureSettings)))
                     .smobbed_copy ();
    }
  set_property (context (), "baseMoment", baseMoment);

  SCM beatStructure = get_property (timing, "beatStructure");
  if (!scm_is_pair (beatStructure))
    {
      beatStructure = Lily::beat_structure (
        to_scm (unsmob<Moment> (baseMoment)->main_part_), timeSignatureFraction,
        timeSignatureSettings);
    }
  set_property (context (), "beatStructure", beatStructure);

  set_property (context (), "beamHalfMeasure",
                get_property (timing, "beamHalfMeasure"));

  set_property (context (), "autoBeaming",
                get_property (timing, "autoBeaming"));
}

Timing_translator::Timing_translator (Context *c)
  : Translator (c)
{
}

void
Timing_translator::start_translation_timestep ()
{
  Global_context *global = find_global_context ();

  const auto now = global->now_mom ();
  auto dt = now - global->previous_moment ();
  if (dt < Moment (0))
    {
      programming_error ("moving backwards in time");
      dt = 0;
    }
  else if (isinf (dt.main_part_))
    {
      programming_error ("moving infinitely to future");
      dt = 0;
    }

  if (!dt)
    return;

  if (fine_event_)
    {
      if (!from_scm<bool> (get_property (fine_event_, "fine-folded")))
        fine_event_->warning (_ ("found music after \\fine"));

      fine_event_ = nullptr;
    }

  Rational mp;
  {
    auto mom = from_scm (get_property (this, "measurePosition"), now);
    mp = mom.main_part_;
  }

  if (from_scm<bool> (get_property (this, "timing")))
    {
      const auto len = measure_length (context ());

      mp += dt.main_part_;

      if (mp >= len)
        {
          auto cbn = from_scm (get_property (this, "currentBarNumber"), 0);
          auto ibn = from_scm (get_property (this, "internalBarNumber"), 0);

          do
            {
              mp -= len;
              cbn++;
              ibn++;
            }
          while (mp >= len);

          set_property (context (), "currentBarNumber", to_scm (cbn));
          set_property (context (), "internalBarNumber", to_scm (ibn));
          measure_started_ = false;
        }

      if (!measure_started_ && !mp && dt.main_part_)
        {
          // We have arrived at zero (as opposed to revisiting it).
          measure_started_ = true;
          set_property (context (), "measureStartNow", SCM_BOOL_T);
        }
    }

  // Because "timing" can be switched on and off asynchronously with
  // graces, measurePosition might get into strange settings of
  // grace_part_.  It does not actually make sense to have it diverge
  // from the main timing.  Updating the grace part outside of the
  // actual check for "timing" looks strange and will lead to changes
  // of grace_part_ even when timing is off.  However, when timing is
  // switched back on again, this will generally happen in an override
  // that does _not_ in itself advance current_moment.  So the whole
  // timing advance logic will only get triggered while "timing" is
  // still of.  Maybe we should keep measurePosition.grace_part_
  // constantly at zero anyway?

  set_property (context (), "measurePosition",
                Moment (mp, now.grace_part_).smobbed_copy ());

  // We set whichBar at each timestep because the user manuals used to suggest
  // using \set Timing.whichBar = ... rather than \once \set Timing.whichBar =
  // ..., so we might need to erase the user's value from the previous
  // timestep.
  //
  // It might be nice to set up a convert-ly rule to change user code to use
  // \bar and redocument whichBar as internal.
  set_property (context (), "whichBar", SCM_EOL);
}

#include "translator.icc"

void
Timing_translator::boot ()
{
  ADD_LISTENER (alternative);
  ADD_LISTENER (bar);
  ADD_LISTENER (fine);
}

ADD_TRANSLATOR (Timing_translator,
                /* doc */
                R"(
This engraver adds the alias @code{Timing} to its containing context.
Responsible for synchronizing timing information from staves.  Normally in
@code{Score}.  In order to create polyrhythmic music, this engraver should be
removed from @code{Score} and placed in @code{Staff}.
                )",

                /* create */
                R"(

                )",

                /* read */
                R"(
alternativeNumberingStyle
baseMoment
currentBarNumber
internalBarNumber
measureLength
measurePosition
timeSignatureFraction
                )",

                /* write */
                R"(
alternativeNumber
baseMoment
currentBarNumber
internalBarNumber
measureLength
measurePosition
measureStartNow
timeSignatureFraction
                )");
