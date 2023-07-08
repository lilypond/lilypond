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
Timing_translator::listen_bar_check (Stream_event *ev)
{
  // Simultaneous bar checks are normal.  Since we're going to issue at most one
  // warning, we only need to handle one of the events.
  if (!bar_check_event_)
    bar_check_event_ = ev;

  // barCheckSynchronize is implemented here so that changes to timing
  // properties occur before any translator's pre_process_music () is called.
  //
  // TODO: barCheckSynchronize is untested.
  // TODO: Formally deprecate barCheckSynchronize.
  const auto &now = now_mom ();
  if ((now.main_part_ != measure_start_mom_.main_part_)
      && from_scm<bool> (get_property (this, "barCheckSynchronize")))
    {
      SCM mp_sym = ly_symbol2scm ("measurePosition");
      const auto mp = from_scm (get_property (context (), mp_sym), Moment ());
      if (mp.main_part_
          && !from_scm<bool> (get_property (this, "ignoreBarChecks")))
        {
          set_property (context (), mp_sym, to_scm (Moment ()));
          set_property (context (), "measureStartNow", SCM_BOOL_T);
          // The old Bar_check_iterator used to warn regardless (once).
        }
    }
}

void
Timing_translator::listen_fine (Stream_event *ev)
{
  assign_event_once (fine_event_, ev);
}

void
Timing_translator::listen_partial (Stream_event *ev)
{
  auto *const dur = unsmob<Duration> (get_property (ev, "duration"));
  if (!dur)
    {
      ev->programming_error ("invalid duration in \\partial");
      return;
    }

  if (!assign_event_once (partial_event_, ev))
    return;

  if (context ()->init_mom () < now_mom ()) // in mid piece
    {
      set_property (context (), "partialBusy", SCM_BOOL_T);
    }
  else
    {
      // It would be consistent with the mid-piece behavior to refuse to adjust
      // measurePosition when measureLength is infinite, and it would help
      // defend consumers that might try to normalize a negative measurePosition
      // using measureLength as a modulus; however, even if we caught it here,
      // measureLength could be changed immediately afterward, so there is no
      // point in trying.  We will detect it and warn in pre_process_music().
      const auto length = dur->get_length ();
      auto mp = from_scm (get_property (this, "measurePosition"), Moment (0));
      mp.main_part_ = -length;
      set_property (context (), "measurePosition", mp.smobbed_copy ());
      if (mp)
        {
          set_property (context (), "measureStartNow", SCM_EOL);
        }
    }
}

void
Timing_translator::pre_process_music ()
{
  // We can't assume that measurePosition and measureStartNow have the same
  // values as at the start of the timestep.  A partial-event or
  // Alternative_sequence_iterator might have changed them.
  //
  // TODO: Using alternativeRestores for timing properties is an imperfect
  // solution.  See comments in Alternative_sequence_iterator.  Consider making
  // Timing_translator responsible for timing properties.
  //
  // We don't pay attention to which alternative a bar check is in.  If the
  // previous alternative ends at a measure boundary or the next alternative
  // begins at a measure boundary, we accept it.  False negatives may result.
  // We could probably eliminate most false negatives if bar-check events told
  // their place in the repeat structure.  The question is whether the value to
  // the user is worth complicating the internals.
  //
  // We ignore differences in grace part.  Simultaneous sequences may include
  // different amounts of grace time, which makes iteration interesting (see
  // issue #34).  The measure starts with the earliest grace note, but we don't
  // want to fail later bar checks when the only difference is grace notes.
  const auto &now = now_mom ();
  bool bar_check_ok = (now.main_part_ == measure_start_mom_.main_part_);
  if (from_scm<bool> (get_property (context (), "measureStartNow")))
    {
      measure_start_mom_ = now;
      bar_check_ok = true;
    }

  // One mistake offsets all subsequent bar checks by the same amount.  It is
  // noisy to warn in every measure until the next mistake or change in timing,
  // so we suppress further warnings.
  if (!bar_check_ok && bar_check_event_ && !warned_for_bar_check_
      && !from_scm<bool> (get_property (this, "ignoreBarChecks")))
    {
      const auto mp
        = from_scm (get_property (this, "measurePosition"), Moment ());
      bar_check_event_->warning (
        _f ("bar check failed at: %s", to_string (mp)));
      warned_for_bar_check_ = true;
    }

  if (partial_event_ && (context ()->init_mom () == now)) // start of piece
    {
      if (!isfinite (measure_length (context ())))
        {
          // This is the same warning as for \partial in mid piece.
          // See listen_partial() for more information.
          partial_event_->warning (
            _ ("cannot calculate a finite measurePosition from an infinite"
               " measureLength"));
        }
    }
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
            SCM style = get_property (this, "alternativeNumberingStyle");
            alt_reset_enabled_
              = scm_is_eq (style, ly_symbol2scm ("numbers"))
                || scm_is_eq (style, ly_symbol2scm ("numbers-with-letters"));
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
  // Defer setting measurePosition (etc.) until the beginning of the next
  // timestep so that translators can read a consisent value from the beginning
  // of pre_process_music() to the end of stop_translation_timestep().
  carried_measure_position_
    = from_scm (get_property (this, "measurePosition"), Moment (0)).main_part_;

  // Handle \partial in mid piece.
  if (partial_event_ && (context ()->init_mom () < now_mom ()))
    {
      auto *const dur
        = unsmob<Duration> (get_property (partial_event_, "duration"));
      if (dur) // paranoia: listen_partial() should have rejected this event
        {
          const auto mlen = measure_length (context ());
          if (isfinite (mlen))
            {
              carried_measure_position_ = mlen - dur->get_length ();
            }
          else
            {
              partial_event_->warning (
                _ ("cannot calculate a finite measurePosition from an infinite"
                   " measureLength"));
              // We could try to handle this more gracefully by setting a
              // calculated measureLength here, but there might be side effects
              // that are hard to foresee, so we don't bother.
            }
        }
    }

  if (from_scm<bool> (get_property (this, "timing"))
      && !from_scm<bool> (get_property (this, "skipBars")))
    {
      Rational barleft = 0;
      if (carried_measure_position_ < 0)
        barleft = -carried_measure_position_;
      else
        barleft = measure_length (context ()) - carried_measure_position_;

      if ((barleft > 0) && isfinite (barleft))
        {
          auto nextmom = Moment (now_mom ().main_part_ + barleft);
          find_global_context ()->add_moment_to_process (nextmom);
        }
    }

  alt_event_ = nullptr;
  bar_check_event_ = nullptr;
}

void
Timing_translator::connect_to_context ()
{
  SCM timing_context_name = ly_symbol2scm ("Timing");
  if (!context ()->is_alias (timing_context_name))
    {
      context ()->add_alias (timing_context_name);
    }
}

void
Timing_translator::initialize ()
{
  // Sanity check: In the polymeter use case, when we are not in the top
  // user-accessible context, we expect that a context above (typically Score)
  // has been initialized as Timing so that we can copy its current property
  // values for our initial values here.
  if (auto *const parent = context ()->get_parent ())
    {
      if (parent->is_accessible_to_user ()
          && !find_context_above (parent, ly_symbol2scm ("Timing")))
        {
          programming_error ("Can't find Timing context template");
        }
    }

  SCM barnumber = get_property (this, "currentBarNumber");
  if (!scm_is_integer (barnumber))
    barnumber = to_scm (1);
  set_property (context (), "currentBarNumber", barnumber);
  set_property (context (), "internalBarNumber", barnumber);

  SCM timeSignatureFraction = get_property (this, "timeSignatureFraction");

  if (!scm_is_pair (timeSignatureFraction)
      && !scm_is_false (timeSignatureFraction))
    {
      programming_error ("missing timeSignatureFraction");
      timeSignatureFraction = SCM_BOOL_F;
    }
  set_property (context (), "timeSignatureFraction", timeSignatureFraction);

  SCM measureLength = get_property (this, "measureLength");

  if (!unsmob<Moment> (measureLength))
    {
      measureLength = Lily::calc_measure_length (timeSignatureFraction);
    }
  set_property (context (), "measureLength", measureLength);
  {
    const auto mp = Moment (0, now_mom ().grace_part_);
    set_property (context (), "measurePosition", mp.smobbed_copy ());
  }
  set_property (context (), "measureStartNow", SCM_BOOL_T);

  SCM timeSignatureSettings = get_property (this, "timeSignatureSettings");
  if (!scm_is_pair (timeSignatureSettings))
    {
      programming_error ("missing timeSignatureSettings");
      timeSignatureSettings = Lily::default_time_signature_settings;
    }
  set_property (context (), "timeSignatureSettings", timeSignatureSettings);

  SCM beamExceptions = get_property (this, "beamExceptions");
  if (!scm_is_pair (beamExceptions))
    {
      beamExceptions
        = Lily::beam_exceptions (timeSignatureFraction, timeSignatureSettings);
    }
  set_property (context (), "beamExceptions", beamExceptions);

  SCM baseMoment = get_property (this, "baseMoment");
  if (!unsmob<Moment> (baseMoment))
    {
      baseMoment = Moment (from_scm<Rational> (Lily::base_length (
                             timeSignatureFraction, timeSignatureSettings)))
                     .smobbed_copy ();
    }
  set_property (context (), "baseMoment", baseMoment);

  SCM beatStructure = get_property (this, "beatStructure");
  if (!scm_is_pair (beatStructure))
    {
      beatStructure = Lily::beat_structure (
        to_scm (unsmob<Moment> (baseMoment)->main_part_), timeSignatureFraction,
        timeSignatureSettings);
    }
  set_property (context (), "beatStructure", beatStructure);

  set_property (context (), "beamHalfMeasure",
                get_property (this, "beamHalfMeasure"));

  set_property (context (), "autoBeaming", get_property (this, "autoBeaming"));
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

  if (partial_event_)
    {
      context ()->unset_property (ly_symbol2scm ("partialBusy"));
      partial_event_ = nullptr;
    }

  Rational mp = carried_measure_position_;
  SCM measure_start_now = SCM_EOL;

  if (from_scm<bool> (get_property (this, "timing")))
    {
      const auto len = measure_length (context ());

      mp += dt.main_part_;

      if (mp >= len)
        {
          auto cbn = from_scm (get_property (this, "currentBarNumber"), 0L);
          auto ibn = from_scm (get_property (this, "internalBarNumber"), 0L);

          // Advance by just one measure.
          mp -= len;
          ++cbn;
          ++ibn;

          // TODO: To support ad-hoc irregular measures more conveniently,
          // reset measureLength at this point according to the time signature
          // (possibly optionally, controlled by a new context property).

          // Advance through any remaining measures.
          const auto num_measures = (mp / len).trunc_int ();
          mp %= len;
          cbn += num_measures;
          ibn += num_measures;

          set_property (context (), "currentBarNumber", to_scm (cbn));
          set_property (context (), "internalBarNumber", to_scm (ibn));
          measure_start_mom_ = Moment::infinity ();
        }

      if (!mp && dt.main_part_ && (measure_start_mom_ == Moment::infinity ()))
        {
          // We have arrived at zero (as opposed to revisiting it).
          measure_start_now = SCM_BOOL_T;
          measure_start_mom_ = now;
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
  set_property (context (), "measureStartNow", measure_start_now);

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
  ADD_LISTENER (bar_check);
  ADD_LISTENER (fine);
  ADD_LISTENER (partial);
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
