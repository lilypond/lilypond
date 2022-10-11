/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>, Erik Sandberg <mandolaerik@gmail.com>

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

#include "engraver.hh"
#include "global-context.hh"
#include "international.hh"
#include "item.hh"
#include "side-position-interface.hh"
#include "spanner.hh"
#include "stream-event.hh"
#include "warn.hh"

#include "translator.icc"

class Percent_repeat_engraver : public Engraver
{
  void typeset_perc ();

public:
  TRANSLATOR_DECLARATIONS (Percent_repeat_engraver);

protected:
  Stream_event *percent_event_ = nullptr;

  // moment (global time) where percent should end
  Moment stop_mom_;

  Spanner *percent_ = nullptr;
  Spanner *percent_counter_ = nullptr;

  /* If the measure starts with grace notes, the percent event
     occurs on the first non-grace note, but we want the spanner's
     left bound to be the non-musical column that was current at
     the time of the first grace note. */
  Grob *first_command_column_ = nullptr;
  Moment command_moment_ {-1};

  void listen_percent (Stream_event *);
  void process_music ();
  void stop_translation_timestep ();
  void finalize () override;
};

Percent_repeat_engraver::Percent_repeat_engraver (Context *c)
  : Engraver (c)
{
}

void
Percent_repeat_engraver::listen_percent (Stream_event *ev)
{
  assign_event_once (percent_event_, ev);
}

void
Percent_repeat_engraver::process_music ()
{
  // Maintain first_command_column_ as the first non-musical column
  // in the grace group.
  if (now_mom ().main_part_ != command_moment_.main_part_)
    {
      first_command_column_
        = unsmob<Grob> (get_property (this, "currentCommandColumn"));
      command_moment_ = now_mom ();
    }

  // Stop running percent if it has reached completion.
  if (percent_ && stop_mom_.main_part_ == now_mom ().main_part_)
    {
      percent_->set_bound (RIGHT, first_command_column_);
      percent_ = nullptr;
      if (percent_counter_)
        {
          percent_counter_->set_bound (RIGHT, first_command_column_);
          percent_counter_ = nullptr;
        }
    }
  else if (percent_ && percent_event_)
    {
      percent_event_->warning (
        _ ("percent repeat started while another already in progress"));
      percent_->suicide ();
      percent_ = nullptr;
      if (percent_counter_)
        {
          percent_counter_->suicide ();
          percent_counter_ = nullptr;
        }
    }

  // Start a new percent if requested.
  if (percent_event_)
    {
      stop_mom_ = now_mom () + get_event_length (percent_event_);
      find_global_context ()->add_moment_to_process (stop_mom_);
      percent_ = make_spanner ("PercentRepeat", percent_event_->self_scm ());
      percent_->set_bound (LEFT, first_command_column_);

      SCM count = get_property (percent_event_, "repeat-count");
      if (!scm_is_null (count)
          && from_scm<bool> (get_property (this, "countPercentRepeats"))
          && check_repeat_count_visibility (context (), count))
        {
          percent_counter_ = make_spanner ("PercentRepeatCounter",
                                           percent_event_->self_scm ());

          SCM text = scm_number_to_string (count, to_scm (10));
          set_property (percent_counter_, "text", text);
          percent_counter_->set_bound (LEFT, first_command_column_);
          Side_position_interface::add_support (percent_counter_, percent_);
          percent_counter_->set_y_parent (percent_);
          percent_counter_->set_x_parent (percent_);
        }
      else
        percent_counter_ = nullptr;
    }
}

void
Percent_repeat_engraver::finalize ()
{
  if (percent_)
    {
      percent_->programming_error (
        "percent end moment should have been processed");
      percent_->suicide ();
      if (percent_counter_)
        percent_counter_->suicide ();
    }
}

void
Percent_repeat_engraver::stop_translation_timestep ()
{
  percent_event_ = nullptr;
}

void
Percent_repeat_engraver::boot ()
{
  ADD_LISTENER (percent);
}

ADD_TRANSLATOR (Percent_repeat_engraver,
                /* doc */
                R"(
Make whole measure repeats.
                )",

                /* create */
                R"(
PercentRepeat
PercentRepeatCounter
                )",

                /* read */
                R"(
countPercentRepeats
currentCommandColumn
repeatCountVisibility
                )",

                /* write */
                R"(

                )");
