/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>, Erik Sandberg <mandolaerik@gmail.com>

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
  Stream_event *percent_event_;

  // moment (global time) where percent started
  Moment start_mom_;
  // moment (global time) where percent should end
  Moment stop_mom_;

  Spanner *percent_;
  Spanner *percent_counter_;

  Grob *first_command_column_;
  Moment command_moment_;

  void finalize () override;
  void listen_percent (Stream_event *);

  void start_translation_timestep ();
  void stop_translation_timestep ();
  void process_music ();
};

Percent_repeat_engraver::Percent_repeat_engraver (Context *c)
  : Engraver (c)
{
  percent_ = 0;
  percent_counter_ = 0;
  percent_event_ = 0;

  first_command_column_ = 0;
  command_moment_ = Moment (-1);
}

void
Percent_repeat_engraver::start_translation_timestep ()
{
  if (now_mom ().main_part_ != command_moment_.main_part_)
    {
      first_command_column_
        = unsmob<Grob> (get_property (this, "currentCommandColumn"));
      command_moment_ = now_mom ();
    }

  if (stop_mom_.main_part_ == now_mom ().main_part_)
    {
      if (percent_)
        typeset_perc ();
      percent_event_ = 0;
    }
}

void
Percent_repeat_engraver::listen_percent (Stream_event *ev)
{
  if (!percent_event_)
    {
      Moment body_length = get_event_length (ev);
      start_mom_ = now_mom ();
      stop_mom_ = now_mom () + body_length;
      find_global_context ()->add_moment_to_process (stop_mom_);
      percent_event_ = ev;
    }
  else
    {
      /*
        print a warning: no assignment happens because
        percent_event_ != 0
      */
      ASSIGN_EVENT_ONCE (percent_event_, ev);
    }
}

void
Percent_repeat_engraver::process_music ()
{
  if (percent_event_
      && now_mom ().main_part_ == start_mom_.main_part_)
    {
      if (percent_)
        typeset_perc ();

      percent_ = make_spanner ("PercentRepeat", percent_event_->self_scm ());

      Grob *col = first_command_column_;
      percent_->set_bound (LEFT, col);

      SCM count = get_property (percent_event_, "repeat-count");
      if (!scm_is_null (count) && from_scm<bool> (get_property (this, "countPercentRepeats"))
          && check_repeat_count_visibility (context (), count))
        {
          percent_counter_ = make_spanner ("PercentRepeatCounter",
                                           percent_event_->self_scm ());

          SCM text = scm_number_to_string (count, to_scm (10));
          set_property (percent_counter_, "text", text);
          percent_counter_->set_bound (LEFT, col);
          Side_position_interface::add_support (percent_counter_, percent_);
          percent_counter_->set_parent (percent_, Y_AXIS);
          percent_counter_->set_parent (percent_, X_AXIS);
        }
      else
        percent_counter_ = 0;
    }
}

void
Percent_repeat_engraver::finalize ()
{
  if (percent_)
    {
      percent_event_->warning (_ ("unterminated percent repeat"));
      percent_->suicide ();
      percent_counter_->suicide ();
    }
}

void
Percent_repeat_engraver::typeset_perc ()
{
  Grob *col = first_command_column_;

  percent_->set_bound (RIGHT, col);
  percent_ = 0;

  if (percent_counter_)
    percent_counter_->set_bound (RIGHT, col);
  percent_counter_ = 0;
}

void
Percent_repeat_engraver::stop_translation_timestep ()
{
}

void
Percent_repeat_engraver::boot ()
{
  ADD_LISTENER (Percent_repeat_engraver, percent);
}

ADD_TRANSLATOR (Percent_repeat_engraver,
                /* doc */
                "Make whole measure repeats.",

                /* create */
                "PercentRepeat "
                "PercentRepeatCounter ",

                /* read */
                "countPercentRepeats "
                "currentCommandColumn "
                "repeatCountVisibility ",

                /* write */
                ""
               );
