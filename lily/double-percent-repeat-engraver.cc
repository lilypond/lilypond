/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2011--2020 Neil Puttock <n.puttock@gmail.com>

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
#include "stream-event.hh"
#include "warn.hh"

#include "translator.icc"

class Double_percent_repeat_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Double_percent_repeat_engraver);

protected:
  Stream_event *percent_event_;

  // moment (global time) where percent started
  Moment start_mom_;

  void listen_double_percent (Stream_event *);

  void process_music ();
};

Double_percent_repeat_engraver::Double_percent_repeat_engraver (Context *c)
    : Engraver (c)
{
  percent_event_ = 0;
}

void
Double_percent_repeat_engraver::listen_double_percent (Stream_event *ev)
{
  if (!percent_event_)
    {
      Moment meas_len (
          robust_scm2moment (get_property ("measureLength"), Moment (1)));
      start_mom_ = now_mom () + meas_len;
      find_global_context ()->add_moment_to_process (start_mom_);
      percent_event_ = ev;
    }
  else
    ASSIGN_EVENT_ONCE (percent_event_, ev);
}

void
Double_percent_repeat_engraver::process_music ()
{
  if (percent_event_ && now_mom ().main_part_ == start_mom_.main_part_)
    {
      Item *double_percent
          = make_item ("DoublePercentRepeat", percent_event_->self_scm ());

      SCM count = percent_event_->get_property ("repeat-count");
      if (!scm_is_null (count)
          && to_boolean (get_property ("countPercentRepeats"))
          && check_repeat_count_visibility (context (), count))
        {
          Item *double_percent_counter = make_item (
              "DoublePercentRepeatCounter", percent_event_->self_scm ());

          SCM text = scm_number_to_string (count, scm_from_int (10));
          double_percent_counter->set_property ("text", text);

          Side_position_interface::add_support (double_percent_counter,
                                                double_percent);
          double_percent_counter->set_parent (double_percent, Y_AXIS);
          double_percent_counter->set_parent (double_percent, X_AXIS);
        }
      // forbid breaks on a % line
      find_score_context ()->set_property ("forbidBreak", SCM_BOOL_T);
      percent_event_ = 0;
    }
}

void
Double_percent_repeat_engraver::boot ()
{
  ADD_LISTENER (Double_percent_repeat_engraver, double_percent);
}

ADD_TRANSLATOR (Double_percent_repeat_engraver,
                /* doc */
                "Make double measure repeats.",

                /* create */
                "DoublePercentRepeat "
                "DoublePercentRepeatCounter ",

                /* read */
                "countPercentRepeats "
                "measureLength "
                "repeatCountVisibility ",

                /* write */
                "forbidBreak ");
