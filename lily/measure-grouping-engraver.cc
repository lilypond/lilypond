/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2002--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "warn.hh"
#include "side-position-interface.hh"
#include "global-context.hh"
#include "item.hh"
#include "engraver.hh"
#include "spanner.hh"

#include "translator.icc"

#include <cstdint>

class Measure_grouping_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Measure_grouping_engraver);

protected:
  Spanner *grouping_;
  Rational stop_grouping_mom_;

  void process_music ();
  void finalize () override;
  void acknowledge_note_column (Grob_info_t<Item>);
};

void
Measure_grouping_engraver::finalize ()
{
  if (grouping_)
    {
      auto *col = unsmob<Grob> (get_property (this, "currentCommandColumn"));
      grouping_->set_bound (RIGHT, col);
      grouping_->suicide ();
      grouping_ = 0;
    }
}

void
Measure_grouping_engraver::acknowledge_note_column (Grob_info_t<Item> gi)
{
  if (grouping_)
    Side_position_interface::add_support (grouping_, gi.grob ());
}

void
Measure_grouping_engraver::process_music ()
{
  auto now = now_mom ();
  if (grouping_ && now.main_part_ >= stop_grouping_mom_ && !now.grace_part_)
    {
      auto *col = unsmob<Grob> (get_property (this, "currentMusicalColumn"));
      grouping_->set_bound (RIGHT, col);
      grouping_ = 0;
    }

  if (now.grace_part_)
    return;

  SCM grouping = get_property (this, "beatStructure");
  if (scm_is_pair (grouping))
    {
      Moment *measpos = unsmob<Moment> (get_property (this, "measurePosition"));
      Rational mp = measpos->main_part_;

      Moment *base_mom = unsmob<Moment> (get_property (this, "baseMoment"));
      Rational base_moment = base_mom->main_part_;

      Rational where (0);
      for (SCM s = grouping; scm_is_pair (s);
           where += Rational (from_scm<int64_t> (scm_car (s))) * base_moment,
               s = scm_cdr (s))
        {
          int grouplen = from_scm<int> (scm_car (s));
          if (where == mp)
            {
              if (grouping_)
                {
                  programming_error ("last grouping not finished yet");
                  continue;
                }
              if (grouplen > 1)
                {
                  grouping_ = make_spanner ("MeasureGrouping", SCM_EOL);
                  auto *col = unsmob<Grob> (
                    get_property (this, "currentMusicalColumn"));
                  grouping_->set_bound (LEFT, col);

                  stop_grouping_mom_
                    = now.main_part_ + Rational (grouplen - 1) * base_moment;
                  find_global_context ()->add_moment_to_process (
                    Moment (stop_grouping_mom_));

                  if (grouplen == 3)
                    set_property (grouping_, "style",
                                  ly_symbol2scm ("triangle"));
                  else
                    set_property (grouping_, "style",
                                  ly_symbol2scm ("bracket"));

                  break;
                }
            }
        }
    }
}

Measure_grouping_engraver::Measure_grouping_engraver (Context *c)
  : Engraver (c)
{
  grouping_ = 0;
}

void
Measure_grouping_engraver::boot ()
{
  ADD_ACKNOWLEDGER (note_column);
}

ADD_TRANSLATOR (Measure_grouping_engraver,
                /* doc */
                R"(
Create @code{MeasureGrouping} to indicate beat subdivision.
                )",

                /* create */
                R"(
MeasureGrouping
                )",

                /* read */
                R"(
baseMoment
beatStructure
currentMusicalColumn
measurePosition
                )",

                /* write */
                R"(

                )");
