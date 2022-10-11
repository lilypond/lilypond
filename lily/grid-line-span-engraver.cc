/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "item.hh"
#include "grid-line-interface.hh"

#include "translator.icc"

using std::vector;

class Grid_line_span_engraver : public Engraver
{
  Item *spanline_;
  vector<Item *> lines_;

public:
  TRANSLATOR_DECLARATIONS (Grid_line_span_engraver);

protected:
  void acknowledge_grid_point (Grob_info_t<Item>);
  void stop_translation_timestep ();
};

Grid_line_span_engraver::Grid_line_span_engraver (Context *c)
  : Engraver (c)
{
  spanline_ = 0;
}

void
Grid_line_span_engraver::acknowledge_grid_point (Grob_info_t<Item> info)
{
  lines_.push_back (info.grob ());

  if (lines_.size () >= 2 && !spanline_)
    {
      spanline_ = make_item ("GridLine", SCM_EOL);
      spanline_->set_x_parent (lines_[0]);
    }
}

void
Grid_line_span_engraver::stop_translation_timestep ()
{
  if (spanline_)
    {
      for (vsize i = 0; i < lines_.size (); i++)
        Grid_line_interface::add_grid_point (spanline_, lines_[i]);

      spanline_ = 0;
    }
  lines_.resize (0);
}

void
Grid_line_span_engraver::boot ()
{
  ADD_ACKNOWLEDGER (grid_point);
}

ADD_TRANSLATOR (Grid_line_span_engraver,
                /* doc */
                R"(
This engraver makes cross-staff lines: It catches all normal lines and draws a
single span line across them.
                )",

                /* create */
                R"(
GridLine
                )",

                /* read */
                R"(

                )",

                /* write */
                R"(

                )");
