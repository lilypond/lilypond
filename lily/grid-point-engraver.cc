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
#include "moment.hh"

#include "translator.icc"

class Grid_point_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Grid_point_engraver);

protected:
  void process_music ();
};

void
Grid_point_engraver::process_music ()
{
  SCM grid_interval = get_property (this, "gridInterval");
  if (Moment *mom = unsmob<Moment> (grid_interval))
    {
      auto now = now_mom ();

      if (!now.main_part_.mod_rat (mom->main_part_))
        make_item ("GridPoint", SCM_EOL);
    }
}

Grid_point_engraver::Grid_point_engraver (Context *c)
  : Engraver (c)
{
}

void
Grid_point_engraver::boot ()
{
}

ADD_TRANSLATOR (Grid_point_engraver,
                /* doc */
                R"(
Generate grid points.
                )",

                /* create */
                R"(
GridPoint
                )",

                /* read */
                R"(
gridInterval
                )",

                /* write */
                R"(

                )");
