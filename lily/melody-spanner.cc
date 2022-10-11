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

#include "melody-spanner.hh"
#include "grob.hh"
#include "pointer-group-interface.hh"

using std::vector;

/*
  TODO: this could be either item or spanner. For efficiency reasons,
  let's take item for now.
*/

/*
  Interpolate stem directions for neutral stems.
 */
MAKE_SCHEME_CALLBACK (Melody_spanner, calc_neutral_stem_direction,
                      "ly:melody-spanner::calc-neutral-stem-direction", 1);
SCM
Melody_spanner::calc_neutral_stem_direction (SCM smob)
{
  auto *const stem = LY_ASSERT_SMOB (Grob, smob, 1);
  Grob *me = unsmob<Grob> (get_object (stem, "melody-spanner"));
  if (!me || !me->is_live ())
    return to_scm (DOWN);

  extract_grob_set (me, "stems", stems);

  vector<Direction> dirs;
  for (vsize i = 0; i < stems.size (); i++)
    dirs.push_back (
      from_scm<Direction> (get_property (stems[i], "default-direction")));

  vsize last_nonneutral = VPOS;
  vsize next_nonneutral = 0;
  while (next_nonneutral != VPOS && next_nonneutral < dirs.size ()
         && !dirs[next_nonneutral])
    next_nonneutral++;

  SCM retval = SCM_EOL;
  while (last_nonneutral == VPOS || last_nonneutral + 1 < dirs.size ())
    {
      Direction d1 = CENTER;
      Direction d2 = CENTER;
      if (last_nonneutral != VPOS)
        d1 = dirs[last_nonneutral];
      if (next_nonneutral < dirs.size ())
        d2 = dirs[next_nonneutral];

      Direction total = CENTER;
      if (d1 && d1 == d2)
        total = d1;
      else if (d1 && !d2)
        total = d1;
      else if (d2 && !d1)
        total = d2;
      else
        total = from_scm<Direction> (get_property (me, "neutral-direction"));

      for (vsize i = last_nonneutral + 1; i < next_nonneutral; i++)
        {
          if (stems[i] == stem)
            retval = to_scm (total);
          else
            set_property (stems[i], "neutral-direction", to_scm (total));
        }

      last_nonneutral = next_nonneutral;
      while (last_nonneutral < dirs.size () && dirs[last_nonneutral])
        last_nonneutral++;
      next_nonneutral = last_nonneutral;
      last_nonneutral--;

      while (next_nonneutral < dirs.size () && !dirs[next_nonneutral])
        next_nonneutral++;
    }

  return retval;
}

void
Melody_spanner::add_stem (Grob *me, Grob *stem)
{
  Pointer_group_interface::add_grob (me, ly_symbol2scm ("stems"), stem);
  set_object (stem, "melody-spanner", me->self_scm ());
  set_property (stem, "neutral-direction",
                Melody_spanner::calc_neutral_stem_direction_proc);
}

ADD_INTERFACE (Melody_spanner,
               R"(
Context dependent typesetting decisions.
               )",

               /* properties */
               R"(
stems
neutral-direction
               )");
