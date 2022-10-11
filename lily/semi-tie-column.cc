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

#include "semi-tie-column.hh"
#include "semi-tie.hh"
#include "grob.hh"
#include "tie-column.hh"
#include "tie.hh"
#include "directional-element-interface.hh"
#include "pointer-group-interface.hh"
#include "staff-symbol-referencer.hh"
#include "item.hh"
#include "tie-formatting-problem.hh"

#include <algorithm>
#include <vector>

using std::vector;

ADD_INTERFACE (Semi_tie_column,
               R"(
The interface for a column of l.v.  (laissez vibrer) ties.
               )",

               /* properties */
               R"(
positioning-done
head-direction
tie-configuration
ties
               )");

/*
  Cut & paste from tie-column.cc
 */
MAKE_SCHEME_CALLBACK (Semi_tie_column, calc_positioning_done,
                      "ly:semi-tie-column::calc-positioning-done", 1);
SCM
Semi_tie_column::calc_positioning_done (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);

  set_property (me, "positioning-done", SCM_BOOL_T);

  extract_grob_set (me, "ties", lv_ro_ties);
  vector<Grob *> lv_ties (lv_ro_ties);

  std::sort (lv_ties.begin (), lv_ties.end (), Semi_tie::less);

  Ties_configuration ties_config;

  Tie_formatting_problem problem;

  problem.from_semi_ties (
    lv_ties, from_scm<Direction> (get_property (me, "head-direction")));

  SCM manual_configs = get_property (me, "tie-configuration");
  problem.set_manual_tie_configuration (manual_configs);

  Ties_configuration base = problem.generate_optimal_configuration ();
  for (vsize i = 0; i < lv_ties.size (); i++)
    {
      SCM cp = Tie::get_control_points (
        lv_ties[i], problem.common_x_refpoint (), base[i], problem.details_);

      set_property (lv_ties[i], "control-points", cp);
      set_grob_direction (lv_ties[i], base[i].dir_);

      problem.set_debug_scoring (base);
    }

  return SCM_BOOL_T;
}

MAKE_SCHEME_CALLBACK (Semi_tie_column, calc_head_direction,
                      "ly:semi-tie-column::calc-head-direction", 1);
SCM
Semi_tie_column::calc_head_direction (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);

  extract_grob_set (me, "ties", ties);
  Direction d = LEFT;
  for (vsize i = 0; i < ties.size (); i++)
    {
      Direction this_d
        = from_scm<Direction> (get_property (ties[i], "head-direction"));
      if (i > 0 && d != this_d)
        {
          programming_error ("all semi-ties in a semi-tie-column should have "
                             "the same head-direction");
          return to_scm (d);
        }
      d = this_d;
    }
  return to_scm (d);
}
