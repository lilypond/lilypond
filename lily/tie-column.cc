/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "tie-column.hh"

#include <cmath>

#include "directional-element-interface.hh"
#include "output-def.hh"
#include "paper-column.hh"
#include "pointer-group-interface.hh"
#include "skyline.hh"
#include "spanner.hh"
#include "tie-configuration.hh"
#include "tie-formatting-problem.hh"
#include "tie.hh"
#include "warn.hh"

using std::vector;

void
Tie_column::add_tie (Grob *tc, Spanner *tie)
{
  Spanner *me = dynamic_cast<Spanner *> (tc);

  if (tie->get_parent (Y_AXIS)
      && has_interface<Tie_column> (tie->get_parent (Y_AXIS)))
    return;

  if (!me->get_bound (LEFT)
      || (me->get_bound (LEFT)->get_column ()->get_rank ()
          > tie->get_bound (LEFT)->get_column ()->get_rank ()))
    {
      me->set_bound (LEFT, Tie::head (tie, LEFT));
      me->set_bound (RIGHT, Tie::head (tie, RIGHT));
    }

  tie->set_parent (me, Y_AXIS);
  Pointer_group_interface::add_grob (me, ly_symbol2scm ("ties"), tie);
}

/*
  Extend the spanner over its Tie constituents.
*/
MAKE_SCHEME_CALLBACK (Tie_column, before_line_breaking, 1);
SCM
Tie_column::before_line_breaking (SCM smob)
{
  Spanner *me = unsmob<Spanner> (smob);
  for (SCM s = me->get_property ("ties"); scm_is_pair (s); s = scm_cdr (s))
    {
      Spanner *tie = unsmob<Spanner> (scm_car (s));
      for (LEFT_and_RIGHT (dir))
        {
          if (dir * tie->get_bound (dir)->get_column ()->get_rank ()
              > dir * me->get_bound (dir)->get_column ()->get_rank ())
            me->set_bound (dir, Tie::head (tie, dir));
        }
    }

  return SCM_UNSPECIFIED;
}

MAKE_SCHEME_CALLBACK (Tie_column, calc_positioning_done, 1)
SCM
Tie_column::calc_positioning_done (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);
  extract_grob_set (me, "ties", ro_ties);
  vector<Grob *> ties (ro_ties);
  if (!ties.size ())
    return SCM_BOOL_T;

  me->set_property ("positioning-done", SCM_BOOL_T);
  vector_sort (ties, Tie::less);

  Tie_formatting_problem problem;
  problem.from_ties (ties);

  SCM manual_configs = me->get_property ("tie-configuration");
  problem.set_manual_tie_configuration (manual_configs);

  Ties_configuration base = problem.generate_optimal_configuration ();
  for (vsize i = 0; i < base.size (); i++)
    {
      SCM cp = Tie::get_control_points (ties[i], problem.common_x_refpoint (),
                                        base[i], problem.details_);

      ties[i]->set_property ("control-points", cp);
      set_grob_direction (ties[i], base[i].dir_);

      problem.set_debug_scoring (base);
    }
  return SCM_BOOL_T;
}

ADD_INTERFACE (Tie_column,
               "Object that sets directions of multiple ties in a tied"
               " chord.",

               /* properties */
               "positioning-done "
               "tie-configuration "
               "ties ");
