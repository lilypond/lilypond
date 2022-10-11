/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "script-column.hh"

#include "accidental-placement.hh"
#include "arpeggio.hh"
#include "directional-element-interface.hh"
#include "script-interface.hh"
#include "side-position-interface.hh"
#include "warn.hh"
#include "grob.hh"
#include "pointer-group-interface.hh"

#include <map>

using std::map;
using std::vector;

typedef map<Grob *, vector<Grob *>> Grob_scripts_map;

void
Script_column::add_side_positioned (Grob *me, Grob *script)
{
  SCM p = get_property (script, "script-priority");
  if (!scm_is_number (p))
    return;

  Pointer_group_interface::add_grob (me, ly_symbol2scm ("scripts"), script);
  set_object (script, "script-column", me->self_scm ());
}

LY_DEFINE (ly_grob_script_priority_less, "ly:grob-script-priority-less", 2, 0,
           0, (SCM a, SCM b),
           R"(
Compare two grobs by script priority.  For internal use.
           )")
{
  auto *const i1 = LY_ASSERT_SMOB (Grob, a, 1);
  auto *const i2 = LY_ASSERT_SMOB (Grob, b, 2);

  return to_scm (Script_interface::script_priority_less (i1, i2));
}

MAKE_SCHEME_CALLBACK (Script_column, row_before_line_breaking,
                      "ly:script-column::row-before-line-breaking", 1);
SCM
Script_column::row_before_line_breaking (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  vector<Grob *> horizontal_grobs;
  extract_grob_set (me, "scripts", scripts);

  Grob_scripts_map head_scripts_map;
  vector<Grob *> affect_all_grobs;
  for (vsize i = 0; i < scripts.size (); i++)
    {
      Grob *sc = scripts[i];

      /*
        Don't want to consider scripts horizontally next to notes.
      */
      if (has_interface<Accidental_placement> (sc)
          || has_interface<Arpeggio> (sc))
        {
          affect_all_grobs.push_back (sc);
        }
      else if (!scm_is_eq (get_property_data (sc, "Y-offset"),
                           Side_position_interface::y_aligned_side_proc))
        {
          head_scripts_map[sc->get_y_parent ()].push_back (sc);
        }
    }

  for (Grob_scripts_map::const_iterator i (head_scripts_map.begin ());
       i != head_scripts_map.end (); i++)
    {
      vector<Grob *> grobs = (*i).second;

      // this isn't right in all cases, but in general a safe assumption.
      grobs.insert (grobs.end (), affect_all_grobs.begin (),
                    affect_all_grobs.end ());
      order_grobs (grobs);
    }

  return SCM_UNSPECIFIED;
}

MAKE_SCHEME_CALLBACK (Script_column, before_line_breaking,
                      "ly:script-column::before-line-breaking", 1);
SCM
Script_column::before_line_breaking (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  vector<Grob *> staff_sided;

  extract_grob_set (me, "scripts", scripts);
  for (auto *sc : scripts)
    {
      if (sc && sc->is_live ())
        {
          /*
            Don't want to consider scripts horizontally next to notes.
          */
          if (!scm_is_eq (get_property_data (sc, "X-offset"),
                          Side_position_interface::x_aligned_side_proc))
            {
              staff_sided.push_back (sc);
            }
        }
    }

  order_grobs (staff_sided);
  return SCM_UNSPECIFIED;
}

void
Script_column::order_grobs (vector<Grob *> grobs)
{
  Drul_array<SCM> scripts_drul (SCM_EOL, SCM_EOL);
  for (vsize i = 0; i < grobs.size (); i++)
    {
      Grob *g = grobs[i];
      Direction d = get_strict_grob_direction (g);

      scripts_drul[d] = scm_cons (g->self_scm (), scripts_drul[d]);
    }

  for (const auto d : {DOWN, UP})
    {
      SCM ss = scm_reverse_x (scripts_drul[d], SCM_EOL);
      ss = scm_stable_sort_x (ss, ly_grob_script_priority_less_proc);

      Grob *g = 0;    // current grob in list
      Grob *last = 0; // previous grob in list
      SCM initial_outside_staff
        = SCM_EOL; // initial outside_staff_priority of current grob
      SCM last_initial_outside_staff
        = SCM_EOL; // initial outside_staff_priority of previous grob

      //  loop over all grobs in script column (already sorted by script_priority)
      for (SCM s = ss; scm_is_pair (s); s = scm_cdr (s), last = g,
               last_initial_outside_staff = initial_outside_staff)
        {
          g = unsmob<Grob> (scm_car (s));
          initial_outside_staff = get_property (g, "outside-staff-priority");
          if (last) //not the first grob in the list
            {
              SCM last_outside_staff
                = get_property (last, "outside-staff-priority");
              /*
                if outside_staff_priority is missing for previous grob,
                use all the scripts so far as support for the current grob
              */
              if (!scm_is_number (last_outside_staff))
                for (SCM t = ss; !scm_is_eq (t, s); t = scm_cdr (t))
                  Side_position_interface::add_support (
                    g, unsmob<Grob> (scm_car (t)));
              /*
                if outside_staff_priority is missing or is equal to original
                outside_staff_priority of previous grob, set new
                outside_staff_priority to just higher than outside_staff_priority
                of previous grob in order to preserve ordering.
              */
              else if ((!scm_is_number (initial_outside_staff))
                       || (fabs (
                             from_scm<double> (initial_outside_staff)
                             - from_scm<double> (last_initial_outside_staff, 0))
                           < 0.001))
                set_property (
                  g, "outside-staff-priority",
                  to_scm (from_scm<double> (last_outside_staff) + 0.1));
            }
        }
    }
}

ADD_INTERFACE (Script_column,
               R"(
An interface that sorts scripts according to their @code{script-priority} and
@code{outside-staff-priority}.
               )",

               /* properties */
               R"(
scripts
               )");
