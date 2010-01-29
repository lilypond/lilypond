/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "side-position-interface.hh"
#include "warn.hh"
#include "grob.hh"
#include "pointer-group-interface.hh"

#include <map>

typedef map<Grob*, vector <Grob*> > Grob_scripts_map;

void
Script_column::add_side_positioned (Grob *me, Grob *script)
{
  SCM p = script->get_property ("script-priority");
  if (!scm_is_number (p))
    return;

  Pointer_group_interface::add_grob (me, ly_symbol2scm ("scripts"), script);
}

LY_DEFINE (ly_grob_script_priority_less, "ly:grob-script-priority-less",
	   2, 0, 0, (SCM a, SCM b),
	   "Compare two grobs by script priority.  For internal use.")
{
  Grob *i1 = unsmob_grob (a);
  Grob *i2 = unsmob_grob (b);

  SCM p1 = i1->get_property ("script-priority");
  SCM p2 = i2->get_property ("script-priority");

  return scm_to_int (p1) < scm_to_int (p2) ? SCM_BOOL_T : SCM_BOOL_F;
}

MAKE_SCHEME_CALLBACK (Script_column, row_before_line_breaking, 1);
SCM
Script_column::row_before_line_breaking (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  vector<Grob*> horizontal_grobs;
  extract_grob_set (me, "scripts", scripts);

  Grob_scripts_map head_scripts_map;
  vector<Grob *> affect_all_grobs;
  for (vsize i = 0; i < scripts.size (); i++)
    {
      Grob *sc = scripts[i];

      /*
	Don't want to consider scripts horizontally next to notes.
      */
      if (Accidental_placement::has_interface (sc)
	  || Arpeggio::has_interface (sc))
	{
	  affect_all_grobs.push_back (sc);
	}
      else if (sc->get_property_data ("Y-offset") !=
	       Side_position_interface::y_aligned_side_proc)
	{
	  head_scripts_map[sc->get_parent (Y_AXIS)].push_back (sc);
	}
    }

  for (Grob_scripts_map::const_iterator i (head_scripts_map.begin ());
       i != head_scripts_map.end ();
       i++)
    {
      vector<Grob*> grobs  = (*i).second;

      // this isn't right in all cases, but in general a safe assumption.
      concat (grobs, affect_all_grobs);
      order_grobs (grobs);
    }

  return SCM_UNSPECIFIED;
}


MAKE_SCHEME_CALLBACK (Script_column, before_line_breaking, 1);
SCM
Script_column::before_line_breaking (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  vector<Grob*> staff_sided;

  extract_grob_set (me, "scripts", scripts);
  for (vsize i = 0; i < scripts.size (); i++)
    {
      Grob *sc = scripts[i];
      /*
	Don't want to consider scripts horizontally next to notes.
      */
      if (sc->get_property_data ("X-offset") !=
	  Side_position_interface::x_aligned_side_proc)
	staff_sided.push_back (sc);
    }

  order_grobs (staff_sided);
  return SCM_UNSPECIFIED;
}

void
Script_column::order_grobs (vector<Grob*> grobs)
{
  Drul_array<SCM> scripts_drul (SCM_EOL, SCM_EOL);
  for (vsize i = 0; i < grobs.size (); i++)
    {
      Grob *g = grobs[i];
      Direction d = get_grob_direction (g);

      scripts_drul[d] = scm_cons (g->self_scm (), scripts_drul[d]);
    }

  Direction d = DOWN;
  do
    {
      SCM ss = scm_reverse_x (scripts_drul[d], SCM_EOL);
      ss = scm_stable_sort_x (ss, ly_grob_script_priority_less_proc);

      Grob *g = 0;  // current grob in list
      Grob *last = 0; // previous grob in list
      SCM initial_outside_staff = SCM_EOL;  // initial outside_staff_priority of current grob
      SCM last_initial_outside_staff = SCM_EOL;  // initial outside_staff_priority of previous grob

      //  loop over all grobs in script column (already sorted by script_priority)
      for (SCM s = ss; scm_is_pair (s);
           s = scm_cdr (s), last = g, last_initial_outside_staff = initial_outside_staff)
	{
	  g = unsmob_grob (scm_car (s));
          initial_outside_staff = g->get_property ("outside-staff-priority");
          if (last)    //not the first grob in the list
            {
	      SCM last_outside_staff = last->get_property ("outside-staff-priority");
              /*
                if outside_staff_priority is missing for previous grob, just
                use it as a support for the current grob
              */
	      if (!scm_is_number (last_outside_staff))
		Side_position_interface::add_support (g, last);
              /*
                if outside_staff_priority is missing or is equal to original
                outside_staff_priority of previous grob, set new
                outside_staff_priority to just higher than outside_staff_priority
                of previous grob in order to preserve ordering.
              */
              else if ((!scm_is_number (initial_outside_staff)) ||
                       (fabs (scm_to_double (initial_outside_staff) -
                             robust_scm2double (last_initial_outside_staff, 0)) < 0.001))
                g->set_property ("outside-staff-priority",
				     scm_from_double (
                                       scm_to_double (last_outside_staff) + 0.1));
	    }
	}
    }
  while (flip (&d) != DOWN);
}

ADD_INTERFACE (Script_column,
	       "An interface that sorts scripts according to their"
	       " @code{script-priority} and @code{outside-staff-priority}.",

	       /* properties */
	       ""
	       );
