/*
  script-column.cc -- implement Script_column

  source file of the GNU LilyPond music typesetter

  (c) 1999--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "script-column.hh"

#include "directional-element-interface.hh"
#include "side-position-interface.hh"
#include "warn.hh"
#include "pointer-group-interface.hh"

void
Script_column::add_staff_sided (Grob *me, Item *item)
{
  SCM p = item->get_property ("script-priority");
  if (!scm_is_number (p))
    return;

  Pointer_group_interface::add_grob (me, ly_symbol2scm ("scripts"), item);
}

LY_DEFINE (ly_grob_script_priority_less, "ly:grob-script-priority-less",
	   2, 0, 0, (SCM a, SCM b),
	   "Compare two grobs by script priority. For internal use.")
{
  Grob *i1 = unsmob_grob (a);
  Grob *i2 = unsmob_grob (b);

  SCM p1 = i1->get_property ("script-priority");
  SCM p2 = i2->get_property ("script-priority");

  return scm_to_int (p1) < scm_to_int (p2) ? SCM_BOOL_T : SCM_BOOL_F;
}

MAKE_SCHEME_CALLBACK (Script_column, before_line_breaking, 1);
SCM
Script_column::before_line_breaking (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Drul_array<SCM> scripts_drul (SCM_EOL, SCM_EOL);
  Link_array<Grob> staff_sided;

  extract_grob_set (me, "scripts", scripts);
  for (int i = 0; i < scripts.size (); i++)
    {
      Grob *sc = scripts[i];

      /*
	Don't want to consider scripts horizontally next to notes.
      */
      if (sc->get_property_data (ly_symbol2scm ("X-offset")) !=
	  Side_position_interface::x_aligned_side_proc)
	staff_sided.push (sc);
    }

  for (int i = 0; i < staff_sided.size (); i++)
    {
      Grob *g = staff_sided[i];
      Direction d = get_grob_direction (g);

      scripts_drul[d] = scm_cons (g->self_scm (), scripts_drul[d]);
    }

  Direction d = DOWN;
  do
    {
      SCM ss = scm_reverse_x (scripts_drul[d], SCM_EOL);
      ss = scm_stable_sort_x (ss, ly_grob_script_priority_less_proc);

      Grob *last = 0;
      for (SCM s = ss; scm_is_pair (s); s = scm_cdr (s))
	{
	  Grob *g = unsmob_grob (scm_car (s));
	  if (last)
	    Side_position_interface::add_support (g, last);

	  last = g;
	}
    }
  while (flip (&d) != DOWN);

  return SCM_UNSPECIFIED;
}

ADD_INTERFACE (Script_column, "script-column-interface",
	       "An interface that sorts scripts "
	       "according to their @code{script-priority}",
	       "");
