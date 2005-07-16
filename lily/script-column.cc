/*
  script-column.cc -- implement Script_column

  source file of the GNU LilyPond music typesetter

  (c) 1999--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "script-column.hh"

#include "side-position-interface.hh"
#include "warn.hh"
#include "pointer-group-interface.hh"

void
Script_column::add_staff_sided (Grob *me, Item *i)
{
  SCM p = i->get_property ("script-priority");
  if (!scm_is_number (p))
    return;

  Pointer_group_interface::add_grob (me, ly_symbol2scm ("scripts"), i);

  me->add_dependency (i);
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
  Drul_array<SCM> scripts (SCM_EOL, SCM_EOL);
  Link_array<Grob> staff_sided;

  for (SCM s = me->get_property ("scripts"); scm_is_pair (s); s = scm_cdr (s))
    {
      Grob *sc = unsmob_grob (scm_car (s));

      /*
	Don't want to consider scripts horizontally next to notes.
      */
      if (!sc->has_offset_callback (Side_position_interface::aligned_side_proc,
				    X_AXIS))
	staff_sided.push (sc);
    }

  for (int i = 0; i < staff_sided.size (); i++)
    {
      Grob *g = staff_sided[i];
      Direction d = Side_position_interface::get_direction (g);
      if (!d)
	{
	  programming_error ("no direction for script");
	  d = DOWN;
	  g->set_property ("direction", scm_int2num (d));
	}

      scripts[d] = scm_cons (g->self_scm (), scripts[d]);
    }

  Direction d = DOWN;
  do
    {
      SCM ss = scm_reverse_x (scripts[d], SCM_EOL);
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
