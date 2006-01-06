/*
  tie-column.cc -- implement Tie_column

  source file of the GNU LilyPond music typesetter

  (c) 2000--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "tie-column.hh"

#include <cmath>

#include "skyline.hh"
#include "warn.hh"
#include "paper-column.hh"
#include "spanner.hh"
#include "pointer-group-interface.hh"
#include "tie.hh"
#include "directional-element-interface.hh"
#include "tie-column-format.hh"
#include "tie-formatting-problem.hh"
#include "tie-configuration.hh"

using namespace std;

void
Tie_column::add_tie (Grob *me, Grob *tie)
{
  if (tie->get_parent (Y_AXIS)
      && Tie_column::has_interface (tie->get_parent (Y_AXIS)))
    return;

  if (!Pointer_group_interface::count (me, ly_symbol2scm ("ties")))
    {
      dynamic_cast<Spanner *> (me)->set_bound (LEFT, Tie::head (tie, LEFT));
      dynamic_cast<Spanner *> (me)->set_bound (RIGHT, Tie::head (tie, RIGHT));
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
  Spanner *me = dynamic_cast<Spanner *> (unsmob_grob (smob));
  for (SCM s = me->get_property ("ties"); scm_is_pair (s); s = scm_cdr (s))
    {
      Spanner *tie = dynamic_cast<Spanner *> (unsmob_grob (scm_car (s)));
      Direction dir = LEFT;
      do
	{
	  if (dir * tie->get_bound (dir)->get_column ()->get_rank ()
	      > dir * me->get_bound (dir)->get_column ()->get_rank ())
	    me->set_bound (dir, Tie::head (tie, dir));
	}
      while (flip (&dir) != LEFT);
    }
  
  return SCM_UNSPECIFIED;
}

MAKE_SCHEME_CALLBACK(Tie_column, calc_positioning_done, 1)
SCM
Tie_column::calc_positioning_done (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  extract_grob_set (me, "ties", ro_ties);
  Link_array<Grob> ties (ro_ties);
  if (!ties.size ())
    return SCM_BOOL_T;

  if (ties.size() == 1)
    {
      /*
	Already handled by standard mechanisms.
       */
      return SCM_BOOL_T;
    }
  
  ties.sort (&Tie::compare);

  Tie_formatting_problem problem;
  problem.from_ties (ties);

  SCM manual_configs = me->get_property ("tie-configuration");
  problem.set_manual_tie_configuration (manual_configs);


  Ties_configuration base = problem.generate_optimal_chord_configuration ();

  for (int i = 0; i < base.size(); i++)
    {
      Tie::set_control_points (ties[i], problem.common_x_refpoint (),
			       base[i],
			       problem.details_);
      set_grob_direction (ties[i],
			  base[i].dir_);
    }
  return SCM_BOOL_T;
}



ADD_INTERFACE (Tie_column, "tie-column-interface",
	       "Object that sets directions of multiple ties in a tied chord",

	       /* properties */
	       "positioning-done "
	       "tie-configuration "
	       );

