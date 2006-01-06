/*
  laissez-vibrer-tie-column.cc -- implement Laissez_vibrer_tie_column

  source file of the GNU LilyPond music typesetter

  (c) 2005--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "laissez-vibrer-tie-column.hh"
#include "laissez-vibrer-tie.hh"
#include "grob.hh"
#include "tie-column.hh"
#include "tie.hh"
#include "directional-element-interface.hh"
#include "pointer-group-interface.hh"
#include "staff-symbol-referencer.hh"
#include "item.hh"
#include "tie-formatting-problem.hh"
#include "tie-configuration.hh"
#include "tie-column-format.hh"


ADD_INTERFACE(Laissez_vibrer_tie_column,
	      "laissez-vibrer-tie-column-interface",
	      "The interface for a column of l.v. ties.",

	      /* properties */
	      "positioning-done "
	      "tie-configuration "
	      );
			   


/*
  Cut & paste from tie-column.cc
 */   
MAKE_SCHEME_CALLBACK(Laissez_vibrer_tie_column, calc_positioning_done, 1);
SCM
Laissez_vibrer_tie_column::calc_positioning_done (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  
  extract_grob_set (me, "ties", lv_ro_ties);
  Link_array<Grob> lv_ties (lv_ro_ties);

  lv_ties.sort (&Laissez_vibrer_tie::compare);

  Ties_configuration ties_config;
  

  Tie_formatting_problem problem;
  
  problem.from_lv_ties (lv_ties);

  SCM manual_configs = me->get_property ("tie-configuration");
  problem.set_manual_tie_configuration (manual_configs);

  Ties_configuration base = problem.generate_optimal_chord_configuration ();
  for (int i = 0; i < lv_ties.size(); i++)
    {
      Tie::set_control_points (lv_ties[i], problem.common_x_refpoint (), base[i],
			       problem.details_);
      set_grob_direction (lv_ties[i], base[i].dir_);
    }

  return SCM_BOOL_T;
}
  

