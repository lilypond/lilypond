/*
  laissez-vibrer-tie-column.cc -- implement Laissez_vibrer_tie_column

  source file of the GNU LilyPond music typesetter

  (c) 2005 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "tie-column-format.hh"
#include "tie-formatting-problem.hh"


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
  for (int i = 0; i < lv_ties.size (); i++)
    {
      Tie_configuration conf;
      conf.dir_ = CENTER;
      Item *head = unsmob_item (lv_ties[i]->get_object ("note-head"));

      if (head)
	conf.position_ = (int) Staff_symbol_referencer::get_position (head);
      
      ties_config.ties_.push (conf);
    }

  bool manual_override = false;
  SCM manual_configs = me->get_property ("tie-configuration");
  set_manual_tie_configuration (&ties_config,
				&manual_override,
				manual_configs
				);

  set_tie_config_directions (&ties_config);

  Tie_formatting_problem problem;
  problem.from_lv_ties (lv_ties);

  /*
    Calculate final width and shape of the ties.
   */
  for (int i = 0; i < lv_ties.size(); i++)
    {
      final_shape_adjustment (ties_config.ties_[i],
			      problem, lv_ties[0]);
    }
  
  /*
    Try to shift small ties into available spaces.
   */
  if (!manual_override)
    {
      shift_small_ties (&ties_config, lv_ties[0], problem.details_);
    }
  
  for (int i = 0; i < lv_ties.size(); i++)
    {
      Tie::set_control_points (lv_ties[i], problem.common_x_refpoint (), ties_config.ties_[i],
			       problem.details_);
      set_grob_direction (lv_ties[i], ties_config.ties_[i].dir_);
    }

  return SCM_BOOL_T;
}
  

