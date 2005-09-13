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
void
Laissez_vibrer_tie_column::set_directions (Grob *me)
{
  if (!to_boolean (me->get_property ("positioning-done")))
    me->set_property ("positioning-done", SCM_BOOL_T); 
  else
    return;


  extract_grob_set (me, "ties", lv_ro_ties);
  Link_array<Grob> lv_ties (lv_ro_ties);

  lv_ties.sort (&Laissez_vibrer_tie::compare);

  Array<Tie_configuration> tie_configs;
  Link_array<Item> heads;
  for (int i = 0; i < lv_ties.size (); i++)
    {
      Tie_configuration conf;
      conf.dir_ = get_grob_direction (lv_ties[i]);
      Item *head = unsmob_item (lv_ties[i]->get_object ("note-head"));

      heads.push (head);
      if (head)
	conf.position_ = (int) Staff_symbol_referencer::get_position (head);
      
      tie_configs.push (conf);
    }

  bool manual_override = false;
  SCM manual_configs = me->get_property ("tie-configuration");
  set_manual_tie_configuration (&tie_configs,
				&manual_override,
				manual_configs
				);

  set_tie_config_directions (&tie_configs);

  Grob *common = me;
  for (int i = 0; i < lv_ties.size (); i++)
    {
      common = lv_ties[i]->common_refpoint (common, X_AXIS); 
    }

  Drul_array< Array<Skyline_entry> > skylines;
  set_chord_outline (&skylines[LEFT],
		     heads,
		     common, LEFT);

  Real right_most = - infinity_f;   
  for (int i = 0; i < skylines[LEFT].size (); i++)
    {
      right_most = max (right_most, skylines[LEFT][i].height_);
    }

  Skyline_entry right_entry;
  right_entry.width_.set_full ();
  right_entry.height_ = right_most + 1.5;
  
  skylines[RIGHT].push (right_entry);

  Tie_details details;
  details.init (lv_ties[0]);

  /*
    Calculate final width and shape of the ties.
   */
  for (int i = 0; i < lv_ties.size(); i++)
    {
      final_shape_adjustment (tie_configs[i],
			      skylines,
			      lv_ties[0],
			      details);
    }
  
  /*
    Try to shift small ties into available spaces.
   */
  if (!manual_override)
    {
      shift_small_ties (&tie_configs, lv_ties[0], details);
    }
  
  for (int i = 0; i < lv_ties.size(); i++)
    {
      Tie::set_control_points (lv_ties[i], common, tie_configs[i],
			       details );
      set_grob_direction (lv_ties[i], tie_configs[i].dir_);
    }
}
  

