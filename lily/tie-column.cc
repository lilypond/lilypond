/*
  tie-column.cc -- implement Tie_column

  source file of the GNU LilyPond music typesetter

  (c) 2000--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "tie-column.hh"

#include <math.h>
#include <map>

#include "skyline.hh"
#include "warn.hh"
#include "paper-column.hh"
#include "spanner.hh"
#include "pointer-group-interface.hh"
#include "tie.hh"
#include "directional-element-interface.hh"
#include "tie-column-format.hh"

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
  tie->add_dependency (me);
}

void
Tie_column::set_directions (Grob *me)
{
  if (!to_boolean (me->get_property ("positioning-done")))
    {
      me->set_property ("positioning-done", SCM_BOOL_T); 
      new_directions (me);
    }
}



MAKE_SCHEME_CALLBACK (Tie_column, after_line_breaking, 1);
SCM
Tie_column::after_line_breaking (SCM smob)
{
  set_directions (unsmob_grob (smob));
  return SCM_UNSPECIFIED;
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


void
Tie_column::new_directions (Grob *me)
{
  extract_grob_set (me, "ties", ro_ties);
  Link_array<Grob> ties (ro_ties);
  if (!ties.size ())
    return;

  if (ties.size() == 1)
    {
      Tie::set_default_control_points (ties[0]);
      return ;
    }
  
  ties.sort (&Tie::compare);

  Array<Tie_configuration> tie_configs;
  for (int i = 0; i < ties.size (); i++)
    {
      Tie_configuration conf;
      conf.dir_ = get_grob_direction (ties[i]);
      conf.position_ = Tie::get_position (ties[i]);
      tie_configs.push (conf);
    }

  SCM manual_configs = me->get_property ("tie-configuration");
  bool manual_override = false;
  set_manual_tie_configuration (&tie_configs,
				&manual_override,
				manual_configs);
  set_tie_config_directions (&tie_configs);

  Grob *common = me;
  for (int i = 0; i < ties.size (); i++)
    {
      common = dynamic_cast<Spanner*> (ties[i])->get_bound (LEFT)->common_refpoint (common, X_AXIS); 
      common = dynamic_cast<Spanner*> (ties[i])->get_bound (RIGHT)->common_refpoint (common, X_AXIS); 
    }

  Drul_array< Array<Skyline_entry> > skylines;
  set_chord_outlines (&skylines, ties, common);
  
  Tie_details details;
  details.init (ties[0]);

  /*
    Let the ties flow out, according to our single-tie formatting.
   */
  if (!manual_override)
    {
      Tie::get_configuration (ties[0], common, &tie_configs.elem_ref (0),
			      &skylines,
			      details
			      );
      Tie::get_configuration (ties.top (), common,
			      &tie_configs.elem_ref (tie_configs.size()-1),
			      &skylines,
			      details
			      );
    }

  /*
    Calculate final width and shape of the ties.
   */
  for (int i = 0; i < ties.size(); i++)
    {
      if (!manual_override
	  && (i == 0 || i == ties.size () -1))
	continue;


      final_shape_adjustment (tie_configs[i],
			      skylines,
			      ties[0],
			      details);
    }

  
  /*
    Try to shift small ties into available spaces.
   */
  if (!manual_override)
    {
      shift_small_ties (&tie_configs, ties[0], details);
    }
  
  for (int i = 0; i < ties.size(); i++)
    {
      Tie::set_control_points (ties[i], common, tie_configs[i],
			       details
			       );
      set_grob_direction (ties[i], tie_configs[i].dir_);
    }
}



ADD_INTERFACE (Tie_column, "tie-column-interface",
	       "Object that sets directions of multiple ties in a tied chord",

	       /* properties */
	       "positioning-done "
	       "tie-configuration "
	       );

