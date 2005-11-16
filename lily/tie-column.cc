/*
  tie-column.cc -- implement Tie_column

  source file of the GNU LilyPond music typesetter

  (c) 2000--2005 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

  Ties_configuration ties_config;
  for (int i = 0; i < ties.size (); i++)
    {
      Tie_configuration conf;
      if (scm_is_number (ties[i]->get_property_data (ly_symbol2scm ("direction"))))
	conf.dir_ = get_grob_direction (ties[i]);
      conf.position_ = Tie::get_position (ties[i]);
      ties_config.ties_.push (conf);
    }

  SCM manual_configs = me->get_property ("tie-configuration");
  bool manual_override = false;
  set_manual_tie_configuration (&ties_config,
				&manual_override,
				manual_configs);
  set_tie_config_directions (&ties_config);

  Tie_formatting_problem problem;
  problem.from_ties (ties);
  
  /*
    Let the ties flow out, according to our single-tie formatting.
   */
  if (!manual_override)
    {
      Tie::get_configuration (ties[0], &ties_config.ties_.elem_ref (0),
			      problem);
      Tie::get_configuration (ties.top (), 
			      &ties_config.ties_.elem_ref (ties_config.ties_.size()-1),
			      problem);
    }

  /*
    Calculate final width and shape of the ties.
   */
  for (int i = 0; i < ties.size(); i++)
    {
      if (!manual_override
	  && (i == 0 || i == ties.size () -1))
	continue;


      final_shape_adjustment (ties_config.ties_[i],
			      problem,
			      ties[0]);
    }

  
  /*
    Try to shift small ties into available spaces.
   */
  if (!manual_override)
    {
      shift_small_ties (&ties_config, ties[0], problem.details_);
    }
  
  for (int i = 0; i < ties.size(); i++)
    {
      Tie::set_control_points (ties[i], problem.common_x_refpoint (), ties_config.ties_[i],
			       problem.details_);
      set_grob_direction (ties[i], ties_config.ties_[i].dir_);
    }
  return SCM_BOOL_T;
}



ADD_INTERFACE (Tie_column, "tie-column-interface",
	       "Object that sets directions of multiple ties in a tied chord",

	       /* properties */
	       "positioning-done "
	       "tie-configuration "
	       );

