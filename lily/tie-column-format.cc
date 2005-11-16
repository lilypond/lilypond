/*
  tie-column-format.cc -- implement formatting routines for Tie_column

  source file of the GNU LilyPond music typesetter

  (c) 2005 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "stem.hh"
#include "note-head.hh"
#include "tie.hh"
#include "parray.hh"
#include "spanner.hh"
#include "item.hh"
#include "staff-symbol-referencer.hh"
#include "directional-element-interface.hh"
#include "rhythmic-head.hh"
#include "tie-formatting-problem.hh"

#include <set>

void
set_manual_tie_configuration (Ties_configuration *ties_config,
			      bool *manual_override,
			      SCM manual_configs
			      )
{
  *manual_override = false;
  int k = 0;
  for (SCM s = manual_configs;
       scm_is_pair (s) && k < ties_config->ties_.size(); s = scm_cdr (s))
    {
      SCM entry = scm_car (s);
      if (!scm_is_pair (entry))
	continue;

      *manual_override = true;
      Tie_configuration &conf = ties_config->ties_.elem_ref (k);
      
      Real complete_pos = robust_scm2double (scm_car (entry),
					     conf.position_);

      conf.position_ = int (rint (complete_pos));
      conf.delta_y_ = complete_pos - conf.position_;
      conf.dir_ = Direction (robust_scm2int (scm_cdr (entry),
					     conf.dir_));
      k ++;
    }
}

void
shift_small_ties (Ties_configuration *tie_configs,
		  Grob *staff_referencer,
		  Tie_details const &details)
{
  set<int> positions_taken;
  for (int i = 0; i < tie_configs->ties_.size (); i++)
    positions_taken.insert (int (rint (tie_configs->ties_.elem (i).position_)));

  for (int i = 0; i < tie_configs->ties_.size (); i++)
    {
      Tie_configuration * conf = &tie_configs->ties_.elem_ref (i);

      /*
	on staff line and small enough, translate a little further 
      */
      Real h = conf->height (details);
      bool next_free = positions_taken.find (int (rint (conf->position_ + conf->dir_)))
	== positions_taken.end ();

      int rounded_pos = int (rint (conf->position_ + conf->delta_y_ / details.staff_space_));
      bool on_line = Staff_symbol_referencer::on_staffline (staff_referencer, rounded_pos);
      
      if (next_free)
	if (on_line && h < 0.4 * details.staff_space_)
	  {
	    positions_taken.insert (int (rint (conf->position_ + conf->dir_)));
	    conf->delta_y_ += 0.2 * details.staff_space_ * conf->dir_;
	  }
	else if (!on_line && h > 0.6 * details.staff_space_)
	  {
	    positions_taken.insert (int (rint (conf->position_ + conf->dir_)));
	    conf->delta_y_ += 0.5 * details.staff_space_ * conf->dir_;
	  }
    }
}


void
final_shape_adjustment (Tie_configuration &conf,
			Tie_formatting_problem const &problem,
			Grob *staff_referencer)
{
  Tie_details const &details (problem.details_);
  Real line_dy = 0.0;
  bool on_line = Staff_symbol_referencer::on_staffline (staff_referencer,
							int (rint (conf.position_)));
  if (on_line)
    line_dy = - sign (conf.height (details) - 0.6 * details.staff_space_)
      * 0.2 * details.staff_space_ * conf.dir_;

  Real y = conf.position_ * details.staff_space_ * 0.5
    + line_dy;
  
  conf.attachment_x_ = problem.get_attachment (y);
  conf.attachment_x_.intersect (problem.get_attachment (y + conf.dir_ * details.staff_space_ * 0.5));

  conf.delta_y_ += line_dy;
  conf.attachment_x_.widen (-details.x_gap_);
  if (!on_line
      && Staff_symbol_referencer::staff_radius (staff_referencer) * details.staff_space_ > y)
    conf.center_tie_vertically (details);
}

void
set_tie_config_directions (Ties_configuration *tie_configs_ptr)
{
  Array<Tie_configuration> &tie_configs (tie_configs_ptr->ties_);
  
  if (!tie_configs[0].dir_)
    tie_configs[0].dir_ = DOWN;
  if (!tie_configs.top().dir_)
    tie_configs.top().dir_ = UP;

  /*
    Seconds
   */
  for (int i = 1; i < tie_configs.size(); i++)
    {
      Real diff = tie_configs[i-1].position_
	- tie_configs[i].position_;
      
      if (fabs (diff) <= 1)
	{
	  if (!tie_configs[i-1].dir_)
	    tie_configs[i-1].dir_ = DOWN;
	  if (!tie_configs[i].dir_)
	    tie_configs[i].dir_ = UP;
	}
    }

  for (int i = 1; i < tie_configs.size() - 1; i++)
    {
      Tie_configuration &conf = tie_configs.elem_ref (i);
      if (conf.dir_)
	continue;

      Direction position_dir =
	Direction (sign (conf.position_));
      if (!position_dir)
	position_dir = DOWN;

      conf.dir_ = position_dir;
    }
}
			   
