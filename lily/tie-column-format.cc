/*
  tie-column-format.cc -- implement formatting routines for Tie_column

  source file of the GNU LilyPond music typesetter

  (c) 2005--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "tie-configuration.hh"

#include <set>


void
shift_small_ties (Ties_configuration *tie_configs,
		  Grob *staff_referencer,
		  Tie_details const &details)
{
  set<int> positions_taken;
  for (int i = 0; i < tie_configs->size (); i++)
    positions_taken.insert (int (rint (tie_configs->elem (i).position_)));

  for (int i = 0; i < tie_configs->size (); i++)
    {
      Tie_configuration * conf = &tie_configs->elem_ref (i);

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

			   
