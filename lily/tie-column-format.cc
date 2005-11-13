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

#include <set>

void
set_manual_tie_configuration (Array<Tie_configuration> *tie_configs,
			      bool *manual_override,
			      SCM manual_configs
			      )
{
  *manual_override = false;
  int k = 0;
  for (SCM s = manual_configs;
       scm_is_pair (s) && k < tie_configs->size(); s = scm_cdr (s))
    {
      SCM entry = scm_car (s);
      if (!scm_is_pair (entry))
	continue;

      *manual_override = true;
      Tie_configuration &conf = tie_configs->elem_ref (k);
      
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
set_chord_outline (Array<Skyline_entry> *skyline,
		   Link_array<Item> bounds,
		   Grob *common,
		   Direction d)
{
  Real staff_space = Staff_symbol_referencer::staff_space (bounds[0]);

  Array<Box> boxes;

  Grob *stem = 0;
  for (int i = 0; i < bounds.size (); i++)
    {
      Grob *head = bounds[i];
      if (!Note_head::has_interface (head))
	continue;
      
      if (!stem)
	stem = unsmob_grob (head->get_object ("stem"));
	  
      Real p = Staff_symbol_referencer::get_position (head);
      Interval y ((p-1) * 0.5 * staff_space,
		  (p+1) * 0.5 * staff_space);

      Interval x = head->extent (common, X_AXIS);
      boxes.push (Box (x, y));

      Grob *dots = Rhythmic_head::get_dots (head);
      if (d == LEFT && dots)
	{
	  Interval x = dots->extent (common, X_AXIS);
	  Interval y (-0.5, 0.5);
	  y.translate (Staff_symbol_referencer::get_position (dots));
	  y *= staff_space * 0.5;
	  
	  boxes.push (Box (x, y));
	}
    }

  (*skyline) = empty_skyline (-d);

  if (bounds[0]->break_status_dir ())
    {
      Real x = robust_relative_extent (bounds[0],  common, X_AXIS)[-d];
      skyline->elem_ref (0).height_ = x; 
    }
	  
  for (int i = 0; i < boxes.size (); i++)
    insert_extent_into_skyline (skyline,
				boxes[i], Y_AXIS, -d);
  if (stem
      && !Stem::is_invisible (stem))
    {
      Interval x;
      x.add_point (stem->relative_coordinate (common, X_AXIS));
      x.widen (staff_space / 20); // ugh.
      Interval y;
      y.add_point (Stem::stem_end_position (stem) * staff_space * .5);

      Direction stemdir = get_grob_direction (stem);
      y.add_point (Stem::head_positions (stem)[-stemdir]
		   * staff_space * .5);
	  
      insert_extent_into_skyline (skyline, Box (x,y), Y_AXIS, -d);



      if (d == LEFT)
	{
	  Box flag_box = Stem::get_translated_flag (stem).extent_box ();
	  flag_box.translate( Offset (x[RIGHT], X_AXIS));
	  insert_extent_into_skyline (skyline, flag_box,
				      Y_AXIS, -d);
	}
    }
  
  Direction updowndir = DOWN;
  do
    {
      Interval x ;
      Interval y;
      if (boxes.size())
	{
	  Box b = boxes.boundary (updowndir, 0);
	  x = b[X_AXIS];
	  x[-d] =  b[X_AXIS].linear_combination (-d / 2);
	  y[-updowndir] = b[Y_AXIS][updowndir];
	  y[updowndir] = updowndir * infinity_f;
	}

      if (!x.is_empty ())
	insert_extent_into_skyline (skyline,
				    Box (x,y),
				    Y_AXIS, -d);
    }
  while (flip (&updowndir) != DOWN);

  for (int i = 0; i < bounds.size (); i++)
    {
      if (!Note_head::has_interface (bounds[i]))
	continue;

      
      Grob *dots = unsmob_grob (bounds[i]->get_object ("dot"));
      if (dots && d == LEFT)
	{
	  Interval x = dots->extent (common, X_AXIS);
	  Real p = Staff_symbol_referencer::get_position (dots);
	      
	  Interval y (-1,1);
	  y *= (staff_space /4);
	  y.translate (p * staff_space * .5);

	  insert_extent_into_skyline (skyline,
				      Box (x,y), Y_AXIS, -d);
	}
    }
}

void
set_chord_outlines (Drul_array< Array<Skyline_entry> > *skyline_drul,
		    Link_array<Grob> ties,
		    Grob *common)
{
  Direction d = LEFT;

  do
    {
      Link_array<Item> bounds;
      
      for (int i = 0; i < ties.size (); i++)
	{
	  Item *it = dynamic_cast<Spanner*> (ties[i])->get_bound (d);
					     
	  bounds.push (it);
	}
      
      set_chord_outline (&skyline_drul->elem_ref (d),
			 bounds, common, d);
    }
  while (flip (&d) != LEFT);
}

void
shift_small_ties (Array<Tie_configuration> *tie_configs,
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
			Drul_array< Array<Skyline_entry> > const &skylines,
			Grob *staff_referencer,
			Tie_details const &details)
{
  Real line_dy = 0.0;
  bool on_line = Staff_symbol_referencer::on_staffline (staff_referencer,
							int (rint (conf.position_)));
  if (on_line)
    line_dy = - sign (conf.height (details) - 0.6 * details.staff_space_)
      * 0.2 * details.staff_space_ * conf.dir_;

  Real y = conf.position_ * details.staff_space_ * 0.5
    + line_dy;
  
  conf.attachment_x_ = get_skyline_attachment (skylines, y);
  conf.attachment_x_.intersect (get_skyline_attachment (skylines,
							y + conf.dir_ * details.staff_space_ * 0.5));

  conf.delta_y_ += line_dy;
  conf.attachment_x_.widen (-details.x_gap_);
  if (!on_line
      && Staff_symbol_referencer::staff_radius (staff_referencer) * details.staff_space_ > y)
    conf.center_tie_vertically (details);
}

void
set_tie_config_directions (Array<Tie_configuration> *tie_configs_ptr)
{
  Array<Tie_configuration> &tie_configs (*tie_configs_ptr);
  
  if (!tie_configs[0].dir_)
    tie_configs[0].dir_ = DOWN;
  if (!tie_configs.top().dir_)
    tie_configs.top().dir_ = UP;

  /*
    Seconds
   */
  for (int i = 1; i < tie_configs.size(); i++)
    {
      Real diff = tie_configs[i-1].position_ - tie_configs[i].position_;
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
      if (tie_configs[i].dir_)
	continue;

      Direction position_dir = (Direction) sign (tie_configs[i].position_);
      if (!position_dir)
	position_dir = DOWN;
      
      tie_configs[i].dir_ = position_dir;
    }
}
			   
