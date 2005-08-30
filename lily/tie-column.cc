/*
  tie-column.cc -- implement Tie_column

  source file of the GNU LilyPond music typesetter

  (c) 2000--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <math.h>

#include <map>
#include <set>

#include "stem.hh"
#include "skyline.hh"
#include "staff-symbol-referencer.hh"
#include "warn.hh"
#include "tie-column.hh"
#include "paper-column.hh"
#include "spanner.hh"
#include "pointer-group-interface.hh"
#include "tie.hh"
#include "directional-element-interface.hh"
#include "rhythmic-head.hh"

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

int
Tie::compare (Grob *const &s1,
	      Grob *const &s2)
{
  return sign (Tie::get_position (s1) - Tie::get_position (s2));
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
set_chord_outlines (Drul_array< Array<Skyline_entry> > *skyline_drul,
		    Link_array<Grob> ties,
		    Grob *common)
{
  Direction d = LEFT;

  Real staff_space = Staff_symbol_referencer::staff_space (ties[0]);
  do
    {
      Array<Box> boxes;
      Interval x_union;

      Grob *stem = 0; 
      for (int i = 0; i < ties.size (); i++)
	{
	  Spanner *tie = dynamic_cast<Spanner*> (ties[i]);

	  Grob *head = Tie::head (tie, d);
	  if (!head)
	    continue;

	  if (!stem)
	    stem = unsmob_grob (head->get_object ("stem"));
	  
	  Real p = Tie::get_position (tie);
	  Interval y ((p-1) * 0.5 * staff_space,
		      (p+1) * 0.5 * staff_space);

	  Interval x = head->extent (common, X_AXIS);
	  boxes.push (Box (x, y));
	  x_union.unite (x);
	}

      (*skyline_drul)[d] = empty_skyline (-d);
      
      Spanner *tie = dynamic_cast<Spanner*> (ties[0]);
      if (tie->get_bound (d)->break_status_dir ())
	{
	  Real x = robust_relative_extent (tie->get_bound (d),
					   common,
					   X_AXIS)[-d];

	  (*skyline_drul)[d].elem_ref (0).height_ = x; 
	}
	  
      for (int i = 0; i < boxes.size (); i++)
	insert_extent_into_skyline (&skyline_drul->elem_ref (d),
				    boxes[i], Y_AXIS, -d);

      Direction updowndir = DOWN;
      do
	{
	  Interval x ;
	  if (boxes.size())
	    {
	      Box b = boxes.boundary (updowndir, 0);
	      x = b[X_AXIS];
	      x[-d] =  b[X_AXIS].linear_combination (-d / 2);
	    }
	  
	  if (stem
	      && !Stem::is_invisible (stem)
	      && updowndir == get_grob_direction (stem))
	    x.unite (robust_relative_extent (stem, common, X_AXIS));

	  if (!x.is_empty ())
	    (*skyline_drul)[d].boundary (updowndir, 0).height_ = x[-d]; 
	}
      while (flip (&updowndir) != DOWN);

      for (int i = 0; i < ties.size (); i++)
	{
	  Spanner *tie = dynamic_cast<Spanner*> (ties[i]);
	  Grob *head = Tie::head (tie, d);
	  if (!head)
	    continue;

	  Grob *dots = unsmob_grob (head->get_object ("dot"));
	  if (dots && d == LEFT)
	    {
	      Interval x = dots->extent (common, X_AXIS);
	      Real p = Staff_symbol_referencer::get_position (dots);
	      
	      Interval y (-1,1);
	      y *= (staff_space /4);
	      y.translate (p * staff_space * .5);

	      insert_extent_into_skyline (&skyline_drul->elem_ref (d),
					  Box (x,y), Y_AXIS, -d);
	    }
	}
      
    }
  while (flip (&d) != LEFT);
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
  int k = 0;
  for (SCM s = manual_configs;
       scm_is_pair (s) && k < tie_configs.size(); s = scm_cdr (s))
    {
      SCM entry = scm_car (s);
      if (!scm_is_pair (entry))
	continue;

      manual_override = true;
      tie_configs[k].position_ = robust_scm2double (scm_car (entry), tie_configs[k].position_);
      tie_configs[k].dir_ = Direction (robust_scm2int (scm_cdr (entry), tie_configs[k].dir_));
      k ++;
    }

  if (!tie_configs[0].dir_)
    tie_configs[0].dir_ = DOWN;
  if (!tie_configs.top().dir_)
    tie_configs.top().dir_ = UP;

  /*
    Seconds
   */
  for (int i = 1; i < tie_configs.size(); i++)
    {
      if (fabs (tie_configs[i-1].position_ - tie_configs[i].position_) <= 1)
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
  Real staff_space = Staff_symbol_referencer::staff_space (ties[0]);
  Real gap = robust_scm2double (ties[0]->get_property ("x-gap"), 0.2);
  for (int i = 0; i < ties.size(); i++)
    {
      if (!manual_override
	  && (i == 0 || i == ties.size () -1))
	continue;
      
      Tie_configuration conf = tie_configs[i];
      conf = tie_configs[i];

      Real line_dy = 0.0;
      bool on_line = Staff_symbol_referencer::on_staffline (ties[0],
							    int (rint (conf.position_)));
      if (on_line)
	line_dy = - sign (conf.height (details) - 0.6 * staff_space)
	  * 0.2 * staff_space * conf.dir_;

      Real y = conf.position_ * staff_space * 0.5
	+ line_dy;
      conf.attachment_x_
	= get_skyline_attachment (skylines, y);
      conf.attachment_x_.intersect (get_skyline_attachment (skylines,
							    y + conf.dir_ * staff_space * .5));

      conf.delta_y_ += line_dy;
      conf.attachment_x_.widen (-gap);
      if (!on_line
	  && Staff_symbol_referencer::staff_radius (ties[0]) * staff_space > y)
	conf.center_tie_vertically (details);
	      
      tie_configs[i] = conf;
    }

  /*
    Try to shift small ties into available spaces.
   */
  if (!manual_override)
    {
      set<int> positions_taken; 
      for (int i = 0; i < tie_configs.size (); i++)
	positions_taken.insert (int (rint (tie_configs[i].position_)));

      for (int i = 0; i < tie_configs.size (); i++)
	{
	  Tie_configuration * conf = &tie_configs.elem_ref (i);
	  if (Staff_symbol_referencer::on_staffline (ties[0], int (rint (conf->position_)))
	      && conf->height (details) < 0.4 * staff_space
	      && (positions_taken.find (int (rint (conf->position_ + conf->dir_)))
		  == positions_taken.end ()))
	    {
	      positions_taken.insert (int (rint (conf->position_ + conf->dir_)));
	      conf->delta_y_ += 0.2 * staff_space * conf->dir_;
	    }
	}
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
	       "positioning-done "
	       "tie-configuration "
	       );
