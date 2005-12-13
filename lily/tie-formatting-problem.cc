/*
  tie-formatting-problem.cc -- implement Tie_formatting_problem6

  source file of the GNU LilyPond music typesetter

  (c) 2005 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "tie-formatting-problem.hh"

#include "directional-element-interface.hh"
#include "staff-symbol-referencer.hh"
#include "tie.hh"

#include "item.hh"
#include "spanner.hh" 
#include "bezier.hh" 
#include "stem.hh"
#include "note-head.hh"
#include "rhythmic-head.hh"

Interval
Tie_formatting_problem::get_attachment (Real y) const
{
  Interval attachments;
  Direction d = LEFT;
  do
    {
      attachments[d] = skyline_height (chord_outlines_[d], y, -d);
    }
  while (flip (&d) != LEFT);
  
  return attachments;
}

Tie_formatting_problem::Tie_formatting_problem()
{
  x_refpoint_ = 0;
}

Tie_formatting_problem::~Tie_formatting_problem ()
{
  for (Tie_configuration_map::const_iterator i (possibilities_.begin ());
       i != possibilities_.end (); i++)
    delete (*i).second;
}

void
Tie_formatting_problem::set_chord_outline (Link_array<Item> bounds,
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

      Interval x = head->extent (x_refpoint_, X_AXIS);
      boxes.push (Box (x, y));

      Grob *dots = Rhythmic_head::get_dots (head);
      if (d == LEFT && dots)
	{
	  Interval x = dots->extent (x_refpoint_, X_AXIS);
	  Interval y (-0.5, 0.5);
	  int p = int (Staff_symbol_referencer::get_position (dots));
	  y.translate (p);

	  dot_positions_.insert (p);
	  dot_x_.unite (x);
	  
	  y *= staff_space * 0.5;
	  // boxes.push (Box (x, y));
	}
    }

  chord_outlines_[d] = empty_skyline (-d);

  if (bounds[0]->break_status_dir ())
    {
      Real x = robust_relative_extent (bounds[0],  x_refpoint_, X_AXIS)[-d];
      chord_outlines_[d].elem_ref (0).height_ = x; 
    }
	  
  for (int i = 0; i < boxes.size (); i++)
    insert_extent_into_skyline (&chord_outlines_[d]  ,
				boxes[i], Y_AXIS, -d);

  if (stem
      && !Stem::is_invisible (stem))
    {
      Interval x;
      x.add_point (stem->relative_coordinate (x_refpoint_, X_AXIS));
      x.widen (staff_space / 20); // ugh.
      Interval y;
      y.add_point (Stem::stem_end_position (stem) * staff_space * .5);

      Direction stemdir = get_grob_direction (stem);
      y.add_point (Stem::head_positions (stem)[-stemdir]
		   * staff_space * .5);
	  
      insert_extent_into_skyline (&chord_outlines_[d], Box (x,y), Y_AXIS, -d);



      if (d == LEFT)
	{
	  Box flag_box = Stem::get_translated_flag (stem).extent_box ();
	  flag_box.translate( Offset (x[RIGHT], X_AXIS));
	  insert_extent_into_skyline (&chord_outlines_[d], flag_box,
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
	insert_extent_into_skyline (&chord_outlines_[d],
				    Box (x,y),
				    Y_AXIS, -d);
    }
  while (flip (&updowndir) != DOWN);
}


void
Tie_formatting_problem::from_tie (Grob *tie)
{
  Link_array<Grob> ties;
  ties.push (tie);

  from_ties (ties);

  details_.from_grob (tie);
}

Grob *
Tie_formatting_problem::common_x_refpoint () const
{
  return x_refpoint_;
}

void
Tie_formatting_problem::from_ties (Link_array<Grob> const &ties)
{
  if (ties.is_empty ())
    return;
  
  x_refpoint_ = ties[0];
  for (int i = 0; i < ties.size (); i++)
    {
      x_refpoint_ = dynamic_cast<Spanner*> (ties[i])->get_bound (LEFT)->common_refpoint (x_refpoint_, X_AXIS); 
      x_refpoint_ = dynamic_cast<Spanner*> (ties[i])->get_bound (RIGHT)->common_refpoint (x_refpoint_, X_AXIS); 
    }

  details_.from_grob (ties[0]);
  
  Direction d = LEFT;
  do
    {
      Link_array<Item> bounds;
      
      for (int i = 0; i < ties.size (); i++)
	{
	  Item *it = dynamic_cast<Spanner*> (ties[i])->get_bound (d);
					     
	  bounds.push (it);
	}
      
      set_chord_outline (bounds, d);
    }
  while (flip (&d) != LEFT);
}

void
Tie_formatting_problem::from_lv_ties (Link_array<Grob> const &lv_ties)
{
  if (lv_ties.is_empty ())
    return ;
  
  details_.from_grob (lv_ties[0]);
  Link_array<Item> heads;
  for (int i = 0; i < lv_ties.size (); i++)
    {
      Item *head = unsmob_item (lv_ties[i]->get_object ("note-head"));
      if (!head)
	continue;
      
      heads.push (head);
    }

  x_refpoint_ = lv_ties [0];
  for (int i = 0; i < lv_ties.size (); i++)
    {
      x_refpoint_ = lv_ties[i]->common_refpoint (x_refpoint_, X_AXIS); 
    }

  set_chord_outline (heads, LEFT);

  Real right_most = - infinity_f;   

  for (int i = 0; i < chord_outlines_[LEFT].size (); i++)
    {
      right_most = max (right_most, chord_outlines_[LEFT][i].height_);
    }

  Skyline_entry right_entry;
  right_entry.width_.set_full ();
  right_entry.height_ = right_most + 1.5;
  
  chord_outlines_[RIGHT].push (right_entry);
}

Tie_configuration*
Tie_formatting_problem::get_configuration (int pos, Direction dir)
{
  pair<int,int> key (pos, dir);
  Tie_configuration_map::const_iterator f = possibilities_.find (key);
							      
  if (f != possibilities_.end ())
    {
      return (*f).second;
    }

  
  Tie_configuration *conf = generate_configuration (pos,dir);
  possibilities_[key] = conf;
  return conf;
}

Tie_configuration*
Tie_formatting_problem::generate_configuration (int pos, Direction dir)
{
  Tie_configuration *conf = new Tie_configuration;
  conf->position_ = pos;
  conf->dir_ = dir;
  Real y = conf->position_ * 0.5 * details_.staff_space_;

  if (dot_positions_.find (pos) != dot_positions_.end ())
    {
      conf->delta_y_ += 0.25 * details_.staff_space_;
    }
  
  conf->attachment_x_ = get_attachment (y + conf->delta_y_);

  Real h =  conf->height (details_);
  if (!conf->delta_y_)
    {
      if (h < 0.5 * details_.staff_space_
	  && !Staff_symbol_referencer::on_staffline (details_.staff_symbol_referencer_, pos))
	{
	  conf->center_tie_vertically (details_);
	}
      else if (h < 0.5 * details_.staff_space_
	       && Staff_symbol_referencer::on_staffline (details_.staff_symbol_referencer_, pos))
	{
	  conf->delta_y_ += dir * 0.2 * details_.staff_space_;
	}
    }
  
  conf->attachment_x_.widen ( - details_.x_gap_);
  return conf;
}

Real
Tie_formatting_problem::score_aptitude (Tie_configuration const &conf,
					int tie_position)
{
  Real wrong_direction_offset_penalty_;
  Real distance_penalty_factor_;
				  
  wrong_direction_offset_penalty_ = 10;
  distance_penalty_factor_ = 5;
  
  Real penalty = 0.0;
  Real curve_y = conf.position_ * details_.staff_space_ * 0.5 + conf.delta_y_;
  Real tie_y = tie_position * details_.staff_space_ * 0.5;
  if (sign (curve_y - tie_y) != conf.dir_)
    penalty += wrong_direction_offset_penalty_;

  penalty += distance_penalty_factor_ * fabs (curve_y - tie_y);
  return penalty;
}


Real
Tie_formatting_problem::score_configuration (Tie_configuration const &conf)
{
  Real length_penalty_factor = 1.0;
  Real min_length = 0.333;
  Real staff_line_clearance = 0.1;
  Real staff_line_collision_penalty = 5;
  Real dot_collision_clearance = 0.25;
  Real dot_collision_penalty = 10;
  

  Real penalty = 0.0;
  Real length = conf.attachment_x_.length ();
  if (length < min_length)
    penalty += length_penalty_factor / max (0.01, length);

  Real tip_pos = conf.position_ + conf.delta_y_ / 0.5 * details_.staff_space_;
  Real tip_y = tip_pos * details_.staff_space_ * 0.5;
  Real height =  conf.height (details_);

  Real top_y = tip_y + conf.dir_ * height;
  Real top_pos = 2 * top_y / details_.staff_space_;
  Real round_top_pos = rint (top_pos);
  if (fabs (top_pos - round_top_pos) < staff_line_clearance
      && Staff_symbol_referencer::on_staffline (details_.staff_symbol_referencer_,
						int (round_top_pos))
      && Staff_symbol_referencer::staff_radius (details_.staff_symbol_referencer_) > top_y)
    {
      penalty += staff_line_collision_penalty;      
    }
  
  if (fabs (tip_pos - rint (tip_pos)) < staff_line_clearance
      && Staff_symbol_referencer::on_staffline (details_.staff_symbol_referencer_,
						int (rint (tip_pos))))
    {
      penalty += staff_line_collision_penalty;
    }

  if (!dot_x_.is_empty ())
    {
      /* use left edge? */
      Real x = dot_x_.center ();
      
      Bezier b = conf.get_transformed_bezier (details_);
      if (b.control_point_extent (X_AXIS).contains (x))
	{
	  Real y = b.get_other_coordinate (X_AXIS, x);

	  for (set<int>::const_iterator i (dot_positions_.begin ());
	       i != dot_positions_.end (); i ++)
	    {
	      int dot_pos = (*i);
	      if (fabs (dot_pos * details_.staff_space_ * 0.5 - y) < dot_collision_clearance)
		{
		  penalty += dot_collision_penalty;
		}
	    }
	}      
    }
  
  return penalty;
}

Tie_configuration
Tie_formatting_problem::find_optimal_tie_configuration (int pos, Direction dir)
{
  Link_array<Tie_configuration> confs;

  int region_size = 3;
  for (int i = 0; i < region_size; i ++)
    {
      confs.push (generate_configuration (pos + i * dir, dir));
    }

  Array<Real> scores;

  int best_idx = -1;
  Real best_score = 1e6;
  for (int i = 0; i < confs.size (); i ++)
    {
      Real score = 0.0;
      score += score_configuration (*confs[i]);
      score += score_aptitude (*confs[i], pos);

      if (score < best_score)
	{
	  best_score = score;
	  best_idx = i;
	}
    }

  Tie_configuration best = *confs[best_idx];
  for (int i = 0; i < confs.size (); i++)
    delete confs[i];

  return best;
}
