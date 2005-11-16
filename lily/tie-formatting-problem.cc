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
	  y.translate (Staff_symbol_referencer::get_position (dots));
	  y *= staff_space * 0.5;
	  
	  boxes.push (Box (x, y));
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

  for (int i = 0; i < bounds.size (); i++)
    {
      if (!Note_head::has_interface (bounds[i]))
	continue;

      
      Grob *dots = unsmob_grob (bounds[i]->get_object ("dot"));
      if (dots && d == LEFT)
	{
	  Interval x = dots->extent (x_refpoint_, X_AXIS);
	  Real p = Staff_symbol_referencer::get_position (dots);
	      
	  Interval y (-1,1);
	  y *= (staff_space /4);
	  y.translate (p * staff_space * .5);

	  insert_extent_into_skyline (&chord_outlines_[d],
				      Box (x,y), Y_AXIS, -d);
	}
    }
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
  details_.from_grob (ties[0]);
  for (int i = 0; i < ties.size (); i++)
    {
      x_refpoint_ = dynamic_cast<Spanner*> (ties[i])->get_bound (LEFT)->common_refpoint (x_refpoint_, X_AXIS); 
      x_refpoint_ = dynamic_cast<Spanner*> (ties[i])->get_bound (RIGHT)->common_refpoint (x_refpoint_, X_AXIS); 
    }
  
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
