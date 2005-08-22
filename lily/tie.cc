/*
  tie.cc -- implement Tie

  source file of the GNU LilyPond music typesetter

  (c) 1997--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "tie.hh"

#include <math.h>

#include "spanner.hh"
#include "lookup.hh"
#include "output-def.hh"
#include "rhythmic-head.hh"
#include "bezier.hh"
#include "paper-column.hh"
#include "warn.hh"
#include "staff-symbol-referencer.hh"
#include "directional-element-interface.hh"
#include "bezier.hh"
#include "stem.hh"
#include "note-head.hh"
#include "tie-column.hh"

/*
  tie: Connect two noteheads.

  What if we have

  c4 ~ \clef bass ; c4 or

  c4 \staffchange c4

  do we have non-horizontal ties then?
*/

void
Tie::set_head (Grob *me, Direction d, Grob *h)
{
  dynamic_cast<Spanner *> (me)->set_bound (d, h);
  me->add_dependency (h);
}

Grob *
Tie::head (Grob *me, Direction d)
{
  Item *it = dynamic_cast<Spanner*> (me)->get_bound (d);
  if (Note_head::has_interface (it))
    return it;
  else
    return 0;
}

int
Tie::get_column_rank (Grob *me, Direction d)
{
  Spanner *span = dynamic_cast<Spanner *> (me);
  Grob *h = head (me, d);
  if (!h)
    h = span->get_bound (d);

  Grob *col = dynamic_cast<Item *> (h)->get_column ();
  return Paper_column::get_rank (col);
}

int
Tie::get_position (Grob *me)
{
  Direction d = head (me, LEFT) ? LEFT : RIGHT;
  return (int) Staff_symbol_referencer::get_position (head (me, d));
}

/*
  Default:  Put the tie oppositie of the stem [Wanske p231]

  In case of chords: Tie_column takes over

  The direction of the Tie is more complicated (See [Ross] p136 and
  further).

  (what about linebreaks? )
*/
Direction
Tie::get_default_dir (Grob *me)
{
  Item *sl = head (me, LEFT) ? Rhythmic_head::get_stem (head (me, LEFT)) : 0;
  Item *sr = head (me, RIGHT) ? Rhythmic_head::get_stem (head (me, RIGHT)) : 0;
  if (sl && sr)
    {
      if (get_grob_direction (sl) == UP
	  && get_grob_direction (sr) == UP)
	return DOWN;
    }
  else if (sl || sr)
    {
      Item *s = sl ? sl : sr;
      return -get_grob_direction (s);
    }

  return UP;
}

void
Tie::set_direction (Grob *me)
{
  if (!get_grob_direction (me))
    {
      if (Tie_column::has_interface (me->get_parent (Y_AXIS)))
	Tie_column::set_directions (me->get_parent (Y_AXIS));
      else
	set_grob_direction (me, Tie::get_default_dir (me));
    }
}

Interval
get_default_attachments (Spanner *me, Grob *common, Real gap,
			 int *staff_position,
			 bool *in_between
			 )
{
  Real staff_space = Staff_symbol_referencer::staff_space (me);
  Direction dir = get_grob_direction (me);
  Interval attachments;
  Direction d = LEFT;
  do
    {
      attachments[d]
	= robust_relative_extent (me->get_bound (d),
				  common,
				  X_AXIS)[-d]
	- gap * d;
    }
  while (flip (&d) != LEFT);

  if (attachments.length () < 0.6 * staff_space)
    {
      /*
	Let short ties start over note heads, instead of between.
      */
      Drul_array<bool> allow (true, true);

      Direction d = LEFT;
      do {
	if (Note_head::has_interface (me->get_bound (d)))
	  {
	    Grob *stem = unsmob_grob (me->get_bound (d)->get_object ("stem"));
	    if (get_grob_direction (stem) == dir
		&& -d == dir)
	      allow[d] = false;
	  }
      } while (flip (&d) != LEFT);

      if (allow[LEFT] && allow[RIGHT])
	{
	  *staff_position += dir;
	  do
	    {
	      if (Note_head::has_interface (me->get_bound (d)))
		{
		  Interval extent
		    = robust_relative_extent (me->get_bound (d),
					      common, X_AXIS);

		  attachments[d] = extent.linear_combination (- 0.5 * d);
		  *in_between = false;
		}
	    }
	  while (flip (&d) != LEFT);
	}
    }

  return attachments;
}  

Interval
get_skyline_attachment (Drul_array< Array < Skyline_entry > > const &skylines,
			Real y)
{
  Interval attachments;
  Direction d = LEFT;
  do
    {
      attachments[d] = skyline_height (skylines[d], y, -d);
    }
  while (flip (&d) != LEFT);
  
  return attachments;
}
			
void
Tie::get_configuration (Grob *me_grob, Grob *common,
			Tie_configuration *conf,
			Drul_array< Array < Skyline_entry > > const *skylines,
			Tie_details const &details
			)
{
  Spanner *me = dynamic_cast<Spanner*> (me_grob);
  if (!head (me, LEFT) && !head (me, RIGHT))
    {
      programming_error ("tie without heads");
      me->suicide ();
      return ;
    }

  Direction dir = CENTER;
  
  int tie_position = (int) Tie::get_position (me);
  int staff_position = (int) conf->position_;

  if (conf->dir_)
    {
      dir = conf->dir_;
    }
  else
    {
      dir = get_grob_direction (me);
      if (!dir)
	dir = get_default_dir (me);
    }

  Real staff_space = details.staff_space_;

  bool in_between = true;
  Interval attachments = conf->attachment_x_;
  Real gap = robust_scm2double (me->get_property ("x-gap"), 0.2);
  if (attachments.is_empty())
    {
      if (!skylines)
	attachments = get_default_attachments (me, common, gap,
					       &staff_position,
					       &in_between);
      else
	{
	  Real y = staff_space * 0.5 * staff_position;
	  attachments = get_skyline_attachment (*skylines, y);
	  attachments.widen (-gap);
	}
    }

  Bezier b = slur_shape (attachments.length(),
			 details.height_limit_,
			 details.ratio_);
  b.scale (1, dir);
  
  Offset middle = b.curve_point (0.5);
  Offset edge = b.curve_point (0.0);

  staff_position = int (rint (staff_position));
  
  Real dy = fabs (middle[Y_AXIS] - edge[Y_AXIS]);
  bool in_space = !(Staff_symbol_referencer::on_staffline (me, (int) staff_position));
  bool fits_in_space =
    (dy < 0.6 * staff_space);
  
  /*
    Avoid dot
   */
  Grob *left_dot = unsmob_grob (me->get_bound (LEFT)->get_object ("dot"));
  if (left_dot && in_space)
    {
      if (staff_position == Staff_symbol_referencer::get_position (left_dot))
	{
	  staff_position += dir;
	  in_space = false;
	}
    }

  /*
    Avoid flag.
   */
  Grob *left_stem = unsmob_grob (me->get_bound (LEFT)->get_object ("stem"));
  if (left_stem)
    {
      Stencil flag = Stem::get_translated_flag (left_stem);
      Real y = staff_position * staff_space * 0.5;
      if (flag.extent (Y_AXIS).contains (y))
	{
	  staff_position += dir;
	  in_space = !in_space;
	}
    }

  if (in_space != fits_in_space)
    {
      if (in_space)
	{
	  staff_position += dir;
	}
      else
	{
	  in_space = true;
	  staff_position += dir;
	}
    }


  /*
    Putting larger in-space ties next to the notes forces
    the edges to be opposite (Y-wise) to the tie direction.
   */
  if (staff_position == tie_position
      && in_space
      && dy > 0.3 * staff_space)
    {
      staff_position += 2 * dir; 
    }

  if (!in_between
      && in_space
      && fabs (staff_position - tie_position) <= 1)
    staff_position += 2*dir;
  
  
  if (in_space)
    {
      if (fabs (dy) < 0.45 * staff_space)
	{
	  /*
	    vertically center in space.
	  */
	  conf->attachment_x_ = attachments;
	  conf->center_tie_vertically(details);
	}
      else
	{
	  conf->delta_y_ = 
	    dir * staff_space * (- 0.3);
	}
    }
  else
    {
      Real where = 0.5 * dir;
      
      Real rounding_dy = (where - middle[Y_AXIS]);
      conf->delta_y_ = rounding_dy;

      if (dir * b.curve_point (0.0)[Y_AXIS] <
	  dir * tie_position * 0.5 * staff_space)
	conf->delta_y_ +=  staff_space * dir; 
    }

  conf->dir_ = dir;
  conf->position_ = staff_position;

  if (skylines)
    {
      Real y = staff_space * 0.5 * staff_position;
      attachments = get_skyline_attachment (*skylines, y);
      attachments.widen (-gap);
    }
  conf->attachment_x_ = attachments;
}


void
Tie::set_default_control_points (Grob *me_grob)
{
  Spanner *me = dynamic_cast<Spanner*> (me_grob);
  Grob *common  = me;
  common = me->get_bound (LEFT)->common_refpoint (common, X_AXIS); 
  common = me->get_bound (RIGHT)->common_refpoint (common, X_AXIS); 
  
  Tie_configuration conf;
  if (!get_grob_direction (me))
    set_grob_direction (me, get_default_dir (me));

  int tie_position = (int) Tie::get_position (me);
  conf.position_ = tie_position;
  
  Tie_details details;
  details.init (me);
  get_configuration (me, common, &conf, 0, details);
  set_control_points (me, common, conf, details);
}

void
Tie::set_control_points (Grob *me,
			 Grob *common,
			 Tie_configuration const &conf,
			 Tie_details const &details
			 )
{
  Bezier b = conf.get_bezier (details);
  b.scale (1, conf.dir_);
  b.translate (Offset (conf.attachment_x_[LEFT]
		       - me->relative_coordinate (common, X_AXIS),
		       0.5 * conf.position_ * details.staff_space_
		       + conf.delta_y_
		       ));
  
  SCM controls = SCM_EOL;
  for (int i = 4; i--;)
    controls = scm_cons (ly_offset2scm (b.control_[i]), controls);

  me->set_property ("control-points", controls);
}



MAKE_SCHEME_CALLBACK (Tie, print, 1);
SCM
Tie::print (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  if (CENTER == get_grob_direction (me))
    set_direction (me);
      
  SCM cp = me->get_property ("control-points");
  if (!scm_is_pair (cp))
    {
      set_default_control_points (me);
      cp = me->get_property ("control-points");
    }

  if (!scm_is_pair (cp))
    return Stencil ().smobbed_copy ();

  Real staff_thick = Staff_symbol_referencer::line_thickness (me);
  Real base_thick = robust_scm2double (me->get_property ("thickness"), 1);
  Real thick = base_thick * staff_thick;

  Bezier b;
  int i = 0;
  for (SCM s = cp; s != SCM_EOL; s = scm_cdr (s))
    {
      b.control_[i] = ly_scm2offset (scm_car (s));
      i++;
    }

  Stencil a;

  SCM p = me->get_property ("dash-period");
  SCM f = me->get_property ("dash-fraction");
  if (scm_is_number (p) && scm_is_number (f))
    a = Lookup::dashed_slur (b,
			     thick,
			     robust_scm2double (p, 1.0),
			     robust_scm2double (f, 0));
  else
    a = Lookup::slur (b,
		      get_grob_direction (me) * staff_thick,
		      thick);

  return a.smobbed_copy ();
}

ADD_INTERFACE (Tie,
	       "tie-interface",
	       
	       "A tie connecting two noteheads.\n",
	       
	       "control-points "
	       "dash-fraction "
	       "dash-period "
	       "details "
	       "direction "
	       "thickness "
	       "x-gap ");
