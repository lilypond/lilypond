/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "hairpin.hh"

#include "dimensions.hh"
#include "international.hh"
#include "line-interface.hh"
#include "output-def.hh"
#include "paper-column.hh"
#include "pointer-group-interface.hh"
#include "spanner.hh"
#include "staff-symbol-referencer.hh"
#include "text-interface.hh"
#include "note-column.hh"
#include "warn.hh"

MAKE_SCHEME_CALLBACK (Hairpin, pure_height, 3);
SCM
Hairpin::pure_height (SCM smob, SCM, SCM)
{
  Grob *me = unsmob_grob (smob);
  Real height = robust_scm2double (me->get_property ("height"), 0.0)
    * Staff_symbol_referencer::staff_space (me);

  Real thickness = robust_scm2double (me->get_property ("thickness"), 1)
    * Staff_symbol_referencer::line_thickness (me);

  height += thickness / 2;
  return ly_interval2scm (Interval (-height, height));
}

MAKE_SCHEME_CALLBACK (Hairpin, print, 1);
SCM
Hairpin::print (SCM smob)
{
  Spanner *me = unsmob_spanner (smob);

  SCM s = me->get_property ("grow-direction");
  if (!is_direction (s))
    {
      me->suicide ();
      return SCM_EOL;
    }

  Direction grow_dir = to_dir (s);
  Real padding = robust_scm2double (me->get_property ("bound-padding"), 0.5);

  Drul_array<bool> broken;
  Drul_array<Item *> bounds;
  Direction d = LEFT;
  do
    {
      bounds[d] = me->get_bound (d);
      broken[d] = bounds[d]->break_status_dir () != CENTER;
    }
  while (flip (&d) != LEFT);

  broken[RIGHT] = broken[RIGHT] && me->broken_neighbor (RIGHT);
  broken[RIGHT] = broken[RIGHT] && me->broken_neighbor (RIGHT)->is_live ();

  if (broken[RIGHT])
    {
      Spanner *next = me->broken_neighbor (RIGHT);
      Stencil *s = next->get_stencil ();
      if (!s || s->is_empty ())
	broken[RIGHT] = false;
    }

  Grob *common = bounds[LEFT]->common_refpoint (bounds[RIGHT], X_AXIS);
  Drul_array<Real> x_points;

  /*
    Use the height and thickness of the hairpin when making a circled tip
  */
  bool circled_tip = ly_scm2bool (me->get_property ("circled-tip"));
  Real height = robust_scm2double (me->get_property ("height"), 0.2)
    * Staff_symbol_referencer::staff_space (me);
  /*
    FIXME: 0.525 is still just a guess...
  */
  Real rad = height * 0.525;
  Real thick = 1.0;
  if (circled_tip)
    thick = robust_scm2double (me->get_property ("thickness"), 1.0)
      * Staff_symbol_referencer::line_thickness (me);

  do
    {
      Item *b = bounds[d];
      x_points[d] = b->relative_coordinate (common, X_AXIS);
      if (broken [d])
	{
	  if (d == LEFT)
	    x_points[d] = b->extent (common, X_AXIS)[RIGHT];
	}
      else
	{
	  if (Text_interface::has_interface (b))
	    {
	      Interval e = b->extent (common, X_AXIS);
	      if (!e.is_empty ())
		x_points[d] = e[-d] - d * padding;
	    }
	  else
	    {
	      bool neighbor_found = false;
	      Spanner *adjacent;
	      extract_grob_set (me, "adjacent-spanners", neighbors);
	      for (vsize i = 0; i < neighbors.size (); i++)
		{
		  /*
		    FIXME: this will fuck up in case of polyphonic
		    notes in other voices. Need to look at note-columns
		    in the current staff/voice.
		  */
		  adjacent = dynamic_cast<Spanner *> (neighbors[i]);
		  if (adjacent
		      && (adjacent->get_bound (-d)->get_column ()
			  == b->get_column ()))
		    {
		      neighbor_found = true;
		      break;
		    }
		}

	      Interval e = robust_relative_extent (b, common, X_AXIS);
	      if (neighbor_found)
		{
		  if (Hairpin::has_interface (adjacent))
		    {
		      /*
			Handle back-to-back hairpins with a circle in the middle
		      */
		      if (circled_tip && (grow_dir != d))
			x_points[d] = e.center () + d * (rad - thick / 2.0);
		      /*
			If we're hung on a paper column, that means we're not
			adjacent to a text-dynamic, and we may move closer. We
			make the padding a little smaller, here.
		      */
		      else
			x_points[d] = e.center () - d * padding / 3;
		    }
		  // Our neighbor is a dynamic text spanner, so add the
		  // same amount of padding as for text dynamics
		  else
		    x_points[d] = e[-d] - d * padding;
		}
	      else
		{
		  if (Note_column::has_interface (b)
		      && Note_column::has_rests (b))
		    x_points[d] = e[-d];
		  else
		    x_points[d] = e[d];

		  Item *bound = me->get_bound (d);
		  if (bound->is_non_musical (bound))
		    x_points[d] -= d * padding;
		}
	    }
	}
    }
  while (flip (&d) != LEFT);

  Real width = x_points[RIGHT] - x_points[LEFT];
  if (width < 0)
    {
      me->warning (_ ((grow_dir < 0) ? "decrescendo too small"
		      : "crescendo too small"));
      width = 0;
    }

  bool continued = broken[Direction (-grow_dir)];

  Real starth = 0;
  Real endh = 0;
  if (grow_dir < 0)
    {
      starth = height;
      endh = continued ? height / 2 : 0.0;
    }
  else
    {
      starth = continued ? height / 2 : 0.0;
      endh = height;
    }

  /*
    should do relative to staff-symbol staff-space?
  */
  Stencil mol;
  Real x = 0.0;

  /*
    Compensate for size of circle
  */
  Direction tip_dir = -grow_dir;
  if (circled_tip && !broken[tip_dir])
    {
      if (grow_dir > 0)
	x = rad * 2.0;
      else if (grow_dir < 0)
	width -= rad *2.0;
    }
  mol = Line_interface::line (me, Offset (x, starth), Offset (width, endh));
  mol.add_stencil (Line_interface::line (me,
					 Offset (x, -starth),
					 Offset (width, -endh)));

  /*
    Support al/del niente notation by putting a circle at the
    tip of the (de)crescendo.
  */
  if (circled_tip)
    {
      Box extent (Interval (-rad, rad), Interval (-rad, rad));

      /* Hmmm, perhaps we should have a Lookup::circle () method? */
      Stencil circle (extent,
		      scm_list_4 (ly_symbol2scm ("circle"),
				  scm_from_double (rad),
				  scm_from_double (thick),
				  SCM_BOOL_F));

      /*
	don't add another circle if the hairpin is broken
      */
      if (!broken[tip_dir])
	mol.add_at_edge (X_AXIS, tip_dir, Stencil (circle), 0);
    }

  mol.translate_axis (x_points[LEFT]
		      - bounds[LEFT]->relative_coordinate (common, X_AXIS),
		      X_AXIS);
  return mol.smobbed_copy ();
}

ADD_INTERFACE (Hairpin,
	       "A hairpin crescendo or decrescendo.",

	       /* properties */
	       "adjacent-spanners "
	       "circled-tip "
	       "bound-padding "
	       "grow-direction "
	       "height "
	       );
