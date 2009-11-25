/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2002--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "horizontal-bracket.hh"	

#include "lookup.hh"
#include "side-position-interface.hh"
#include "pointer-group-interface.hh"
#include "directional-element-interface.hh"
#include "output-def.hh"
#include "staff-symbol-referencer.hh"
#include "tuplet-bracket.hh"
#include "axis-group-interface.hh"
#include "spanner.hh"
#include "item.hh"


Stencil
Horizontal_bracket::make_bracket (Grob *me,
				  Real length,
				  Axis a, Direction dir)				 
{
  Drul_array<Real> edge_height = robust_scm2interval (me->get_property ("edge-height"),
						      Interval (1.0, 1.0));
  Drul_array<Real> flare = robust_scm2interval (me->get_property ("bracket-flare"),
						Interval (0, 0));
  Drul_array<Real> shorten = robust_scm2interval (me->get_property ("shorten-pair"),
						  Interval (0, 0));

  
  // Make sure that it points in the correct direction:
  scale_drul (&edge_height, Real (-dir));
 
  Interval empty;
  Offset start;
  start[a] = length;

  Drul_array<bool> connect_to_other =
    robust_scm2booldrul (me->get_property ("connect-to-neighbor"),
			 Drul_array<bool> (false, false));

  Direction d = LEFT;
  do
    {
      if (connect_to_other[d])
	{
	  edge_height[d] = 0.0;
	  flare[d] = 0.0;
	  shorten[d] = 0.0;
	}
    }
  while (flip (&d) != LEFT);
	      
  /*
    ugh, Tuplet_bracket should use Horizontal_bracket, not the other way around. 
  */
  return Tuplet_bracket::make_bracket (me, other_axis (a), start, 
				       edge_height, empty, flare, shorten);
}


Stencil
Horizontal_bracket::make_enclosing_bracket (Grob *me, Grob *refpoint,
					    vector<Grob*> grobs,
					    Axis a, Direction dir)
{
  Grob *common = common_refpoint_of_array (grobs, refpoint, a);
  Interval ext = Axis_group_interface::relative_group_extent (grobs, common, a);

  if (ext.is_empty ())
    {
      me->programming_error ("Can't enclose empty extents with bracket");
      return Stencil ();
    }
  else
    {
      Stencil b = make_bracket (me, ext.length (), a, dir);
      b.translate_axis (ext[LEFT] - refpoint->relative_coordinate (common, a), a);

      return b;
    }
}

/*
  TODO:

  Support texts on the brackets?
*/
MAKE_SCHEME_CALLBACK (Horizontal_bracket, print, 1);
SCM
Horizontal_bracket::print (SCM smob)
{
  Spanner *me = unsmob_spanner (smob);
  extract_grob_set (me, "columns", gs);

  vector<Grob*> enclosed = gs;
  if (!gs.size ())
    {
      me->suicide ();
      return SCM_EOL;
    }

  Direction d = LEFT;
  do
    {
      Item *b = me->get_bound (d);
      if (b->break_status_dir ())
	enclosed.push_back (b);
    }
  while (flip (&d) != LEFT);
  
  Stencil b = make_enclosing_bracket (me, me, enclosed, X_AXIS, get_grob_direction (me));
  return b.smobbed_copy ();
}

ADD_INTERFACE (Horizontal_bracket,
	       "A horizontal bracket encompassing notes.",

	       /* properties */		  
	       "bracket-flare "
	       "columns "
	       "edge-height "
	       "shorten-pair "
	       "connect-to-neighbor "
	       );

