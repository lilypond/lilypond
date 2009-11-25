/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>


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

#include "semi-tie-column.hh"
#include "semi-tie.hh"
#include "directional-element-interface.hh"
#include "grob.hh"
#include "tie.hh"
#include "warn.hh"
#include "staff-symbol-referencer.hh"

ADD_INTERFACE (Semi_tie,
	      "A tie which is only on one side connected to a note head.",

	      /* properties */
	      "control-points "
	      "direction "
	      "details "
	      "head-direction "
	      "note-head "
	      "thickness "
	      );

MAKE_SCHEME_CALLBACK (Semi_tie, calc_control_points, 1)
SCM
Semi_tie::calc_control_points (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  (void) me->get_property ("direction");
  
  if (Semi_tie_column::has_interface (me->get_parent (Y_AXIS)))
    {
      me->get_parent (Y_AXIS)->get_property ("positioning-done");
    }
  else
    {
      programming_error ("lv tie without Semi_tie_column. Killing lv tie."); 
      me->suicide (); 
    }

  return me->get_property_data ("control-points");
}

int
Semi_tie::get_position (Grob *me)
{
  Grob *h = unsmob_grob (me->get_object ("note-head"));
  return (int) rint (Staff_symbol_referencer::get_position (h));
}

bool
Semi_tie::less (Grob *const &s1,
		Grob *const &s2)
{
  return get_position (s1) < get_position (s2);
}
				 
