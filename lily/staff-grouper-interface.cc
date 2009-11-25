/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2009 Joe Neeman <joeneeman@gmail.com>

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

#include "staff-grouper-interface.hh"

#include "pointer-group-interface.hh"

Grob*
Staff_grouper_interface::get_last_grob (Grob *me)
{
  extract_grob_set (me, "elements", elts);
  for (vsize i = elts.size (); i--;)
    if (elts[i]->is_live ())
      return elts[i];

  return 0;
}

ADD_INTERFACE (Staff_grouper_interface,
	       "A grob that collects staves together.",

	       /* properties */
	       "between-staff-spacing "
	       "after-last-staff-spacing "
	       );

