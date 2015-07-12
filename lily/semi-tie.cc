/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2015 Han-Wen Nienhuys <hanwen@xs4all.nl>


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
#include "paper-column.hh"
#include "tie.hh"
#include "warn.hh"
#include "staff-symbol-referencer.hh"

ADD_INTERFACE (Semi_tie,
               "A tie which is only connected to a note head on one side."
               "\n"
               "The following properties may be set in the @code{details}"
               " list:\n"
               "\n"
               "@table @code\n"
               "@item height-limit\n"
               "Maximum tie height: The longer the tie, the closer it is"
               " to this height.\n"
               "@item ratio\n"
               "Parameter for tie shape.  The higher this number, the"
               " quicker the tie attains its @code{height-limit}.\n"
               "@end table\n",

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
  Grob *me = unsmob<Grob> (smob);
  (void) me->get_property ("direction");

  if (Semi_tie_column::has_interface (me->get_parent (Y_AXIS)))
    {
      me->get_parent (Y_AXIS)->get_property ("positioning-done");
    }
  else
    {
      programming_error ("lv tie without Semi_tie_column.  Killing lv tie.");
      me->suicide ();
    }

  // TODO: Even if me->suicide() was called?
  return me->get_property_data ("control-points");
}

int
Semi_tie::get_column_rank (Item *me)
{
  return Paper_column::get_rank (me->get_column ());
}

int
Semi_tie::get_position (Item *me)
{
  return (int) rint (Staff_symbol_referencer::get_position (head (me)));
}

bool
Semi_tie::less (Grob *g1, Grob *g2)
{
  Item *i1 = dynamic_cast<Item *> (g1);
  Item *i2 = dynamic_cast<Item *> (g2);
  if (i1 && i2) {
    return get_position (i1) < get_position (i2);
  }

  programming_error ("grob is not a semi-tie");
  return false;
}

Item *
Semi_tie::head (Item *me)
{
  return unsmob<Item> (me->get_object ("note-head"));
}

Item *
Semi_tie::head (Item *me, Direction d)
{
  SCM head_dir = me->get_property ("head-direction");
  return (is_direction (head_dir) && (to_dir (head_dir) == d)) ? head (me) : 0;
}
