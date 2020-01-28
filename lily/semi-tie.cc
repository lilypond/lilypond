/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>


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

#include "semi-tie.hh"
#include "directional-element-interface.hh"
#include "grob.hh"
#include "paper-column.hh"
#include "semi-tie-column.hh"
#include "staff-symbol-referencer.hh"
#include "tie.hh"
#include "warn.hh"

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
               "line-thickness ");

MAKE_SCHEME_CALLBACK (Semi_tie, calc_control_points, 1)
SCM
Semi_tie::calc_control_points (SCM smob)
{
  Item *me = LY_ASSERT_SMOB (Item, smob, 1);

  (void)me->get_property ("direction");

  Grob *yparent = me->get_parent (Y_AXIS);
  if (has_interface<Semi_tie_column> (yparent))
    {
      /* trigger positioning. */
      yparent->get_property ("positioning-done");

      return me->get_property_data ("control-points");
    }

  programming_error ("lv tie without Semi_tie_column.  Killing lv tie.");
  me->suicide ();
  return SCM_EOL;
}

int
Semi_tie::get_column_rank (Item *me)
{
  return me->get_column ()->get_rank ();
}

int
Semi_tie::get_position (Item *me)
{
  return (int)rint (Staff_symbol_referencer::get_position (head (me)));
}

bool
Semi_tie::less (Grob *g1, Grob *g2)
{
  Item *i1 = dynamic_cast<Item *> (g1);
  if (!i1)
    {
      g1->programming_error ("grob is not a semi-tie");
      return false;
    }

  Item *i2 = dynamic_cast<Item *> (g2);
  if (!i2)
    {
      g2->programming_error ("grob is not a semi-tie");
      return true;
    }

  return get_position (i1) < get_position (i2);
}

Item *
Semi_tie::head (Item *me)
{
  return unsmob<Item> (me->get_object ("note-head"));
}
