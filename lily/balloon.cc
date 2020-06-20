/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "text-interface.hh"
#include "grob.hh"
#include "item.hh"
#include "line-interface.hh"
#include "lookup.hh"
#include "font-interface.hh"
#include "lily-guile.hh"
#include "output-def.hh"
#include "misc.hh"
#include "spanner.hh"

class Balloon_interface
{
public:
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (print_spanner, (SCM));
  DECLARE_SCHEME_CALLBACK (pure_height, (SCM, SCM, SCM));

  static Stencil internal_balloon_print (Grob *me, Box b, Offset off);
};

MAKE_SCHEME_CALLBACK (Balloon_interface, print, 1);
SCM
Balloon_interface::print (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);

  if (Item *item = dynamic_cast<Item *> (me))
    if (!Item::break_visible (item))
      return SCM_EOL;

  Grob *p = me->get_x_parent ();

  Offset off (me->relative_coordinate (p, X_AXIS),
              me->relative_coordinate (p, Y_AXIS));
  Box b (robust_relative_extent (p, p, X_AXIS),
         robust_relative_extent (p, p, Y_AXIS));

  return internal_balloon_print (me, b, off).smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Balloon_interface, print_spanner, 1);
SCM
Balloon_interface::print_spanner (SCM smob)
{
  Spanner *me = unsmob<Spanner> (smob);

  if (Spanner *orig = me->original ())
    {
      Direction spanner_placement = from_scm (get_property (me, "spanner-placement"), LEFT);

      Spanner *wanted = (spanner_placement != RIGHT)
                        ? orig->broken_intos_[0]
                        : orig->broken_intos_.back ();

      if (me != wanted)
        return SCM_EOL;
    }

  Spanner *p = dynamic_cast<Spanner *> (me->get_y_parent ());

  if (!p)
    return SCM_EOL;

  Offset off (me->relative_coordinate (me->get_bound (LEFT), X_AXIS),
              me->relative_coordinate (p, Y_AXIS));
  Box b (robust_relative_extent (p, p, X_AXIS),
         robust_relative_extent (p, p, Y_AXIS));
  return internal_balloon_print (me, b, off).smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Balloon_interface, pure_height, 3);
SCM
Balloon_interface::pure_height (SCM smob, SCM start_scm, SCM end_scm)
{
  Grob *me = unsmob<Grob> (smob);
  Spanner *p = dynamic_cast<Spanner *> (me->get_y_parent ());

  if (!p || !p->is_live ())
    return SCM_EOL;

  vsize start = from_scm<vsize> (start_scm);
  int end = scm_to_int (end_scm);

  Interval y = p->pure_y_extent (p, start, end);

  Real off = me->relative_coordinate (p, Y_AXIS);

  return to_scm (
    internal_balloon_print (me, Box (Interval (0, 0), y), Offset (0, off))
      .extent (Y_AXIS));
}

Stencil
Balloon_interface::internal_balloon_print (Grob *me, Box b, Offset off)
{
  Real padding = from_scm<double> (get_property (me, "padding"), .1);
  b.widen (padding, padding);

  // FIXME
  Stencil fr;
  if (from_scm<bool> (get_property (me, "annotation-balloon")))
    fr = Lookup::frame (b, 0.1, 0.05);

  SCM bt = get_property (me, "text");
  SCM chain = Font_interface::text_font_alist_chain (me);
  SCM stencil = Text_interface::interpret_markup (me->layout ()->self_scm (),
                                                  chain, bt);
  Stencil *text_stil = unsmob<Stencil> (stencil);

  Offset z1;

  for (int i = X_AXIS; i < NO_AXES; i++)
    {
      Axis a ((Axis)i);
      z1[a] = b[a].linear_combination (sign (off[a]));
      text_stil->align_to (a, -sign (off[a]));
    }

  Offset z2 = z1 + off;

  if (from_scm<bool> (get_property (me, "annotation-line")))
    fr.add_stencil (Line_interface::line (me, z1, z2));

  text_stil->translate (z2);
  fr.add_stencil (*text_stil);

  fr.translate (-off);
  return fr;
}

ADD_INTERFACE (Balloon_interface,
               "A collection of routines to put text balloons around an"
               " object.",

               /* properties */
               "annotation-balloon "
               "annotation-line "
               "padding "
               "spanner-placement "
               "text "
              );
