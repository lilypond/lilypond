/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2011 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "international.hh"

class Balloon_interface
{
public:
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (print_spanner, (SCM));
  DECLARE_GROB_INTERFACE ();
};

MAKE_SCHEME_CALLBACK (Balloon_interface, print, 1);
SCM
Balloon_interface::print (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  Grob *p = me->get_parent (X_AXIS);
  
  Offset off (me->relative_coordinate (p, X_AXIS),
	      me->relative_coordinate (p, Y_AXIS));

  Box b (p->extent (p, X_AXIS),
	 p->extent (p, Y_AXIS));

  Real padding = robust_scm2double (me->get_property ("padding"), .1);
  b.widen (padding, padding);

  // FIXME
  Stencil fr;
  if (to_boolean (me->get_property ("annotation-balloon")))
    fr = Lookup::frame (b, 0.1, 0.05);

  SCM bt = me->get_property ("text");
  SCM chain = Font_interface::text_font_alist_chain (me);

  SCM stencil = Text_interface::interpret_markup (me->layout ()->self_scm (),
						  chain, bt);

  Stencil *text_stil = unsmob_stencil (stencil);

  Offset z1;
  for (int i = X_AXIS; i < NO_AXES; i++)
    {
      Axis a ((Axis)i);
      z1[a] = b[a].linear_combination (sign (off[a]));
      text_stil->align_to (a, -sign (off[a]));
    }

  Offset z2 = z1 + off;

  if (to_boolean (me->get_property ("annotation-line")))
    fr.add_stencil (Line_interface::line (me, z1, z2));

  text_stil->translate (z2);
  fr.add_stencil (*text_stil);

  fr.translate (-off);
  return fr.smobbed_copy ();
}

// ugh...code dup...hopefully can be consolidated w/ above one day
MAKE_SCHEME_CALLBACK (Balloon_interface, print_spanner, 1);
SCM
Balloon_interface::print_spanner (SCM smob)
{
  Spanner *me = unsmob_spanner (smob);
  Spanner *parent = unsmob_spanner (me->get_property ("parent-spanner"));
  Spanner *p;
  message (_f ("foo %d", robust_scm2int (me->get_property ("spanner-to-annotate"), 0)));
  message (_f ("bar %d", robust_scm2int (me->broken_intos_[0]->get_property ("spanner-to-annotate"), 0)));
  if (parent->broken_intos_.size () == 0)
    p = parent;
  else
    p = parent->broken_intos_[robust_scm2int (me->get_property ("spanner-to-annotate"), 0) % parent->broken_intos_.size ()];

  
  Drul_array<Item *> bounds;
  Direction d = LEFT;
  
  do
    {
      bounds[d] = me->get_bound (d);
    }
  while (flip (&d) != LEFT);
  
  Grob *commonx = bounds[LEFT]->common_refpoint (bounds[RIGHT], X_AXIS);

  Offset off (me->relative_coordinate (commonx, X_AXIS),
	      me->relative_coordinate (p, Y_AXIS));
  
  Box b (p->extent (p, X_AXIS),
	 p->extent (p, Y_AXIS));

  Real padding = robust_scm2double (me->get_property ("padding"), .1);
  b.widen (padding, padding);

  // FIXME
  Stencil fr;
  if (to_boolean (me->get_property ("annotation-balloon")))
    fr = Lookup::frame (b, 0.1, 0.05);

  SCM bt = me->get_property ("text");
  SCM chain = Font_interface::text_font_alist_chain (me);

  SCM stencil = Text_interface::interpret_markup (me->layout ()->self_scm (),
						  chain, bt);

  Stencil *text_stil = unsmob_stencil (stencil);

  Offset z1;
  for (int i = X_AXIS; i < NO_AXES; i++)
    {
      Axis a ((Axis)i);
      z1[a] = b[a].linear_combination (sign (off[a]));
      text_stil->align_to (a, -sign (off[a]));
    }

  Offset z2 = z1 + off;

  if (to_boolean (me->get_property ("annotation-line")))
    fr.add_stencil (Line_interface::line (me, z1, z2));

  text_stil->translate (z2);
  fr.add_stencil (*text_stil);

  fr.translate (-off);
  return fr.smobbed_copy ();
}

ADD_INTERFACE (Balloon_interface,
	       "A collection of routines to put text balloons around an"
	       " object.",

	       /* properties */
	       "annotation-balloon "
	       "annotation-line "
	       "padding "
	       "spanner-to-annotate "
	       "text "
	       );

