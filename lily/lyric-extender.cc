/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2020 Jan Nieuwenhuizen <janneke@gnu.org>
  Han-Wen Nienhuys

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

#include "lyric-extender.hh"

#include "item.hh"
#include "lookup.hh"
#include "note-head.hh"
#include "output-def.hh"
#include "pointer-group-interface.hh"
#include "system.hh"
#include "warn.hh"

MAKE_SCHEME_CALLBACK (Lyric_extender, print, 1);
SCM
Lyric_extender::print (SCM smob)
{
  Spanner *me = unsmob<Spanner> (smob);
  Item *left_edge = me->get_bound (LEFT);
  Item *right_text = unsmob<Item> (me->get_object ("next"));

  Grob *common = left_edge;

  if (right_text)
    common = common->common_refpoint (right_text, X_AXIS);

  common = common->common_refpoint (me->get_bound (RIGHT), X_AXIS);
  common = common->common_refpoint (me->get_system (), X_AXIS);

  Real sl = me->layout ()->get_dimension (ly_symbol2scm ("line-thickness"));

  extract_grob_set (me, "heads", heads);

  if (!heads.size ())
    return SCM_EOL;

  common = common_refpoint_of_array (heads, common, X_AXIS);

  Real left_point = 0.0;
  if (left_edge->internal_has_interface (
          ly_symbol2scm ("lyric-syllable-interface")))
    left_point = left_edge->extent (common, X_AXIS)[RIGHT];
  else if (heads.size ())
    left_point = heads[0]->extent (common, X_AXIS)[LEFT];
  else
    left_point = left_edge->extent (common, X_AXIS)[RIGHT];

  if (std::isinf (left_point))
    return SCM_EOL;

  /* It seems that short extenders are even lengthened to go past the
     note head, but haven't found a pattern in it yet. --hwn 1/1/04  */
  SCM minlen = me->get_property ("minimum-length");
  Real right_point = left_point + (robust_scm2double (minlen, 0));

  right_point = std::min (
      right_point, me->get_system ()->get_bound (RIGHT)->relative_coordinate (
                       common, X_AXIS));

  if (heads.size ())
    right_point
        = std::max (right_point, heads.back ()->extent (common, X_AXIS)[RIGHT]);

  Real h = sl * robust_scm2double (me->get_property ("thickness"), 0);
  Drul_array<Real> paddings (
      robust_scm2double (me->get_property ("left-padding"), h),
      robust_scm2double (me->get_property ("right-padding"), h));

  if (right_text)
    right_point = std::min (
        right_point, (robust_relative_extent (right_text, common, X_AXIS)[LEFT]
                      - paddings[RIGHT]));

  /* run to end of line. */
  if (me->get_bound (RIGHT)->break_status_dir ())
    right_point = std::max (
        right_point,
        (robust_relative_extent (me->get_bound (RIGHT), common, X_AXIS)[LEFT]
         - paddings[RIGHT]));

  left_point += paddings[LEFT];
  Real w = right_point - left_point;

  if (w < 1.5 * h)
    return SCM_EOL;

  Stencil mol (Lookup::round_filled_box (Box (Interval (0, w), Interval (0, h)),
                                         0.8 * h));
  mol.translate_axis (left_point - me->relative_coordinate (common, X_AXIS),
                      X_AXIS);
  return mol.smobbed_copy ();
}

ADD_INTERFACE (Lyric_extender,
               "The extender is a simple line at the baseline of the lyric"
               " that helps show the length of a melisma (a tied or slurred"
               " note).",

               /* properties */
               "heads "
               "left-padding "
               "next "
               "right-padding "
               "thickness ");
