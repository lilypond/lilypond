/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2022 Jan Nieuwenhuizen <janneke@gnu.org>
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

#include "system.hh"
#include "item.hh"
#include "warn.hh"
#include "lookup.hh"
#include "output-def.hh"
#include "note-head.hh"
#include "pointer-group-interface.hh"

MAKE_SCHEME_CALLBACK (Lyric_extender, print, "ly:lyric-extender::print", 1);
SCM
Lyric_extender::print (SCM smob)
{
  Spanner *me = unsmob<Spanner> (smob);
  Item *left_edge = me->get_bound (LEFT);
  Item *right_text = unsmob<Item> (get_object (me, "next"));

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
  SCM minlen = get_property (me, "minimum-length");
  Real right_point = left_point + (from_scm<double> (minlen, 0));

  {
    auto *const rb = me->get_system ()->get_bound (RIGHT);
    auto limit = rb->relative_coordinate (common, X_AXIS);
    right_point = std::min (right_point, limit);
  }

  if (!heads.empty ())
    {
      auto limit = heads.back ()->extent (common, X_AXIS)[RIGHT];
      right_point = std::max (right_point, limit);
    }

  Real h = sl * from_scm<double> (get_property (me, "thickness"), 0);
  Drul_array<Real> paddings {
    from_scm<double> (get_property (me, "left-padding"), h),
    from_scm<double> (get_property (me, "right-padding"), h)};

  if (right_text)
    {
      auto limit = robust_relative_extent (right_text, common, X_AXIS)[LEFT]
                   - paddings[RIGHT];
      right_point = std::min (right_point, limit);
    }

  /* run to end of line. */
  {
    auto *const rb = me->get_bound (RIGHT);
    if (rb->break_status_dir ())
      {
        auto limit
          = robust_relative_extent (rb, common, X_AXIS)[LEFT] - paddings[RIGHT];
        right_point = std::max (right_point, limit);
      }
  }

  left_point += paddings[LEFT];
  Real w = right_point - left_point;

  if (w < 1.5 * h)
    return SCM_EOL;

  Stencil mol (
    Lookup::round_filled_box (Box (Interval (0, w), Interval (0, h)), 0.8 * h));
  mol.translate_axis (left_point - me->relative_coordinate (common, X_AXIS),
                      X_AXIS);
  return mol.smobbed_copy ();
}

ADD_INTERFACE (Lyric_extender,
               R"(
The extender is a simple line at the baseline of the lyric that helps show the
length of a melisma (a tied or slurred note).
               )",

               /* properties */
               R"(
heads
left-padding
next
right-padding
thickness
               )");
