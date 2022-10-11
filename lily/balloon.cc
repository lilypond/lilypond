/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "staff-symbol-referencer.hh"

class Balloon_interface
{
public:
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (width, (SCM));
  DECLARE_SCHEME_CALLBACK (pure_height, (SCM, SCM, SCM));

  static Stencil internal_balloon_print (Grob *me, Box b, Offset off);
};

MAKE_SCHEME_CALLBACK (Balloon_interface, print, "ly:balloon-interface::print",
                      1);
SCM
Balloon_interface::print (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);

  Grob *annotated = unsmob<Grob> (get_object (me, "sticky-host"));
  if (!annotated)
    {
      me->programming_error ("sticky grob without host");
      return Stencil ().smobbed_copy ();
    }
  Stencil result;
  Offset off (me->relative_coordinate (annotated, X_AXIS),
              me->relative_coordinate (annotated, Y_AXIS));
  Box b (robust_relative_extent (annotated, annotated, X_AXIS),
         robust_relative_extent (annotated, annotated, Y_AXIS));

  return internal_balloon_print (me, b, off).smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Balloon_interface, width, "ly:balloon-interface::width",
                      1);
SCM
Balloon_interface::width (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  Grob *annotated = unsmob<Grob> (get_object (me, "sticky-host"));
  if (!annotated)
    {
      me->programming_error ("sticky grob without host");
      return to_scm (Interval ());
    }
  Box b (robust_relative_extent (annotated, annotated, X_AXIS),
         Interval (0, 0));
  Real off = me->relative_coordinate (annotated, X_AXIS);
  return to_scm (
    internal_balloon_print (me, b, Offset (off, 0)).extent (X_AXIS));
}

MAKE_SCHEME_CALLBACK (Balloon_interface, pure_height,
                      "ly:balloon-interface::pure-height", 3);
SCM
Balloon_interface::pure_height (SCM smob, SCM start_scm, SCM end_scm)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  Grob *annotated = unsmob<Grob> (get_object (me, "sticky-host"));
  if (!annotated)
    {
      me->programming_error ("sticky grob without host");
      return to_scm (Interval ());
    }

  vsize start = from_scm<vsize> (start_scm);
  vsize end = from_scm<vsize> (end_scm);

  Interval y = robust_relative_pure_y_extent (annotated, annotated, start, end);

  Real off = me->relative_coordinate (annotated, Y_AXIS);

  return to_scm (
    internal_balloon_print (me, Box (Interval (0, 0), y), Offset (0, off))
      .extent (Y_AXIS));
}

Stencil
Balloon_interface::internal_balloon_print (Grob *me, Box b, Offset off)
{
  Real padding = from_scm<double> (get_property (me, "padding"), .1);
  b.widen (padding, padding);

  Stencil result;
  if (from_scm<bool> (get_property (me, "annotation-balloon")))
    {
      Real thickness = from_scm (get_property (me, "thickness"), 1.0);
      thickness *= Staff_symbol_referencer::line_thickness (me);
      Real blot_diameter = 0.05; // FIXME: hardcoded
      result = Lookup::frame (b, thickness, blot_diameter);
    }

  SCM bt = get_property (me, "text");
  SCM chain = Font_interface::text_font_alist_chain (me);
  // TODO: cache somehow?
  auto text_stil = Text_interface::interpret_markup (me->layout (), chain, bt);

  Offset z1;

  for (const auto a : {X_AXIS, Y_AXIS})
    {
      /* By default, we use these alignments:

          Balloon text
                      \
                       \
                        grob

                     Balloon text
                          |
                          |
                        grob

                              Balloon text
                             /
                            /
                        grob

      */
      Real off_sign = static_cast<Real> (sign (off[a]));
      SCM text_align_prop
        = ((a == X_AXIS) ? ly_symbol2scm ("text-alignment-X")
                         : ly_symbol2scm ("text-alignment-Y"));
      Real text_align
        = from_scm (get_property (me, text_align_prop), -off_sign);
      SCM attach_align_prop = ((a == X_AXIS) ? ly_symbol2scm ("X-attachment")
                                             : ly_symbol2scm ("Y-attachment"));
      Real attach_align
        = from_scm (get_property (me, attach_align_prop), off_sign);
      z1[a] = b[a].linear_combination (attach_align);
      text_stil.align_to (a, text_align);
    }

  Offset z2 = z1 + off;

  if (from_scm<bool> (get_property (me, "annotation-line")))
    result.add_stencil (Line_interface::line (me, z1, z2));

  text_stil.translate (z2);
  result.add_stencil (text_stil);

  result.translate (-off);
  return result;
}

ADD_INTERFACE (Balloon_interface,
               R"(
A collection of routines to put text balloons around an object.
               )",

               /* properties */
               R"(
annotation-balloon
annotation-line
padding
spanner-placement
text
text-alignment-X
text-alignment-Y
thickness
X-attachment
Y-attachment
               )");
