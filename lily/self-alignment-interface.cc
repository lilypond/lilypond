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

#include "self-alignment-interface.hh"

#include "grob.hh"
#include "note-column.hh"
#include "paper-column.hh"
#include "pointer-group-interface.hh"
#include "stencil.hh"
#include "warn.hh"

MAKE_SCHEME_CALLBACK (Self_alignment_interface, y_aligned_on_self,
                      "ly:self-alignment-interface::y-aligned-on-self", 1);
SCM
Self_alignment_interface::y_aligned_on_self (SCM element)
{
  auto *const me = LY_ASSERT_SMOB (Grob, element, 1);
  return to_scm (aligned_on_self (me, Y_AXIS, false, 0, 0));
}

MAKE_SCHEME_CALLBACK (Self_alignment_interface, x_aligned_on_self,
                      "ly:self-alignment-interface::x-aligned-on-self", 1);
SCM
Self_alignment_interface::x_aligned_on_self (SCM element)
{
  auto *const me = LY_ASSERT_SMOB (Grob, element, 1);
  return to_scm (x_aligned_on_self (me));
}

MAKE_SCHEME_CALLBACK (Self_alignment_interface, pure_y_aligned_on_self,
                      "ly:self-alignment-interface::pure-y-aligned-on-self", 3);
SCM
Self_alignment_interface::pure_y_aligned_on_self (SCM smob, SCM start, SCM end)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  return to_scm (
    pure_y_aligned_on_self (me, from_scm (start, 0), from_scm (end, INT_MAX)));
}

Real
Self_alignment_interface::aligned_on_self (Grob *me, Axis a, bool pure,
                                           int start, int end)
{
  SCM align = (a == X_AXIS) ? get_property (me, "self-alignment-X")
                            : get_property (me, "self-alignment-Y");
  if (scm_is_number (align))
    {
      Interval ext (me->maybe_pure_extent (me, a, pure, start, end));
      // Empty extent doesn't mean an error - we simply don't align such grobs.
      if (!ext.is_empty ())
        return -ext.linear_combination (from_scm<double> (align));
    }
  return 0.0;
}

Real
Self_alignment_interface::centered_on_self (Grob *me, Axis a)
{
  return robust_relative_extent (me, me, a).center ();
}

MAKE_SCHEME_CALLBACK (Self_alignment_interface, centered_on_x_parent,
                      "ly:self-alignment-interface::centered-on-x-parent", 1);
SCM
Self_alignment_interface::centered_on_x_parent (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  return to_scm (centered_on_self (me->get_x_parent (), X_AXIS));
}

MAKE_SCHEME_CALLBACK (Self_alignment_interface, centered_on_y_parent,
                      "ly:self-alignment-interface::centered-on-y-parent", 1);
SCM
Self_alignment_interface::centered_on_y_parent (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  return to_scm (centered_on_self (me->get_y_parent (), Y_AXIS));
}

MAKE_SCHEME_CALLBACK (Self_alignment_interface, aligned_on_x_parent,
                      "ly:self-alignment-interface::aligned-on-x-parent", 1);
SCM
Self_alignment_interface::aligned_on_x_parent (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  return to_scm (aligned_on_parent (me, X_AXIS));
}

MAKE_SCHEME_CALLBACK (Self_alignment_interface, aligned_on_y_parent,
                      "ly:self-alignment-interface::aligned-on-y-parent", 1);
SCM
Self_alignment_interface::aligned_on_y_parent (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  return to_scm (aligned_on_parent (me, Y_AXIS));
}

Real
Self_alignment_interface::aligned_on_parent (Grob *me, Axis a)
{
  Grob *him = me->get_parent (a);
  Interval he;
  if (has_interface<Paper_column> (him))
    /*
      PaperColumn extents aren't reliable (they depend on size and alignment
      of PaperColumn's children), so we align on NoteColumn instead.
      This happens e.g. for lyrics without associatedVoice.
    */
    he = Paper_column::get_interface_extent (
      him, ly_symbol2scm ("note-column-interface"), a);
  else
    {
      if (from_scm<bool> (get_property (me, "X-align-on-main-noteheads"))
          && has_interface<Note_column> (him))
        he = from_scm<Interval> (get_property (him, "main-extent"));
      else
        he = him->extent (him, a);
    }

  SCM self_align = (a == X_AXIS) ? get_property (me, "self-alignment-X")
                                 : get_property (me, "self-alignment-Y");

  SCM par_align = (a == X_AXIS) ? get_property (me, "parent-alignment-X")
                                : get_property (me, "parent-alignment-Y");

  if (scm_is_null (par_align))
    par_align = self_align;

  Real x = 0.0;
  Interval ext (me->extent (me, a));

  if (scm_is_number (self_align))
    {
      // Empty extent doesn't mean an error - we simply don't align such grobs.
      if (!ext.is_empty ())
        x -= ext.linear_combination (from_scm<double> (self_align));
    }

  if (scm_is_number (par_align))
    {
      if (!he.is_empty ())
        x += he.linear_combination (from_scm<double> (par_align));
    }

  return x;
}

void
Self_alignment_interface::set_aligned_on_parent (Grob *me, Axis a)
{
  add_offset_callback (
    me, (a == X_AXIS) ? aligned_on_x_parent_proc : aligned_on_y_parent_proc, a);
}

ADD_INTERFACE (Self_alignment_interface,
               R"(
Position this object on itself and/or on its parent.  To this end, the
following functions are provided:

@table @code
@item Self_alignment_interface::[xy]_aligned_on_self
Align self on reference point, using @code{self-alignment-X} and
@code{self-alignment-Y}.@item Self_alignment_interface::aligned_on_[xy]_parent
@item Self_alignment_interface::centered_on_[xy]_parent
Shift the object so its own reference point is centered on the extent of the
parent
@end table
               )",

               /* properties */
               R"(
parent-alignment-X
parent-alignment-Y
self-alignment-X
self-alignment-Y
X-align-on-main-noteheads
               )");
