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

#include "self-alignment-interface.hh"

#include "grob.hh"
#include "note-column.hh"
#include "paper-column.hh"
#include "pointer-group-interface.hh"
#include "stencil.hh"
#include "warn.hh"

MAKE_SCHEME_CALLBACK (Self_alignment_interface, y_aligned_on_self, 1);
SCM
Self_alignment_interface::y_aligned_on_self (SCM element)
{
  return aligned_on_self (unsmob<Grob> (element), Y_AXIS, false, 0, 0);
}

MAKE_SCHEME_CALLBACK (Self_alignment_interface, x_aligned_on_self, 1);
SCM
Self_alignment_interface::x_aligned_on_self (SCM element)
{
  return aligned_on_self (unsmob<Grob> (element), X_AXIS, false, 0, 0);
}

MAKE_SCHEME_CALLBACK (Self_alignment_interface, pure_y_aligned_on_self, 3);
SCM
Self_alignment_interface::pure_y_aligned_on_self (SCM smob, SCM start, SCM end)
{
  return aligned_on_self (unsmob<Grob> (smob), Y_AXIS, true, from_scm (start, 0), from_scm (end, INT_MAX));
}

SCM
Self_alignment_interface::aligned_on_self (Grob *me, Axis a, bool pure, int start, int end)
{
  SCM align = (a == X_AXIS)
              ? get_property (me, "self-alignment-X")
              : get_property (me, "self-alignment-Y");
  if (scm_is_number (align))
    {
      Interval ext (me->maybe_pure_extent (me, a, pure, start, end));
      // Empty extent doesn't mean an error - we simply don't align such grobs.
      // However, empty extent and non-empty stencil would be suspicious.
      if (!ext.is_empty ())
        return to_scm (- ext.linear_combination (scm_to_double (align)));
      else
        {
          Stencil *st = me->get_stencil ();
          if (st && !st->is_empty ())
            warning (me->name () + " has empty extent and non-empty stencil.");
        }
    }
  return to_scm (0.0);
}

SCM
Self_alignment_interface::centered_on_object (Grob *him, Axis a)
{
  return to_scm (robust_relative_extent (him, him, a).center ());
}

MAKE_SCHEME_CALLBACK (Self_alignment_interface, centered_on_x_parent, 1);
SCM
Self_alignment_interface::centered_on_x_parent (SCM smob)
{
  return centered_on_object (unsmob<Grob> (smob)->get_parent (X_AXIS), X_AXIS);
}

MAKE_SCHEME_CALLBACK (Self_alignment_interface, centered_on_y_parent, 1);
SCM
Self_alignment_interface::centered_on_y_parent (SCM smob)
{
  return centered_on_object (unsmob<Grob> (smob)->get_parent (Y_AXIS), Y_AXIS);
}

MAKE_SCHEME_CALLBACK (Self_alignment_interface, aligned_on_x_parent, 1);
SCM
Self_alignment_interface::aligned_on_x_parent (SCM smob)
{
  return aligned_on_parent (unsmob<Grob> (smob), X_AXIS);
}

MAKE_SCHEME_CALLBACK (Self_alignment_interface, aligned_on_y_parent, 1);
SCM
Self_alignment_interface::aligned_on_y_parent (SCM smob)
{
  return aligned_on_parent (unsmob<Grob> (smob), Y_AXIS);
}

SCM
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
    he = Paper_column::get_interface_extent
         (him, ly_symbol2scm ("note-column-interface"), a);
  else
    {
      if (from_scm<bool> (get_property (me, "X-align-on-main-noteheads"))
          && has_interface<Note_column> (him))
        he = Note_column::calc_main_extent (him);
      else
        he = him->extent (him, a);
    }

  SCM self_align = (a == X_AXIS)
                   ? get_property (me, "self-alignment-X")
                   : get_property (me, "self-alignment-Y");

  SCM par_align = (a == X_AXIS)
                  ? get_property (me, "parent-alignment-X")
                  : get_property (me, "parent-alignment-Y");

  if (scm_is_null (par_align))
    par_align = self_align;

  Real x = 0.0;
  Interval ext (me->extent (me, a));

  if (scm_is_number (self_align))
    {
      // Empty extent doesn't mean an error - we simply don't align such grobs.
      // However, empty extent and non-empty stencil would be suspicious.
      if (!ext.is_empty ())
        x -= ext.linear_combination (scm_to_double (self_align));
      else
        {
          Stencil *st = me->get_stencil ();
          if (st && !st->is_empty ())
            warning (me->name () + " has empty extent and non-empty stencil.");
        }
    }

  if (scm_is_number (par_align))
    {
      // See comment above.
      if (!he.is_empty ())
        x += he.linear_combination (scm_to_double (par_align));
      else
        {
          Stencil *st = him->get_stencil ();
          if (st && !st->is_empty ())
            warning (him->name () + " has empty extent and non-empty stencil.");
        }
    }

  return to_scm (x);
}

void
Self_alignment_interface::set_aligned_on_parent (Grob *me, Axis a)
{
  add_offset_callback (me,
                       (a == X_AXIS) ? aligned_on_x_parent_proc : aligned_on_y_parent_proc,
                       a);
}

ADD_INTERFACE (Self_alignment_interface,
               "Position this object on itself and/or on its parent.  To this"
               " end, the following functions are provided:\n"
               "\n"
               "@table @code\n"
               "@item Self_alignment_interface::[xy]_aligned_on_self\n"
               "Align self on reference point, using"
               " @code{self-alignment-X} and @code{self-alignment-Y}."
               "@item Self_alignment_interface::aligned_on_[xy]_parent\n"
               "@item Self_alignment_interface::centered_on_[xy]_parent\n"
               "Shift the object so its own reference point is centered on"
               " the extent of the parent\n"
               "@end table\n",

               /* properties */
               "parent-alignment-X "
               "parent-alignment-Y "
               "self-alignment-X "
               "self-alignment-Y "
               "X-align-on-main-noteheads "
              );
