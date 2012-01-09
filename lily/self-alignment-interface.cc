/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2012 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "directional-element-interface.hh"
#include "grob.hh"
#include "grob-array.hh"
#include "interval-minefield.hh"
#include "note-column.hh"
#include "paper-column.hh"
#include "pointer-group-interface.hh"
#include "warn.hh"

MAKE_SCHEME_CALLBACK (Self_alignment_interface, y_aligned_on_self, 1);
SCM
Self_alignment_interface::y_aligned_on_self (SCM element)
{
  return aligned_on_self (unsmob_grob (element), Y_AXIS, false, 0, 0);
}

MAKE_SCHEME_CALLBACK (Self_alignment_interface, x_aligned_on_self, 1);
SCM
Self_alignment_interface::x_aligned_on_self (SCM element)
{
  return aligned_on_self (unsmob_grob (element), X_AXIS, false, 0, 0);
}

MAKE_SCHEME_CALLBACK (Self_alignment_interface, pure_y_aligned_on_self, 3);
SCM
Self_alignment_interface::pure_y_aligned_on_self (SCM smob, SCM start, SCM end)
{
  return aligned_on_self (unsmob_grob (smob), Y_AXIS, true, robust_scm2int (start, 0), robust_scm2int (end, INT_MAX));
}

SCM
Self_alignment_interface::aligned_on_self (Grob *me, Axis a, bool pure, int start, int end)
{
  SCM sym = (a == X_AXIS) ? ly_symbol2scm ("self-alignment-X")
            : ly_symbol2scm ("self-alignment-Y");

  SCM align (me->internal_get_property (sym));
  if (scm_is_number (align))
    {
      Interval ext (me->maybe_pure_extent (me, a, pure, start, end));
      if (ext.is_empty ())
        programming_error ("cannot align on self: empty element");
      else
        return scm_from_double (- ext.linear_combination (scm_to_double (align)));
    }
  return scm_from_double (0.0);
}

SCM
Self_alignment_interface::centered_on_object (Grob *him, Axis a)
{
  return scm_from_double (robust_relative_extent (him, him, a).center ());
}

MAKE_SCHEME_CALLBACK (Self_alignment_interface, centered_on_x_parent, 1);
SCM
Self_alignment_interface::centered_on_x_parent (SCM smob)
{
  return centered_on_object (unsmob_grob (smob)->get_parent (X_AXIS), X_AXIS);
}

MAKE_SCHEME_CALLBACK (Self_alignment_interface, centered_on_note_columns, 1);
SCM
Self_alignment_interface::centered_on_note_columns (SCM smob)
{
  Item *it = unsmob_item (smob)->get_column ();
  if (!it)
    return scm_from_double (0.0);

  extract_grob_set (it, "elements", elts);
  vector<Grob *> ncs;
  Interval centers;
  for (vsize i = 0; i < elts.size (); i++)
    if (Note_column::has_interface (elts[i]))
      centers.add_point (scm_to_double (centered_on_object (elts[i], X_AXIS)));

  if (centers.is_empty ())
    return scm_from_double (0.0);

  return scm_from_double (centers.center ());
}

MAKE_SCHEME_CALLBACK (Self_alignment_interface, centered_on_y_parent, 1);
SCM
Self_alignment_interface::centered_on_y_parent (SCM smob)
{
  return centered_on_object (unsmob_grob (smob)->get_parent (Y_AXIS), Y_AXIS);
}

MAKE_SCHEME_CALLBACK (Self_alignment_interface, x_centered_on_y_parent, 1);
SCM
Self_alignment_interface::x_centered_on_y_parent (SCM smob)
{
  return centered_on_object (unsmob_grob (smob)->get_parent (Y_AXIS), X_AXIS);
}

MAKE_SCHEME_CALLBACK (Self_alignment_interface, aligned_on_x_parent, 1);
SCM
Self_alignment_interface::aligned_on_x_parent (SCM smob)
{
  return aligned_on_parent (unsmob_grob (smob), X_AXIS);
}

MAKE_SCHEME_CALLBACK (Self_alignment_interface, aligned_on_y_parent, 1);
SCM
Self_alignment_interface::aligned_on_y_parent (SCM smob)
{
  return aligned_on_parent (unsmob_grob (smob), Y_AXIS);
}

SCM
Self_alignment_interface::aligned_on_parent (Grob *me, Axis a)
{
  Grob *him = me->get_parent (a);
  if (Paper_column::has_interface (him))
    return scm_from_double (0.0);

  Interval he = him->extent (him, a);

  SCM sym = (a == X_AXIS) ? ly_symbol2scm ("self-alignment-X")
            : ly_symbol2scm ("self-alignment-Y");
  SCM align_prop (me->internal_get_property (sym));

  if (!scm_is_number (align_prop))
    return scm_from_int (0);

  Real x = 0.0;
  Real align = scm_to_double (align_prop);

  Interval ext (me->extent (me, a));
  if (ext.is_empty ())
    programming_error ("cannot align on self: empty element");
  else
    x -= ext.linear_combination (align);

  if (!he.is_empty ())
    x += he.linear_combination (align);

  return scm_from_double (x);
}

MAKE_SCHEME_CALLBACK (Self_alignment_interface, avoid_x_colliding_grobs, 2);
SCM
Self_alignment_interface::avoid_x_colliding_grobs (SCM smob, SCM o)
{
  SCM avoided = avoid_colliding_grobs (unsmob_grob (smob), X_AXIS, robust_scm2double (o, 0.0));
  return scm_is_null (avoided) ? o : avoided;
}

MAKE_SCHEME_CALLBACK (Self_alignment_interface, x_colliding_grobs, 1);
SCM
Self_alignment_interface::x_colliding_grobs (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  extract_grob_set (me, "potential-X-colliding-grobs", pot);
  vector<Grob *> act;
  Direction d = get_grob_direction (me->get_parent (Y_AXIS));
  for (vsize i = 0; i < pot.size (); i++)
    if (d == get_grob_direction (pot[i])
        && to_boolean (pot[i]->get_property ("cross-staff")))
      act.push_back (pot[i]);

  SCM grobs_scm = Grob_array::make_array ();
  unsmob_grob_array (grobs_scm)->set_array (act);

  return grobs_scm;
}

SCM
Self_alignment_interface::avoid_colliding_grobs (Grob *me, Axis a, Real offset)
{
  extract_grob_set (me, a == X_AXIS ? "X-colliding-grobs" : "Y-colliding-grobs", colls);
  if (!colls.size ())
    return SCM_EOL;
  vector<Interval> ivs;

  Item *refp = dynamic_cast<Item *> (common_refpoint_of_array (colls, me, a));
  if (!refp)
    return SCM_EOL;

  Interval iv = me->extent (me, a) + offset;
  for (vsize i = 0; i < colls.size (); i++)
    ivs.push_back (colls[i]->extent (refp, a));

  Interval_minefield minefield (Interval (iv.center (), iv.center ()), iv.length ());
  for (vsize i = 0; i < ivs.size (); i++)
    minefield.add_forbidden_interval (ivs[i]);
  minefield.solve ();
  Interval pos = minefield.feasible_placements ();

  if (pos[LEFT] == pos[RIGHT])
    return SCM_EOL;

  Direction col_dir = ((abs (pos[LEFT] - iv.center ())
                        + robust_scm2double (me->get_property ("collision-bias"), 0.0))
                       > abs (pos[RIGHT] - iv.center ()))
                      ? RIGHT
                      : LEFT;

  return scm_from_double ((pos[col_dir] - (iv.length () / 2)
                          + col_dir
                          * robust_scm2double (me->get_property ("collision-padding"), 0.0)));
}

void
Self_alignment_interface::set_center_parent (Grob *me, Axis a)
{
  add_offset_callback (me,
                       (a == X_AXIS) ? centered_on_x_parent_proc : centered_on_y_parent_proc,
                       a);
}

void
Self_alignment_interface::avoid_x_collisions (Grob *me)
{
  chain_offset_callback (me, avoid_x_colliding_grobs_proc, X_AXIS);
}

void
Self_alignment_interface::set_align_self (Grob *me, Axis a)
{
  add_offset_callback (me,
                       (a == X_AXIS) ? x_aligned_on_self_proc : y_aligned_on_self_proc,
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
               "collision-bias "
               "collision-padding "
               "potential-X-colliding-grobs "
               "self-alignment-X "
               "self-alignment-Y "
               "X-colliding-grobs "
               "Y-colliding-grobs "
              );
