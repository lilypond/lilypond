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

#include "tuplet-number.hh"
#include "axis-group-interface.hh"
#include "beam.hh"
#include "directional-element-interface.hh"
#include "lookup.hh"
#include "moment.hh" // needed?
#include "note-column.hh"
#include "paper-column.hh"
#include "pointer-group-interface.hh"
#include "spanner.hh"
#include "staff-symbol-referencer.hh"
#include "stem.hh"
#include "text-interface.hh"
#include "tuplet-bracket.hh"
#include "warn.hh"

using std::vector;

/*
  The reference stem is used to determine on which side of the beam to place
  the tuplet number when it is positioned independently of a bracket.  (The
  number is always placed on the opposite side of this stem.)
*/
Grob *
Tuplet_number::select_reference_stem (Grob *me_grob, vector<Grob *> const &cols)
{
  Spanner *me = dynamic_cast<Spanner *> (me_grob);

  vsize col_count = cols.size ();

  if (!col_count)
    return 0;

  /*
    When we have an odd number of stems, we choose the middle stem as
    our reference.
  */
  Grob *ref_stem = Note_column::get_stem (cols[col_count / 2]);

  if (col_count % 2 == 1)
    return ref_stem;

  /*
    When we have an even number of stems, we choose between the central
    two stems.
  */
  Direction me_dir = robust_scm2dir (me->get_property ("direction"), UP);
  Drul_array<Item *> bounding_stems (
      Note_column::get_stem (cols[col_count / 2 - 1]),
      Note_column::get_stem (cols[col_count / 2]));

  for (LEFT_and_RIGHT (d))
    if (!bounding_stems[d])
      return bounding_stems[-d];

  /*
    If the central stems point in opposite directions, the number may
    be placed on either side unless there is a fractional beam, in which
    case the number goes opposite to the partial beam.

    When there is an option, we use the setting of TupletNumber.direction.

    If the central stems are in the same direction, it doesn't matter
    which is used as the reference.  We use the one on the left.
  */
  Direction dir_left = get_grob_direction (bounding_stems[LEFT]);
  Direction dir_right = get_grob_direction (bounding_stems[RIGHT]);

  if (dir_left == dir_right)
    ref_stem = bounding_stems[LEFT];
  else
    {
      int beam_count_L_R = Stem::get_beaming (bounding_stems[LEFT], RIGHT);
      int beam_count_R_L = Stem::get_beaming (bounding_stems[RIGHT], LEFT);
      if (beam_count_L_R == beam_count_R_L)
        ref_stem = (dir_left == me_dir) ? bounding_stems[LEFT]
                                        : bounding_stems[RIGHT];
      else
        ref_stem = (beam_count_L_R > beam_count_R_L) ? bounding_stems[LEFT]
                                                     : bounding_stems[RIGHT];
    }

  return ref_stem;
}

/*
  When we place the number close to the beam, we need to consider the note
  columns adjoining the tuplet number on the same side of the beam.  The
  number may not fit in the available space, or may need to be shifted
  horizontally out of the way of stems and ledger lines.
*/
Drul_array<Grob *>
Tuplet_number::adjacent_note_columns (Grob *me_grob, Grob *ref_stem)
{
  Spanner *me = dynamic_cast<Spanner *> (me_grob);
  Spanner *tuplet = unsmob<Spanner> (me->get_object ("bracket"));

  extract_grob_set (tuplet, "note-columns", columns);
  Grob *ref_col
      = ref_stem->get_parent (X_AXIS); // X-parent of Stem = NoteColumn
  Direction ref_stem_dir = get_grob_direction (ref_stem);
  vector<Grob *> filtered_cols;
  vsize ref_pos = 0;

  for (vsize i = 0, counter = 0; i < columns.size (); ++i)
    {
      Grob *stem = Note_column::get_stem (columns[i]);
      if (stem && get_grob_direction (stem) == -ref_stem_dir)
        {
          filtered_cols.push_back (columns[i]);
          ++counter;
        }
      if (columns[i] == ref_col)
        {
          filtered_cols.push_back (columns[i]);
          ref_pos = counter;
        }
    }

  Drul_array<Grob *> adj_cols (0, 0);

  if (ref_pos > 0)
    adj_cols[LEFT] = filtered_cols[ref_pos - 1];
  if (ref_pos < filtered_cols.size () - 1)
    adj_cols[RIGHT] = filtered_cols[ref_pos + 1];

  return adj_cols;
}

/*
  We determine whether our tuplet number will be put next to the beam
  independently of the positioning of the associated tuplet bracket.

  Draw next to the beam if:
  --bracket isn't visible, AND
  --there is a beam above or below the number, AND
  --this beam is kneed, AND
  --the tuplet number will fit between adjoining note columns
*/
bool
Tuplet_number::knee_position_against_beam (Grob *me_grob, Grob *ref_stem)
{
  Spanner *me = dynamic_cast<Spanner *> (me_grob);
  Spanner *tuplet = unsmob<Spanner> (me->get_object ("bracket"));

  bool bracket_visible = to_boolean (me->get_property ("bracket-visibility"))
                         || !tuplet->extent (tuplet, Y_AXIS).is_empty ();

  if (bracket_visible || !to_boolean (me->get_property ("knee-to-beam")))
    return false;

  Grob *beam = Stem::get_beam (ref_stem);

  if (!beam || !Beam::is_knee (beam))
    return false;

  Grob *commonx = Tuplet_bracket::get_common_x (tuplet);
  commonx = commonx->common_refpoint (me, X_AXIS);

  Interval number_ext = me->extent (commonx, X_AXIS);

  Drul_array<Grob *> adj_cols = adjacent_note_columns (me, ref_stem);

  Item *left = me->get_bound (LEFT);
  Item *right = me->get_bound (RIGHT);

  if (!left || !right)
    return false;

  Drul_array<Item *> bounds (left, right);

  Interval available_ext;
  Real padding = robust_scm2double (me->get_property ("padding"), 0.5);

  /*
     If there is no note column on a given side of the tuplet number, we use
     a paper column instead to determine the available space.  Padding is only
     considered in the case of a note column.
  */
  for (LEFT_and_RIGHT (d))
    {
      if (adj_cols[d])
        available_ext[d] = Axis_group_interface::generic_bound_extent (
                               adj_cols[d], commonx, X_AXIS)[-d]
                           + (-d * padding);
      else
        available_ext[d] = Axis_group_interface::generic_bound_extent (
            bounds[d], commonx, X_AXIS)[-d];
    }

  if (number_ext.length () > available_ext.length ())
    {
      programming_error ("not enough space for tuplet number against beam");
      return false;
    }

  return true;
}

MAKE_SCHEME_CALLBACK (Tuplet_number, print, 1);
SCM
Tuplet_number::print (SCM smob)
{
  Spanner *me = unsmob<Spanner> (smob);
  Spanner *tuplet = unsmob<Spanner> (me->get_object ("bracket"));

  if (!tuplet || !tuplet->is_live ())
    {
      me->suicide ();
      return SCM_EOL;
    }

  SCM stc_scm = Text_interface::print (smob);
  Stencil *stc = unsmob<Stencil> (stc_scm);

  stc->align_to (X_AXIS, CENTER);
  stc->align_to (Y_AXIS, CENTER);

  return stc->smobbed_copy ();
}

/*
  For a given horizontal displacement of the tuplet number, how much
  vertical shift is necessary to keep it the same distance from the beam?
*/
Real
calc_beam_y_shift (Grob *ref_stem, Real dx)
{
  Grob *beam = Stem::get_beam (ref_stem);
  Interval x_pos = robust_scm2interval (beam->get_property ("X-positions"),
                                        Interval (0.0, 0.0));
  Interval y_pos = robust_scm2interval (
      beam->get_property ("quantized-positions"), Interval (0.0, 0.0));
  Real beam_dx = x_pos.length ();
  Real beam_dy = y_pos[RIGHT] - y_pos[LEFT];
  Real slope = beam_dx ? beam_dy / beam_dx : 0.0;

  return (slope * dx);
}

/*
  The X- and Y-offset of the tuplet number are calculated in relation either
  to the bracket associated with it, or with the beam it is placed against.
*/

MAKE_SCHEME_CALLBACK (Tuplet_number, calc_x_offset, 1);
SCM
Tuplet_number::calc_x_offset (SCM smob)
{
  Spanner *me = unsmob<Spanner> (smob);

  Item *left_bound = me->get_bound (LEFT);
  Item *right_bound = me->get_bound (RIGHT);
  Drul_array<Item *> bounds (left_bound, right_bound);

  Spanner *tuplet = unsmob<Spanner> (me->get_object ("bracket"));

  Grob *commonx = Tuplet_bracket::get_common_x (tuplet);
  commonx = commonx->common_refpoint (me, X_AXIS);

  Interval bound_poss;

  for (LEFT_and_RIGHT (d))
    {
      if (has_interface<Note_column> (bounds[d])
          && Note_column::get_stem (bounds[d]))
        bounds[d] = Note_column::get_stem (bounds[d]);
      bound_poss[d] = Axis_group_interface::generic_bound_extent (
          bounds[d], commonx, X_AXIS)[-d];
    }

  extract_grob_set (tuplet, "note-columns", cols);
  Grob *ref_stem = select_reference_stem (me, cols);

  /*
    Return bracket-based positioning.
  */
  if (!ref_stem || !knee_position_against_beam (me, ref_stem))
    {
      Interval x_positions;
      x_positions = robust_scm2interval (tuplet->get_property ("X-positions"),
                                         Interval (0.0, 0.0));
      return scm_from_double (x_positions.center ());
    }

  /*
     Horizontally center the number on the beam.
  */
  Real col_pos = left_bound->relative_coordinate (commonx, X_AXIS);
  Real x_offset = bound_poss.center () - col_pos;

  /*
    Consider possible collisions with adjacent note columns.
  */
  Drul_array<Grob *> adj_cols = adjacent_note_columns (me, ref_stem);
  Interval number_ext = me->extent (commonx, X_AXIS);
  number_ext.translate (x_offset);
  Real padding = robust_scm2double (me->get_property ("padding"), 0.5);
  number_ext.widen (padding);

  Interval cor (0.0, 0.0);

  for (LEFT_and_RIGHT (d))
    if (adj_cols[d])
      {
        Interval nc_ext = adj_cols[d]->extent (commonx, X_AXIS);
        Interval overlap (nc_ext);
        overlap.intersect (number_ext);
        if (!overlap.is_empty ())
          cor[d] = overlap.length () * -d;
        x_offset += cor[d];
      }

  return scm_from_double (x_offset);
}

/*
  When a number is placed against the beam (independently of a bracket), the
  Y-extent of a reference stem is used to determine the vertical placement of
  the number.  When French beams are used the stem may not reach all beams.
*/
int
count_beams_not_touching_stem (SCM beaming)
{
  int count = 0;

  for (SCM s = scm_car (beaming); scm_is_pair (s); s = scm_cdr (s))
    {
      if (scm_is_true (ly_memv (scm_car (s), scm_cdr (beaming))))
        ++count;
    }

  return std::max (0, count - 1);
}

MAKE_SCHEME_CALLBACK (Tuplet_number, calc_y_offset, 1);
SCM
Tuplet_number::calc_y_offset (SCM smob)
{
  Spanner *me = unsmob<Spanner> (smob);
  Spanner *tuplet = unsmob<Spanner> (me->get_object ("bracket"));
  Drul_array<Real> positions = robust_scm2drul (
      tuplet->get_property ("positions"), Drul_array<Real> (0.0, 0.0));
  SCM to_bracket = scm_from_double ((positions[LEFT] + positions[RIGHT]) / 2.0);

  Grob *commonx = Tuplet_bracket::get_common_x (me);
  commonx = commonx->common_refpoint (me, X_AXIS);
  Real x_coord = me->relative_coordinate (commonx, X_AXIS);
  extract_grob_set (tuplet, "note-columns", columns);
  Grob *ref_stem = select_reference_stem (me, columns);

  if (!ref_stem || !knee_position_against_beam (me, ref_stem))
    return to_bracket;

  /*
    First, we calculate the Y-offset of the tuplet number as if it
    is positioned at the reference stem.
  */
  Grob *commony = common_refpoint_of_array (columns, tuplet, Y_AXIS);
  commony = commony->common_refpoint (me, Y_AXIS);
  extract_grob_set (me, "tuplets", tuplets);
  commony = common_refpoint_of_array (tuplets, commony, Y_AXIS);
  if (Grob *st = Staff_symbol_referencer::get_staff_symbol (me))
    commony = st->common_refpoint (commony, Y_AXIS);

  Interval ref_stem_ext = ref_stem->extent (commony, Y_AXIS);
  Real tuplet_y = tuplet->relative_coordinate (commony, Y_AXIS);
  Direction ref_stem_dir = get_grob_direction (ref_stem);

  Real y_offset = ref_stem_ext[ref_stem_dir] - tuplet_y;

  /*
    Additional displacement for French beaming.
  */
  if (to_boolean (ref_stem->get_property ("french-beaming")))
    {
      Grob *beam = Stem::get_beam (ref_stem);
      Real beam_translation = Beam::get_beam_translation (beam);
      SCM beaming = ref_stem->get_property ("beaming");
      y_offset += ref_stem_dir * count_beams_not_touching_stem (beaming)
                  * beam_translation;
    }

  Real padding = robust_scm2double (me->get_property ("padding"), 0.5);
  Real num_height = me->extent (commony, Y_AXIS).length ();

  y_offset += ref_stem_dir * (padding + num_height / 2.0);

  /*
    Now we adjust the vertical position of the number to reflect
    its actual horizontal placement along the beam.
  */
  Real ref_stem_x = ref_stem->relative_coordinate (commonx, X_AXIS);
  y_offset += calc_beam_y_shift (ref_stem, x_coord - ref_stem_x);

  /*
    Check if the number is between the beam and the staff.  If so, it will
    collide with ledger lines.  Move it into the staff.
  */
  if (Grob *st = Staff_symbol_referencer::get_staff_symbol (ref_stem))
    {
      Interval staff_ext_y = st->extent (commony, Y_AXIS);
      bool move = ref_stem_dir == DOWN ? ref_stem_ext[DOWN] > staff_ext_y[UP]
                                       : staff_ext_y[DOWN] > ref_stem_ext[UP];
      if (move)
        {
          Interval ledger_domain
              = Interval (std::min (staff_ext_y[UP], ref_stem_ext[UP]),
                          std::max (staff_ext_y[DOWN], ref_stem_ext[DOWN]));
          Interval num_y (me->extent (commony, Y_AXIS));
          num_y.translate (y_offset);
          Interval num_ledger_overlap (num_y);
          num_ledger_overlap.intersect (ledger_domain);
          Real line_thickness = Staff_symbol_referencer::line_thickness (st);
          Real staff_space = Staff_symbol_referencer::staff_space (st);
          // Number will touch outer staff line.
          if (!num_ledger_overlap.is_empty ()
              && num_ledger_overlap.length () > (staff_space / 2.0) && move)
            y_offset += staff_ext_y[-ref_stem_dir] - num_y[-ref_stem_dir]
                        + line_thickness * ref_stem_dir;
        }
    }

  /*
    Now consider possible collisions with accidentals on the right.  We
    move the accidental away from the beam.
  */
  Drul_array<Grob *> adj_cols = adjacent_note_columns (me, ref_stem);

  if (!adj_cols[RIGHT])
    return scm_from_double (y_offset);

  /*
    Collect Y-extents of accidentals that overlap the number
    along the X-axis.
  */
  extract_grob_set (adj_cols[RIGHT], "note-heads", heads);
  Interval colliding_acc_ext_y;

  for (vsize i = 0; i < heads.size (); i++)
    if (Grob *acc = unsmob<Grob> (heads[i]->get_object ("accidental-grob")))
      {
        commony = commony->common_refpoint (acc, Y_AXIS);
        Interval acc_ext_y = acc->extent (commony, Y_AXIS);

        commonx = commonx->common_refpoint (acc, X_AXIS);
        Interval num_ext_x = me->extent (commonx, X_AXIS);
        num_ext_x.widen (padding);
        Interval overlap_x (num_ext_x);
        Interval acc_x = acc->extent (commonx, X_AXIS);
        overlap_x.intersect (acc_x);

        if (!overlap_x.is_empty ())
          colliding_acc_ext_y.unite (acc_ext_y);
      }
  /*
    Does our number intersect vertically with the accidental Y-extents we
    combined above?  If so, move it.
  */
  Interval overlap_acc_y (colliding_acc_ext_y);
  Interval num_ext_y (me->extent (commony, Y_AXIS));
  num_ext_y.translate (y_offset);
  overlap_acc_y.intersect (num_ext_y);

  if (!overlap_acc_y.is_empty ())
    y_offset += colliding_acc_ext_y[ref_stem_dir] - num_ext_y[-ref_stem_dir]
                + padding * ref_stem_dir;

  return scm_from_double (y_offset);
}

MAKE_SCHEME_CALLBACK (Tuplet_number, calc_cross_staff, 1)
SCM
Tuplet_number::calc_cross_staff (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);
  return unsmob<Grob> (me->get_object ("bracket"))
      ->get_property ("cross-staff");
}

ADD_INTERFACE (Tuplet_number, "The number for a bracket.",

               /* properties */
               "avoid-slur " // UGH.
               "bracket "
               "direction "
               "knee-to-beam ");
