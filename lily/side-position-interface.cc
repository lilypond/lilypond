/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "side-position-interface.hh"

#include "accidental-interface.hh"
#include "accidental-placement.hh"
#include "axis-group-interface.hh"
#include "directional-element-interface.hh"
#include "grob.hh"
#include "grob-array.hh"
#include "international.hh"
#include "item.hh"
#include "main.hh"
#include "misc.hh"
#include "note-head.hh"
#include "note-column.hh"
#include "pointer-group-interface.hh"
#include "skyline-pair.hh"
#include "staff-grouper-interface.hh"
#include "staff-symbol-referencer.hh"
#include "staff-symbol.hh"
#include "stem.hh"
#include "string-convert.hh"
#include "system.hh"
#include "warn.hh"
#include "unpure-pure-container.hh"

#include <algorithm>
#include <cmath> // ceil.
#include <map>
#include <set>

using std::set;
using std::string;
using std::vector;

void
Side_position_interface::add_support (Grob *me, Grob *e)
{
  Pointer_group_interface::add_unordered_grob (
    me, ly_symbol2scm ("side-support-elements"), e);
}

set<Grob *>
get_support_set (Grob *me)
{
  // Only slightly kludgy heuristic...
  // We want to make sure that all AccidentalPlacements'
  // accidentals make it into the side support
  extract_grob_set (me, "side-support-elements", proto_support);
  set<Grob *> support;

  for (vsize i = 0; i < proto_support.size (); i++)
    {
      if (has_interface<Accidental_placement> (proto_support[i]))
        {
          Grob *accs = proto_support[i];
          for (SCM acs = get_object (accs, "accidental-grobs");
               scm_is_pair (acs); acs = scm_cdr (acs))
            for (SCM s = scm_cdar (acs); scm_is_pair (s); s = scm_cdr (s))
              {
                Grob *a = unsmob<Grob> (scm_car (s));
                support.insert (a);
              }
        }
      else
        support.insert (proto_support[i]);
    }
  return support;
}

/*
  Position next to support, taking into account my own dimensions and padding.
*/
static SCM
axis_aligned_side_helper (Grob *me, Axis a, bool pure, int start, int end,
                          SCM current_off_scm)
{
  Real r;
  Real *current_off_ptr = 0;
  if (scm_is_number (current_off_scm))
    {
      r = from_scm<double> (current_off_scm);
      current_off_ptr = &r;
    }

  // We will only ever want widths of spanners after line breaking
  // so we can set pure to false
  if ((a == X_AXIS) && dynamic_cast<Spanner *> (me))
    pure = false;

  return Side_position_interface::aligned_side (me, a, pure, start, end,
                                                current_off_ptr);
}

MAKE_SCHEME_CALLBACK_WITH_OPTARGS (Side_position_interface, x_aligned_side,
                                   "ly:side-position-interface::x-aligned-side",
                                   2, 1, "");
SCM
Side_position_interface::x_aligned_side (SCM smob, SCM current_off)
{
  // Because horizontal skylines need vertical heights, we'd trigger
  // unpure calculations too soon if this were called before line breaking.
  // So, we always use pure heights.  Given that horizontal skylines are
  // almost always used before line breaking anyway, this doesn't cause
  // problems.
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  return axis_aligned_side_helper (me, X_AXIS, true, 0, 0, current_off);
}

MAKE_SCHEME_CALLBACK_WITH_OPTARGS (Side_position_interface, y_aligned_side,
                                   "ly:side-position-interface::y-aligned-side",
                                   2, 1, "");
SCM
Side_position_interface::y_aligned_side (SCM smob, SCM current_off)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  return axis_aligned_side_helper (me, Y_AXIS, false, 0, 0, current_off);
}

MAKE_SCHEME_CALLBACK_WITH_OPTARGS (
  Side_position_interface, pure_y_aligned_side,
  "ly:side-position-interface::pure-y-aligned-side", 4, 1, "");
SCM
Side_position_interface::pure_y_aligned_side (SCM smob, SCM start, SCM end,
                                              SCM cur_off)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  return axis_aligned_side_helper (me, Y_AXIS, true, from_scm<int> (start),
                                   from_scm<int> (end), cur_off);
}

MAKE_SCHEME_CALLBACK (Side_position_interface, calc_cross_staff,
                      "ly:side-position-interface::calc-cross-staff", 1)
SCM
Side_position_interface::calc_cross_staff (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  extract_grob_set (me, "side-support-elements", elts);

  Direction my_dir = get_grob_direction (me);

  for (vsize i = 0; i < elts.size (); i++)
    {
      /*
        If 'me' is placed relative to any cross-staff element with a
        'direction callback defined, the placement of 'me' is likely
        to depend on staff-spacing, thus 'me' should be considered
        cross-staff.
      */
      if (from_scm<bool> (get_property (elts[i], "cross-staff"))
          && !is_scm<Direction> (get_property_data (elts[i], "direction")))
        return SCM_BOOL_T;

      /*
        If elts[i] is cross-staff and is pointing in the same
        direction as 'me', we assume that the alignment
        of 'me' is influenced the cross-staffitude of elts[i]
        and thus we mark 'me' as cross-staff.
      */
      if (from_scm<bool> (get_property (elts[i], "cross-staff"))
          && my_dir == get_grob_direction (elts[i]))
        return SCM_BOOL_T;
    }

  Grob *myvag = Grob::get_vertical_axis_group (me);
  for (vsize i = 0; i < elts.size (); i++)
    if (myvag != Grob::get_vertical_axis_group (elts[i]))
      return SCM_BOOL_T;

  return SCM_BOOL_F;
}

// long function - each stage is clearly marked

SCM
Side_position_interface::aligned_side (Grob *me, Axis a, bool pure, int start,
                                       int end, Real *current_off)
{
  Direction dir = get_grob_direction (me);

  if (!dir)
    {
      // This is occasionally useful, for example to place
      // scripts in the middle of two piano staves using a
      // Dynamics context.
      return to_scm (current_off ? *current_off : 0.0);
    }

  set<Grob *> support = get_support_set (me);

  Grob *common[NO_AXES];
  for (const auto ax : {X_AXIS, Y_AXIS})
    {
      common[ax] = common_refpoint_of_array (
        support, (ax == a ? me->get_parent (ax) : me), ax);
    }

  Grob *staff_symbol = Staff_symbol_referencer::get_staff_symbol (me);
  bool quantize_position = from_scm<bool> (
    get_maybe_pure_property (me, "quantize-position", pure, start, end));
  bool me_cross_staff = from_scm<bool> (get_property (me, "cross-staff"));

  bool include_staff = staff_symbol && a == Y_AXIS
                       && scm_is_number (get_maybe_pure_property (
                         me, "staff-padding", pure, start, end))
                       && !quantize_position;

  if (include_staff)
    common[Y_AXIS] = staff_symbol->common_refpoint (common[Y_AXIS], Y_AXIS);

  Skyline my_dim (-dir);
  SCM skyp_scm = get_maybe_pure_property (
    me, a == X_AXIS ? "horizontal-skylines" : "vertical-skylines", pure, start,
    end);
  if (is_scm<Skyline_pair> (skyp_scm))
    {
      // for spanner pure heights, we don't know horizontal spacing,
      // so a spanner can never have a meaningful x coordiante
      // we just give it the parents' coordinate because its
      // skyline will likely be of infinite width anyway
      // and we don't want to prematurely trigger H spacing
      Real xc;
      if (a == X_AXIS)
        xc = me->parent_relative (common[X_AXIS], X_AXIS);
      else // Y_AXIS
        {
          if (!pure)
            xc = me->relative_coordinate (common[X_AXIS], X_AXIS);
          else
            // Not safe to call X-offset callbacks here as that may
            // trigger stem and beam direction, so just set to 0
            xc = 0;
        }

      // same here, for X_AXIS spacing, if it's happening, it should only be
      // before line breaking.  because there is no thing as "pure" x spacing,
      // we assume that it is all pure
      Real yc = a == X_AXIS
                  ? me->pure_relative_y_coordinate (common[Y_AXIS], start, end)
                  : me->get_y_parent ()->maybe_pure_coordinate (
                    common[Y_AXIS], Y_AXIS, pure, start, end);
      Skyline_pair skyp = from_scm<Skyline_pair> (skyp_scm);
      skyp.shift (a == X_AXIS ? yc : xc);
      skyp.raise (a == X_AXIS ? xc : yc);
      my_dim = skyp[-dir];
    }

  vector<Box> boxes;
  vector<Skyline_pair> skyps;
  set<Grob *>::iterator it;

  for (it = support.begin (); it != support.end (); it++)
    {
      Grob *e = *it;

      bool cross_staff = from_scm<bool> (get_property (e, "cross-staff"));
      if (a == Y_AXIS
          && !me_cross_staff // 'me' promised not to adapt to staff-spacing
          && cross_staff)    // but 'e' might move based on staff-pacing
        continue;            // so 'me' may not move in response to 'e'

      if (a == Y_AXIS && has_interface<Stem> (e))
        {
          // If called as 'pure' we may not force a stem to set its direction,
          if (pure && !is_scm<Direction> (get_property_data (e, "direction")))
            continue;
          // There is no need to consider stems pointing away.
          if (dir == -get_grob_direction (e))
            continue;
        }

      if (e)
        {
          SCM skyp_scm = get_maybe_pure_property (
            e, a == X_AXIS ? "horizontal-skylines" : "vertical-skylines", pure,
            start, end);

          if (is_scm<Skyline_pair> (skyp_scm))
            {
              Real xc = pure && dynamic_cast<Spanner *> (e)
                          ? e->parent_relative (common[X_AXIS], X_AXIS)
                          : e->relative_coordinate (common[X_AXIS], X_AXIS);
              // same logic as above
              // we assume horizontal spacing is always pure
              Real yc
                = a == X_AXIS
                    ? e->pure_relative_y_coordinate (common[Y_AXIS], start, end)
                    : e->maybe_pure_coordinate (common[Y_AXIS], Y_AXIS, pure,
                                                start, end);
              Skyline_pair skyp = from_scm<Skyline_pair> (skyp_scm);
              if (a == Y_AXIS && has_interface<Stem> (e)
                  && from_scm<bool> (get_maybe_pure_property (
                    me, "add-stem-support", pure, start, end)))
                skyp[dir].set_minimum_height (skyp[dir].max_height ());
              skyp.shift (a == X_AXIS ? yc : xc);
              skyp.raise (a == X_AXIS ? xc : yc);
              skyps.push_back (skyp);
            }
          else
            { /* no warning*/
            }
        }
    }

  Skyline dim (boxes, other_axis (a), dir);
  if (skyps.size ())
    {
      Skyline_pair merged (skyps);
      dim.merge (merged[dir]);
    }

  if (include_staff)
    {
      Interval staff_extents;
      common[Y_AXIS] = staff_symbol->common_refpoint (common[Y_AXIS], Y_AXIS);
      staff_extents = staff_symbol->maybe_pure_extent (common[Y_AXIS], Y_AXIS,
                                                       pure, start, end);
      dim.set_minimum_height (staff_extents[dir]);
    }

  // Sometimes, we want to side position for grobs but they
  // don't position against anything.  Some cases where this is true:
  //   - StanzaNumber if the supporting lyrics are hara-kiri'd
  //     SystemStartBracket
  //     InstrumentName
  // In all these cases, we set the height of the support to 0.
  // This becomes then like the self-alignment-interface with the
  // caveat that there is padding added.
  // TODO: if there is a grob that never has side-support-elements
  // (like InstrumentName), why are we using this function? Isn't it
  // overkill? A function like self-alignment-interface with padding
  // works just fine.
  // One could even imagine the two interfaces merged, as the only
  // difference is that in self-alignment-interface we align on the parent
  // where as here we align on a group of grobs.
  if (dim.is_empty ())
    {
      dim = Skyline (dim.direction ());
      dim.set_minimum_height (0.0);
    }

  Real ss = Staff_symbol_referencer::staff_space (me);
  Real dist = dim.distance (
    my_dim,
    from_scm<double> (
      get_maybe_pure_property (me, "horizon-padding", pure, start, end), 0.0));
  Real total_off = !std::isinf (dist) ? dir * dist : 0.0;

  total_off
    += dir * ss
       * from_scm<double> (
         get_maybe_pure_property (me, "padding", pure, start, end), 0.0);

  Real minimum_space
    = ss
      * from_scm<double> (
        get_maybe_pure_property (me, "minimum-space", pure, start, end), -1);

  if (minimum_space >= 0 && dir && total_off * dir < minimum_space)
    total_off = minimum_space * dir;

  if (current_off)
    total_off = dir * std::max (dir * total_off, dir * (*current_off));

  /* FIXME: 1000 should relate to paper size.  */
  if (fabs (total_off) > 1000)
    {
      string msg = String_convert::form_string (
        "Improbable offset for grob %s: %f", me->name ().c_str (), total_off);

      programming_error (msg);
      if (strict_infinity_checking)
        scm_misc_error (__FUNCTION__, "Improbable offset.", SCM_EOL);
    }

  /*
    Ensure 'staff-padding' from my refpoint to the staff.  This is similar to
    side-position with padding, but it will put adjoining objects on a row if
    stuff sticks out of the staff a little.
  */
  Grob *staff = Staff_symbol_referencer::get_staff_symbol (me);
  if (staff && a == Y_AXIS)
    {
      if (quantize_position)
        {
          Grob *common = me->common_refpoint (staff, Y_AXIS);
          Real my_off = me->get_y_parent ()->maybe_pure_coordinate (
            common, Y_AXIS, pure, start, end);
          Real staff_off
            = staff->maybe_pure_coordinate (common, Y_AXIS, pure, start, end);
          Real ss = Staff_symbol::staff_space (staff);
          Real position = 2 * (my_off + total_off - staff_off) / ss;
          Real rounded = directed_round (position, dir);
          Grob *head = me->get_x_parent ();

          Interval staff_span = Staff_symbol::line_span (staff);
          staff_span.widen (1);
          if (
            staff_span.contains (position)
            /* If we are between notehead and staff, quantize for ledger lines. */
            || (has_interface<Note_head> (head) && dir * position < 0))
            {
              total_off += (rounded - position) * 0.5 * ss;
              if (Staff_symbol_referencer::on_line (me, int (rounded)))
                total_off += dir * 0.5 * ss;
            }
        }
      else if (scm_is_number (get_maybe_pure_property (me, "staff-padding",
                                                       pure, start, end))
               && dir)
        {
          Real staff_padding = Staff_symbol_referencer::staff_space (me)
                               * from_scm<double> (get_maybe_pure_property (
                                 me, "staff-padding", pure, start, end));

          Grob *parent = me->get_y_parent ();
          Grob *common = me->common_refpoint (staff, Y_AXIS);
          Real parent_position
            = parent->maybe_pure_coordinate (common, Y_AXIS, pure, start, end);
          Real staff_position
            = staff->maybe_pure_coordinate (common, Y_AXIS, pure, start, end);
          Interval staff_extent
            = staff->maybe_pure_extent (staff, a, pure, start, end);
          Real diff = (dir * staff_extent[dir] + staff_padding - dir * total_off
                       + dir * (staff_position - parent_position));
          total_off += dir * std::max (diff, 0.0);
        }
    }
  return to_scm (total_off);
}

void
Side_position_interface::set_axis (Grob *me, Axis a)
{
  if (!scm_is_number (get_property (me, "side-axis")))
    {
      set_property (me, "side-axis", to_scm (a));
      chain_offset_callback (me,
                             (a == X_AXIS)
                               ? x_aligned_side_proc
                               : Unpure_pure_container::make_smob (
                                 y_aligned_side_proc, pure_y_aligned_side_proc),
                             a);
    }
}

MAKE_SCHEME_CALLBACK (Side_position_interface, set_axis_x,
                      "ly:side-position-interface::set-axis!", 2);
SCM
Side_position_interface::set_axis_x (SCM grob, SCM axis)
{
  auto *const g = LY_ASSERT_SMOB (Grob, grob, 1);
  LY_ASSERT_TYPE (is_scm<Axis>, axis, 2);
  Axis a = from_scm<Axis> (axis);
  set_axis (g, a);
  return SCM_UNSPECIFIED;
}

static bool
is_on_axis (Grob *me, Axis a)
{
  SCM axis_scm = get_property (me, "side-axis");
  if (is_scm<Axis> (axis_scm))
    return from_scm<Axis> (axis_scm) == a;

  if (scm_is_true (get_property (me, "stencil")))
    me->programming_error (
      _f ("no side-axis setting found for grob %s.", me->name ().c_str ()));
  return false;
}

bool
Side_position_interface::is_on_x_axis (Grob *me)
{
  return is_on_axis (me, X_AXIS);
}

bool
Side_position_interface::is_on_y_axis (Grob *me)
{
  return is_on_axis (me, Y_AXIS);
}

MAKE_SCHEME_CALLBACK (Side_position_interface, move_to_extremal_staff,
                      "ly:side-position-interface::move-to-extremal-staff", 1);
SCM
Side_position_interface::move_to_extremal_staff (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);

  Direction dir = get_grob_direction (me);
  if (dir != DOWN)
    dir = UP;

  System *sys = me->get_system ();
  Interval iv = me->extent (sys, X_AXIS);
  iv.widen (1.0);

  Grob *grouper = me->get_y_parent ();
  if (has_interface<Staff_grouper_interface> (grouper))
    ; // find the extremal staff of this group
  else if (grouper == sys)
    {
      // find the extremal staff of the whole system
      grouper = unsmob<Grob> (get_object (sys, "vertical-alignment"));
      if (!grouper)
        return SCM_BOOL_F;
    }
  else // do not move marks from other staves to the top staff
    return SCM_BOOL_F;

  // N.B. It's ugly to pass a VerticalAlignment to this staff-grouper function.
  // Read the comments in the function for more detail.
  Grob *top_staff
    = Staff_grouper_interface::get_extremal_staff (grouper, sys, dir, iv);
  if (!top_staff)
    return SCM_BOOL_F;

  me->set_y_parent (top_staff);
  me->flush_extent_cache (Y_AXIS);
  Axis_group_interface::add_element (top_staff, me);

  // Remove any cross-staff side-support dependencies
  Grob_array *ga
    = unsmob<Grob_array> (get_object (me, "side-support-elements"));
  if (ga)
    {
      vector<Grob *> const &elts = ga->array ();
      vector<Grob *> new_elts;
      for (vsize i = 0; i < elts.size (); ++i)
        {
          if (me->common_refpoint (elts[i], Y_AXIS) == top_staff)
            new_elts.push_back (elts[i]);
        }
      ga->set_array (new_elts);
    }
  return SCM_BOOL_T;
}

ADD_INTERFACE (Side_position_interface,
               R"(
Position a victim object (this one) next to other objects (the support).  The
property @code{direction} signifies where to put the victim object relative to
the support (left or right, up or down?)

The routine also takes the size of the staff into account if
@code{staff-padding} is set.  If undefined, the staff symbol is ignored.
               )",

               /* properties */
               R"(
add-stem-support
direction
minimum-space
horizon-padding
padding
quantize-position
side-axis
side-support-elements
slur-padding
staff-padding
use-skylines
               )");
