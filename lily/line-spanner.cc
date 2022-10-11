/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2022 Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "align-interface.hh"
#include "axis-group-interface.hh"
#include "font-interface.hh"
#include "grob.hh"
#include "grob-interface.hh"
#include "item.hh"
#include "lily-imports.hh"
#include "lily-proto.hh"
#include "line-interface.hh"
#include "note-column.hh"
#include "output-def.hh"
#include "paper-column.hh"
#include "pointer-group-interface.hh"
#include "spanner.hh"
#include "staff-symbol-referencer.hh"
#include "system.hh"
#include "text-interface.hh"
#include "warn.hh"

#include <cassert>

/*
  There are basically two types of horizontal line spanners we
  want to distinguish between.  The non-horizontal ones (such as
  Glissando and VoiceFollower) usually compute Y positions automatically,
  and even when the positions are tweaked by the user, they try
  to make them relative to their containing vertical axis group.
  The horizontal ones (TextSpanner, DynamicTextSpanner, etc.)
  don't try to compute refpoints. In particular, for these, user
  tweaks to Y values are always relative to the spanner itself.
  This means that horizontal line spanners can be side-positioned
  without causing cyclic dependencies on their distance from the
  staff.
*/

class Line_spanner
{
public:
  static SCM calc_bound_info (SCM, Direction, bool);
  static SCM calc_left_bound_info_and_text (SCM, bool);
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_cross_staff, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_left_bound_info, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_left_bound_info_and_text, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_right_bound_info, (SCM));
};

class Horizontal_line_spanner
{
public:
  DECLARE_SCHEME_CALLBACK (calc_left_bound_info, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_left_bound_info_and_text, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_right_bound_info, (SCM));
};

Drul_array<Real>
offsets_maybe (Drul_array<Grob *> grobs, Grob *&common)
{
  Grob *g1 = grobs[LEFT];
  Grob *g2 = grobs[RIGHT];
  if (g1 && g2)
    {
      common = g1->common_refpoint (g2, Y_AXIS);
      Real coord1 = g1->relative_coordinate (common, Y_AXIS);
      Real coord2 = g2->relative_coordinate (common, Y_AXIS);
      return Drul_array<Real> (coord1, coord2);
    }
  else if (g1)
    {
      common = g1;
      Real coord1 = g1->relative_coordinate (common, Y_AXIS);
      // The 0.0 shouldn't get used.
      return Drul_array<Real> (coord1, 0.0);
    }
  else if (g2)
    {
      common = g2;
      Real coord2 = g2->relative_coordinate (common, Y_AXIS);
      return Drul_array<Real> (0.0, coord2);
    }
  else
    {
      common = nullptr;
      return Drul_array<Real> (0.0, 0.0);
    }
}

SCM
Line_spanner::calc_bound_info (SCM smob, Direction dir, bool horizontal)
{
  Spanner *me = LY_ASSERT_SMOB (Spanner, smob, 0);

  Item *bound_item = me->get_bound (dir);

  SCM bound_details = get_property (me, "bound-details");

  SCM details = ly_assoc_get ((dir == LEFT) ? ly_symbol2scm ("left")
                                            : ly_symbol2scm ("right"),
                              bound_details, SCM_BOOL_F);

  // Don't use bound_item->break_status_dir (): a spanner running to the end of
  // the piece has a broken right bound, but should not get details from
  // right-broken.
  Lily::Variable checker
    = ((dir == LEFT) ? Lily::unbroken_or_first_broken_spanner_p
                     : Lily::unbroken_or_last_broken_spanner_p);
  bool unfinished_at_bound = !from_scm<bool> (checker (smob));
  if (unfinished_at_bound)
    {
      SCM extra = ly_assoc_get ((dir == LEFT) ? ly_symbol2scm ("left-broken")
                                              : ly_symbol2scm ("right-broken"),
                                bound_details, SCM_EOL);

      details = ly_append (extra, scm_is_true (details) ? details : SCM_EOL);
    }

  if (scm_is_false (details))
    details = ly_assoc_get (ly_symbol2scm ("default"), bound_details, SCM_EOL);

  SCM text = ly_assoc_get (ly_symbol2scm ("text"), details, SCM_BOOL_F);
  if (Text_interface::is_markup (text))
    {
      Output_def *layout = me->layout ();
      SCM properties = Font_interface::text_font_alist_chain (me);
      details = scm_acons (ly_symbol2scm ("stencil"),
                           Text_interface::interpret_markup (
                             layout->self_scm (), properties, text),
                           details);
    }

  if (!scm_is_number (ly_assoc_get (ly_symbol2scm ("X"), details, SCM_BOOL_F)))
    {
      auto *commonx
        = me->get_bound (LEFT)->common_refpoint (me->get_bound (RIGHT), X_AXIS);
      commonx = me->common_refpoint (commonx, X_AXIS);

      Direction attach = from_scm (
        ly_assoc_get (ly_symbol2scm ("attach-dir"), details, SCM_BOOL_F),
        CENTER);

      Grob *bound_grob = bound_item;
      SCM end_note
        = ly_assoc_get (ly_symbol2scm ("end-on-note"), details, SCM_BOOL_F);
      if (from_scm<bool> (end_note) && unfinished_at_bound)
        {
          extract_grob_set (me, "note-columns", columns);
          if (!columns.empty ())
            {
              bound_grob = (dir == LEFT) ? columns.front () : columns.back ();
            }
        }

      Real x_coord = (has_interface<Paper_column> (bound_grob)
                        ? Axis_group_interface::generic_bound_extent (
                          bound_grob, commonx, X_AXIS)
                        : robust_relative_extent (bound_grob, commonx, X_AXIS))
                       .linear_combination (attach);

      SCM end_acc = ly_assoc_get (ly_symbol2scm ("end-on-accidental"), details,
                                  SCM_BOOL_F);
      if (from_scm<bool> (end_acc))
        {
          Grob *maybe_note_column = nullptr;
          // If the bound is already a note column, use it.
          if (has_interface<Note_column> (bound_grob))
            maybe_note_column = bound_grob;
          else
            {
              /* Our bound may be a note head or rest, so try the parent
                 axis group. */
              Grob *ag
                = unsmob<Grob> (get_object (bound_grob, "axis-group-parent-Y"));
              if (has_interface<Note_column> (ag))
                maybe_note_column = ag;
            }
          if (maybe_note_column)
            {
              if (Grob *acc_placement
                  = Note_column::accidentals (maybe_note_column))
                x_coord
                  = robust_relative_extent (acc_placement, commonx, X_AXIS)
                      .linear_combination (attach);
            }
        }

      Grob *dot = unsmob<Grob> (get_object (bound_grob, "dot"));
      if (dot
          && from_scm<bool> (
            ly_assoc_get (ly_symbol2scm ("start-at-dot"), details, SCM_BOOL_F)))
        x_coord = robust_relative_extent (dot, commonx, X_AXIS)
                    .linear_combination (attach);

      SCM adj = ly_assoc_get (ly_symbol2scm ("adjust-on-neighbor"), details,
                              SCM_BOOL_F);
      if (from_scm<bool> (adj))
        {
          SCM sym = (dir == LEFT) ? ly_symbol2scm ("left-neighbor")
                                  : ly_symbol2scm ("right-neighbor");
          Grob *neighbor = unsmob<Grob> (get_object (me, sym));
          if (neighbor)
            {
              Interval neighbor_ext = neighbor->extent (commonx, X_AXIS);
              Real neighbor_x = neighbor_ext[-dir];
              x_coord = (dir == LEFT) ? std::max (x_coord, neighbor_x)
                                      : std::min (x_coord, neighbor_x);
            }
        }

      details = scm_acons (ly_symbol2scm ("X"), to_scm (x_coord), details);
    }

  Grob *common_y;
  if (horizontal)
    {
      common_y = me;
    }
  else
    {
      bool y_needed = (!scm_is_number (
        ly_assoc_get (ly_symbol2scm ("Y"), details, SCM_BOOL_F)));
      // Even when we don't need to compute a Y value, run through part
      // of the code below in order to convey a reference point.  The
      // purpose is to make user tweaks to the Y value relative to the
      // relevant staff in the case of cross-staff line spanners.  Note
      // that this is not relevant for horizontal line spanners, the Y
      // value is always relative to the spanner itself.
      Real y = 0.0;

      if (unfinished_at_bound)
        {
          /*
            We want to compute the slope of something like a glissando
            when broken across several systems.  We make it continuous,
            giving the same slope to all pieces and choosing it such that
            visually it could be one glissando line if the systems were
            stuck together in a row.  To this end, we have to compute the
            vertical coordinates on two different systems and bring them
            to a common coordinate reference.  For normal glissandi, this
            is achieved by taking coordinates relative to the VerticalAxisGroup
            of the staff, which is broken just like the glissando.  For
            cross-staff glissandi, we schematically have this:

                                                      |
            -----------------------                   |
            -----------------------  \\               |
            -----------------------  \\            xxx|
            -----------------------  \\   /~~~~/---xxx|-----------
            -----------------------  \\ /~~~/---------------------
                                /  ~~~~ /-------------------------
          This line                 / ~~~~  \\  -------------------------
          might be            / ~~~~~       \\  -------------------------
          stretched...      ~~~~~~~/        \\                               ... while this
                  ~~~ /              \\  -------------------------    line might be
             |xxx                    \\  -------------------------    compressed.
             |xxx------------------  \\  -------------------------
            -|---------------------  \\  -------------------------
            -|---------------------  \\  -------------------------
            -|---------------------  \\
            -----------------------

                                 Line break
                                    here.


            The distance between the two staves can vary across the
            break.  There is not an obvious way to choose the common
            slope in this case.  If we take the lower staff as a
            reference baseline in the example above, the line on the left will
            have a small slope compared to the distance between the
            systems.  On the other hand, aligning the upper staves will
            result in a steep line on the second system.

            This code makes the choice of Solomon: align the middles
            of each pair of staves.  Blame Jean AS if you don't like
            it, and feel free to improve.
          */

          Spanner *orig = me->original ();
          System *sys_here = me->get_system ();
          System *sys_there;
          Drul_array<Item *> extreme_bounds;
          Drul_array<Grob *> extreme_bound_groups;
          for (const auto d : {LEFT, RIGHT})
            {
              Spanner *extreme = ((d == LEFT) ? orig->broken_intos_.front ()
                                              : orig->broken_intos_.back ());
              extreme_bounds[d] = extreme->get_bound (d);
              extreme_bound_groups[d]
                = Grob::get_vertical_axis_group (extreme_bounds[d]);
              if (!extreme_bound_groups[d])
                {
                  programming_error (
                    "extremal broken spanner's bound has no parent"
                    " vertical axis group");
                  return details;
                }
            }
          sys_there = extreme_bounds[dir]->get_system ();
          Drul_array<Grob *> extreme_bound_groups_here;
          Drul_array<Grob *> extreme_bound_groups_there;
          for (const auto d : {LEFT, RIGHT})
            {
              // This one can be null if the corresponding staff ended
              // prematurely or started after the beginning of the score.
              extreme_bound_groups_here[d]
                = extreme_bound_groups[d]->original ()->find_broken_piece (
                  sys_here);
              // Can be null for the direction other than dir.
              extreme_bound_groups_there[d]
                = extreme_bound_groups[d]->original ()->find_broken_piece (
                  sys_there);
            }
          Grob *common_here;
          Grob *common_there;
          Drul_array<Real> offsets_here
            = offsets_maybe (extreme_bound_groups_here, common_here);
          Real offset_here;
          Drul_array<Real> offsets_there
            = offsets_maybe (extreme_bound_groups_there, common_there);
          Real offset_there;

          if (y_needed)
            {
              // Here we have all weird edge cases that can happen
              // when staves are added or removed midway.  To continue
              // the example above, if the lower staff was removed on
              // the second system, we would align the upper staves.
              assert (extreme_bound_groups_there[dir]);
              if (!extreme_bound_groups_here[dir]
                  && !extreme_bound_groups_here[-dir])
                {
                  // If neither of the staves is present on this system, just
                  // disappear.  This can happen with contorted input that starts
                  // a glissando, stops that staff, then later spawns another
                  // staff and ends the glissando there.
                  me->suicide ();
                  return SCM_UNSPECIFIED;
                }
              if (extreme_bound_groups_there[-dir])
                {
                  if (extreme_bound_groups_here[dir]
                      && extreme_bound_groups_here[-dir])
                    {
                      offset_here
                        = (offsets_here[LEFT] + offsets_here[RIGHT]) / 2;
                      offset_there
                        = (offsets_there[LEFT] + offsets_there[RIGHT]) / 2;
                    }
                  else if (extreme_bound_groups_here[dir])
                    {
                      offset_here = offsets_here[dir];
                      offset_there = offsets_there[dir];
                    }
                  else
                    {
                      offset_here = offsets_here[-dir];
                      offset_there = offsets_there[-dir];
                    }
                }
              else // !extreme_bound_groups_there[-dir]
                {
                  if (extreme_bound_groups_here[dir])
                    {
                      offset_here = offsets_here[dir];
                      offset_there = offsets_there[dir];
                    }
                  else
                    {
                      offset_here = offsets_here[-dir];
                      offset_there = offsets_there[dir];
                    }
                }
              Interval extent
                = extreme_bounds[dir]->extent (common_there, Y_AXIS);
              Real coord_there = extent.center ();
              y = coord_there - offset_there + offset_here;
            }
          if (extreme_bound_groups_here[dir])
            {
              common_y = extreme_bound_groups_here[dir];
              if (y_needed)
                y -= offsets_here[dir];
            }
          else
            {
              common_y = common_here;
            }
        }
      else
        {
          common_y = Grob::get_vertical_axis_group (bound_item);
          if (!common_y)
            {
              programming_error (
                "bound item has no parent vertical axis group");
              common_y = bound_item;
            }
          if (y_needed)
            {
              Interval ii = bound_item->extent (common_y, Y_AXIS);
              if (!ii.is_empty ())
                y = ii.center ();
            }
        }

      if (y_needed)
        {
          Real extra_dy = from_scm<double> (get_property (me, "extra-dy"), 0.0);
          y += dir * extra_dy / 2;
          details = scm_acons (ly_symbol2scm ("Y"), to_scm (y), details);
        }
    }
  details
    = scm_acons (ly_symbol2scm ("common-Y"), common_y->self_scm (), details);
  return details;
}

MAKE_SCHEME_CALLBACK (Line_spanner, calc_right_bound_info,
                      "ly:line-spanner::calc-right-bound-info", 1);
SCM
Line_spanner::calc_right_bound_info (SCM smob)
{
  return Line_spanner::calc_bound_info (smob, RIGHT, false);
}

MAKE_SCHEME_CALLBACK (Line_spanner, calc_left_bound_info,
                      "ly:line-spanner::calc-left-bound-info", 1);
SCM
Line_spanner::calc_left_bound_info (SCM smob)
{
  return Line_spanner::calc_bound_info (smob, LEFT, false);
}

MAKE_SCHEME_CALLBACK (Horizontal_line_spanner, calc_right_bound_info,
                      "ly:horizontal-line-spanner::calc-right-bound-info", 1);
SCM
Horizontal_line_spanner::calc_right_bound_info (SCM smob)
{
  return Line_spanner::calc_bound_info (smob, RIGHT, true);
}

MAKE_SCHEME_CALLBACK (Horizontal_line_spanner, calc_left_bound_info,
                      "ly:horizontal-line-spanner::calc-left-bound-info", 1);
SCM
Horizontal_line_spanner::calc_left_bound_info (SCM smob)
{
  return Line_spanner::calc_bound_info (smob, LEFT, true);
}

SCM
Line_spanner::calc_left_bound_info_and_text (SCM smob, bool horizontal)
{
  SCM alist = Line_spanner::calc_bound_info (smob, LEFT, horizontal);
  Spanner *me = unsmob<Spanner> (smob);

  SCM text = get_property (me, "text");
  if (Text_interface::is_markup (text)
      && from_scm<bool> (Lily::unbroken_or_first_broken_spanner_p (smob))
      && scm_is_false (
        ly_assoc_get (ly_symbol2scm ("stencil"), alist, SCM_BOOL_F)))
    {
      Output_def *layout = me->layout ();
      SCM properties = Font_interface::text_font_alist_chain (me);
      alist = scm_acons (ly_symbol2scm ("stencil"),
                         Text_interface::interpret_markup (layout->self_scm (),
                                                           properties, text),
                         alist);
    }

  return alist;
}

MAKE_SCHEME_CALLBACK (Line_spanner, calc_left_bound_info_and_text,
                      "ly:line-spanner::calc-left-bound-info-and-text", 1);
SCM
Line_spanner::calc_left_bound_info_and_text (SCM smob)
{
  return Line_spanner::calc_left_bound_info_and_text (smob, false);
}

MAKE_SCHEME_CALLBACK (
  Horizontal_line_spanner, calc_left_bound_info_and_text,
  "ly:horizontal-line-spanner::calc-left-bound-info-and-text", 1);
SCM
Horizontal_line_spanner::calc_left_bound_info_and_text (SCM smob)
{
  return Line_spanner::calc_left_bound_info_and_text (smob, true);
}

// TODO: for horizontal line spanners, avoid looking at the
// right bound, and never mark cross-staff.
MAKE_SCHEME_CALLBACK (Line_spanner, calc_cross_staff,
                      "ly:line-spanner::calc-cross-staff", 1);
SCM
Line_spanner::calc_cross_staff (SCM smob)
{
  Spanner *me = unsmob<Spanner> (smob);
  if (!me)
    return SCM_BOOL_F;

  auto *const lb = me->get_bound (LEFT);
  auto *const rb = me->get_bound (RIGHT);
  return to_scm (Staff_symbol_referencer::get_staff_symbol (lb)
                 != Staff_symbol_referencer::get_staff_symbol (rb));
}

MAKE_SCHEME_CALLBACK (Line_spanner, print, "ly:line-spanner::print", 1);
SCM
Line_spanner::print (SCM smob)
{
  Spanner *me = unsmob<Spanner> (smob);

  Drul_array<SCM> bounds (get_property (me, "left-bound-info"),
                          get_property (me, "right-bound-info"));

  auto *commonx
    = me->get_bound (LEFT)->common_refpoint (me->get_bound (RIGHT), X_AXIS);
  commonx = me->common_refpoint (commonx, X_AXIS);

  Drul_array<Offset> span_points;

  for (const auto d : {LEFT, RIGHT})
    {
      Offset z (
        from_scm<double> (
          ly_assoc_get (ly_symbol2scm ("X"), bounds[d], SCM_BOOL_F), 0.0),
        from_scm<double> (
          ly_assoc_get (ly_symbol2scm ("Y"), bounds[d], SCM_BOOL_F), 0.0));

      span_points[d] = z;
    }

  Drul_array<Real> gaps;
  Drul_array<bool> arrows;
  Drul_array<const Stencil *> stencils;
  Drul_array<Grob *> common_y;

  // For scaling of 'padding and 'stencil-offset
  Real magstep
    = pow (2, from_scm<double> (get_property (me, "font-size"), 0.0) / 6);

  for (const auto d : {LEFT, RIGHT})
    {
      gaps[d] = from_scm<double> (
        ly_assoc_get (ly_symbol2scm ("padding"), bounds[d], SCM_BOOL_F), 0.0);
      arrows[d] = from_scm<bool> (
        ly_assoc_get (ly_symbol2scm ("arrow"), bounds[d], SCM_BOOL_F));
      stencils[d] = unsmob<const Stencil> (
        ly_assoc_get (ly_symbol2scm ("stencil"), bounds[d], SCM_BOOL_F));
      common_y[d] = unsmob<Grob> (
        ly_assoc_get (ly_symbol2scm ("common-Y"), bounds[d], SCM_BOOL_F));
      if (!common_y[d])
        {
          programming_error ("no common-Y in bound details");
          common_y[d] = me;
        }
    }

  Grob *my_common_y = common_y[LEFT]->common_refpoint (common_y[RIGHT], Y_AXIS);
  for (const auto d : {LEFT, RIGHT})
    span_points[d][Y_AXIS]
      += common_y[d]->relative_coordinate (my_common_y, Y_AXIS);

  Interval normalized_endpoints
    = from_scm (get_property (me, "normalized-endpoints"), Interval (0, 1));
  Real y_length = span_points[RIGHT][Y_AXIS] - span_points[LEFT][Y_AXIS];

  span_points[LEFT][Y_AXIS] += normalized_endpoints[LEFT] * y_length;
  span_points[RIGHT][Y_AXIS] -= (1 - normalized_endpoints[RIGHT]) * y_length;

  Offset dz = (span_points[RIGHT] - span_points[LEFT]);
  Offset dz_dir = dz.direction ();
  if (gaps[LEFT] + gaps[RIGHT] > dz.length ())
    {
      return SCM_EOL;
    }

  Stencil line;
  for (const auto d : {LEFT, RIGHT})
    {
      span_points[d] += -d * gaps[d] * magstep * dz.direction ();

      if (stencils[d])
        {
          Stencil s (*stencils[d]);
          SCM align = ly_assoc_get (ly_symbol2scm ("stencil-align-dir-y"),
                                    bounds[d], SCM_BOOL_F);
          SCM off = ly_assoc_get (ly_symbol2scm ("stencil-offset"), bounds[d],
                                  SCM_BOOL_F);

          if (scm_is_number (align))
            s.align_to (Y_AXIS, from_scm<double> (align));

          if (is_number_pair (off))
            s.translate (from_scm<Offset> (off) * magstep);

          s.translate (span_points[d]);

          line.add_stencil (s);
        }
    }

  for (const auto d : {LEFT, RIGHT})
    {
      if (stencils[d] && !stencils[d]->is_empty ())
        span_points[d]
          += dz_dir * (stencils[d]->extent (X_AXIS)[-d] / dz_dir[X_AXIS]);
    }

  Offset adjust = dz.direction () * Staff_symbol_referencer::staff_space (me);
  Offset line_left
    = span_points[LEFT] + (arrows[LEFT] ? adjust * 1.4 : Offset (0, 0));
  Offset line_right
    = span_points[RIGHT] - (arrows[RIGHT] ? adjust * 0.55 : Offset (0, 0));

  if (line_right[X_AXIS] > line_left[X_AXIS])
    {
      line.add_stencil (Line_interface::line (me, line_left, line_right));

      line.add_stencil (Line_interface::arrows (me, span_points[LEFT],
                                                span_points[RIGHT],
                                                arrows[LEFT], arrows[RIGHT]));
    }

  line.translate (Offset (-me->relative_coordinate (commonx, X_AXIS),
                          -me->relative_coordinate (my_common_y, Y_AXIS)));

  return line.smobbed_copy ();
}

ADD_INTERFACE (Line_spanner,
               R"(
Generic line drawn between two objects, e.g., for use with glissandi.

@code{bound-details} is a nested alist.  It's possible to specify
settings for the sub-properties: @code{left}, @code{left-broken},
@code{right} and @code{right-broken}.

Values for the following keys may be set:

@table @code
@item Y
Sets the Y@tie{}coordinate of the end point, in staff-spaces offset
from the staff center line.  By default, it is the center of the bound
object, so a glissando points to the vertical center of the note head.
Not relevant for grobs having the
@ref{horizontal-line-spanner-interface,horizontal-@/line-@/spanner-@/interface}.

@item attach-dir
Determines where the line starts and ends in the X@tie{}direction,
relative to the bound object.  So, a value of -1 (or @code{LEFT})
makes the line start/end at the left side of the note head it is
attached to.

@item X
This is the absolute X@tie{}coordinate of the end point.  Usually
computed on the fly.

@item end-on-note
If set to true, when the line spanner is broken, each broken piece
only extends to the furthest note, not to the end of the staff,
on sides where it is broken.

@item end-on-accidental
Only meaningful in @code{bound-details.right}.  If set to true,
the line spanner stops before the accidentals if its right bound
is a note column or a grob contained in a note column, and this
note column has accidentals.

@item start-at-dot
Only meaningful in @code{bound-details.left}.  If true, the line
spanner starts after dots, in a fashion similar to
@code{end-on-accidental}.

@item adjust-on-neighbor
If true, the @code{left-neighbor} or @code{right-neighbor} object is
read, and if it exists, the line spanner starts after it or stops
before it.

@item stencil
Line spanners may have symbols at the beginning or end, which is
contained in this sub-property.  For internal use.

@item text
This is a markup that is evaluated to yield the stencil.

@item stencil-align-dir-y
@itemx stencil-offset
Without setting one of these, the stencil is simply put at the
end-point, centered on the line, as defined by the @code{X} and
@code{Y} sub-properties.  Setting @code{stencil-align-dir-y} moves the
symbol at the edge vertically relative to the end point of the line.
With @code{stencil-offset}, expecting a number pair, the stencil is
moved along the X@tie{}axis according to the first value, the second
value moves the stencil along the Y@tie{}axis.

@item arrow
Produces an arrowhead at the end-points of the line.

@item padding
Controls the space between the specified end point of the line and the
actual end.  Without padding, a glissando would start and end in the
center of each note head.

@end table
)",

               /* properties */
               R"(
bound-details
extra-dy
gap
left-bound-info
left-neighbor
note-columns
right-bound-info
right-neighbor
thickness
to-barline
               )");

ADD_INTERFACE (Horizontal_line_spanner,
               R"(
This interface is a subset of the @ref{line-spanner-interface}, for
use with line spanners that are always horizontal (such as crescendo
spanners).  The @code{details.Y} subproperty is irrelevant.  Grobs
having this interface can be side-positioned vertically.
)",
               /* properties */
               R"(
               )");
