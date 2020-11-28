/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2020 Jan Nieuwenhuizen <janneke@gnu.org>

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
#include "grob-interface.hh"
#include "item.hh"
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

class Line_spanner
{
public:
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_cross_staff, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_left_bound_info, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_left_bound_info_and_text, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_right_bound_info, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_bound_info, (SCM, Direction));
};

Spanner *parent_spanner (Grob *g)
{
  if (has_interface<Spanner> (g))
    return dynamic_cast<Spanner *> (g);
  return parent_spanner (g->get_y_parent ());
}

SCM
Line_spanner::calc_bound_info (SCM smob, Direction dir)
{
  Spanner *me = unsmob<Spanner> (smob);

  Grob *commonx = me->get_bound (LEFT)->common_refpoint (me->get_bound (RIGHT), X_AXIS);
  commonx = me->common_refpoint (commonx, X_AXIS);

  SCM bound_details = get_property (me, "bound-details");

  SCM details = SCM_BOOL_F;
  if (scm_is_false (details))
    details = ly_assoc_get ((dir == LEFT)
                            ? ly_symbol2scm ("left")
                            : ly_symbol2scm ("right"), bound_details, SCM_BOOL_F);

  if (me->get_bound (dir)->break_status_dir ())
    {
      SCM extra = ly_assoc_get ((dir == LEFT)
                                ? ly_symbol2scm ("left-broken")
                                : ly_symbol2scm ("right-broken"), bound_details, SCM_EOL);

      details = scm_append (scm_list_2 (extra, details));
    }

  if (scm_is_false (details))
    details = ly_assoc_get (ly_symbol2scm ("default"), bound_details, SCM_EOL);

  SCM text = ly_assoc_get (ly_symbol2scm ("text"), details, SCM_BOOL_F);
  if (Text_interface::is_markup (text))
    {
      Output_def *layout = me->layout ();
      SCM properties = Font_interface::text_font_alist_chain (me);
      details = scm_acons (ly_symbol2scm ("stencil"),
                           Text_interface::interpret_markup (layout->self_scm (),
                                                             properties, text),
                           details);
    }

  if (!scm_is_number (ly_assoc_get (ly_symbol2scm ("X"), details, SCM_BOOL_F)))
    {
      Direction attach = from_scm (ly_assoc_get (ly_symbol2scm ("attach-dir"),
                                                 details, SCM_BOOL_F),
                                   CENTER);

      Item *bound_item = me->get_bound (dir);
      Grob *bound_grob = bound_item;
      if (from_scm<bool> (ly_assoc_get (ly_symbol2scm ("end-on-note"), details, SCM_BOOL_F))
          && bound_item->break_status_dir ())
        {
          extract_grob_set (me, "note-columns", columns);
          if (columns.size ())
            bound_grob = (dir == LEFT)
                         ? columns[0] : columns.back ();
        }

      Real x_coord = (has_interface<Paper_column> (bound_grob)
                      ? Axis_group_interface::generic_bound_extent (bound_grob, commonx, X_AXIS)
                      : robust_relative_extent (bound_grob, commonx, X_AXIS)).linear_combination (attach);

      Grob *acc = Note_column::accidentals (bound_grob->get_x_parent ());
      if (acc && from_scm<bool> (ly_assoc_get (ly_symbol2scm ("end-on-accidental"), details, SCM_BOOL_F)))
        x_coord = robust_relative_extent (acc, commonx, X_AXIS).linear_combination (attach);

      Grob *dot = unsmob<Grob> (get_object (bound_grob, "dot"));
      if (dot && from_scm<bool> (ly_assoc_get (ly_symbol2scm ("start-at-dot"), details, SCM_BOOL_F)))
        x_coord = robust_relative_extent (dot, commonx, X_AXIS).linear_combination (attach);

      details = scm_acons (ly_symbol2scm ("X"),
                           to_scm (x_coord),
                           details);
    }

  if (!scm_is_number (ly_assoc_get (ly_symbol2scm ("Y"), details, SCM_BOOL_F)))
    {
      Real y = 0.0;

      Real extra_dy = from_scm<double> (get_property (me, "extra-dy"),
                                        0.0);

      Grob *common_y = me->common_refpoint (me->get_bound (dir), Y_AXIS);
      if (me->get_bound (dir)->break_status_dir ())
        {
          if (from_scm<bool> (get_property (me, "simple-Y")))
            {
              Spanner *orig = me->original ();
              Spanner *extreme = dir == LEFT ? orig->broken_intos_.front () : orig->broken_intos_.back ();
              Grob *e_bound = extreme->get_bound (dir);
              Grob *e_common_y = extreme->common_refpoint (e_bound, Y_AXIS);
              y = e_bound->extent (e_common_y, Y_AXIS).center ();
            }
          else
            {
              Spanner *next_sp = me->broken_neighbor (dir);
              Item *next_bound = next_sp->get_bound (dir);

              if (next_bound->break_status_dir ())
                {
                  programming_error ("no note heads for the line spanner on neighbor line?"
                                     " Confused.");
                  me->suicide ();
                  return SCM_EOL;
                }

              Spanner *next_bound_parent = parent_spanner (next_bound);
              Interval next_ext = next_bound->extent (next_bound_parent, Y_AXIS);

              /*
                We want to know what would be the y-position of the next
                bound (relative to my y-parent) if it belonged to the
                same system as this bound.  We rely on the fact that the
                y-parent of the next bound is a spanner (probably the
                VerticalAxisGroup of a staff) that extends over the break.
              */
              Spanner *next_bound_parent_on_this_line
                = next_bound_parent->broken_neighbor (-dir);

              if (next_bound_parent_on_this_line)
                {
                  Grob *common = me->common_refpoint (next_bound_parent_on_this_line, Y_AXIS);
                  Real bound_offset = next_bound_parent_on_this_line->relative_coordinate (common, Y_AXIS);
                  y = next_ext.center () + bound_offset - me->relative_coordinate (common, Y_AXIS);
                }
              else
                {
                  /*
                    We fall back to assuming that the distance between
                    staves doesn't change over line breaks.
                  */
                  programming_error ("next-bound's parent doesn't extend to this line");
                  Grob *next_system = next_bound->get_system ();
                  Grob *this_system = me->get_system ();
                  y = next_ext.center () + next_bound_parent->relative_coordinate (next_system, Y_AXIS)
                      - me->relative_coordinate (this_system, Y_AXIS);
                }
            }
        }
      else
        {
          Interval ii = me->get_bound (dir)->extent (common_y, Y_AXIS);
          if (!ii.is_empty ())
            y = ii.center ();
          details = scm_acons (ly_symbol2scm ("common-Y"), common_y->self_scm (), details);
        }

      y += dir * extra_dy / 2;
      details = scm_acons (ly_symbol2scm ("Y"), to_scm (y), details);
    }

  return details;
}

MAKE_SCHEME_CALLBACK (Line_spanner, calc_cross_staff, 1);
SCM
Line_spanner::calc_cross_staff (SCM smob)
{
  Spanner *me = unsmob<Spanner> (smob);
  if (!me)
    return SCM_BOOL_F;

  if (from_scm<bool> (get_property (me->get_bound (LEFT), "non-musical"))
      || from_scm<bool> (get_property (me->get_bound (RIGHT), "non-musical")))
    return SCM_BOOL_F;

  return scm_from_bool (Staff_symbol_referencer::get_staff_symbol (me->get_bound (LEFT))
                        != Staff_symbol_referencer::get_staff_symbol (me->get_bound (RIGHT)));
}

MAKE_SCHEME_CALLBACK (Line_spanner, calc_right_bound_info, 1);
SCM
Line_spanner::calc_right_bound_info (SCM smob)
{
  return Line_spanner::calc_bound_info (smob, RIGHT);
}

MAKE_SCHEME_CALLBACK (Line_spanner, calc_left_bound_info, 1);
SCM
Line_spanner::calc_left_bound_info (SCM smob)
{
  return Line_spanner::calc_bound_info (smob, LEFT);
}

MAKE_SCHEME_CALLBACK (Line_spanner, calc_left_bound_info_and_text, 1);
SCM
Line_spanner::calc_left_bound_info_and_text (SCM smob)
{
  SCM alist = Line_spanner::calc_bound_info (smob, LEFT);
  Spanner *me = unsmob<Spanner> (smob);

  SCM text = get_property (me, "text");
  if (Text_interface::is_markup (text)
      && me->get_bound (LEFT)->break_status_dir () == CENTER
      && scm_is_false (ly_assoc_get (ly_symbol2scm ("stencil"), alist, SCM_BOOL_F)))
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

MAKE_SCHEME_CALLBACK (Line_spanner, print, 1);
SCM
Line_spanner::print (SCM smob)
{
  Spanner *me = unsmob<Spanner> (smob);

  // Triggers simple-Y calculations
  bool simple_y = from_scm<bool> (get_property (me, "simple-Y")) && !from_scm<bool> (get_property (me, "cross-staff"));

  Drul_array<SCM> bounds (get_property (me, "left-bound-info"),
                          get_property (me, "right-bound-info"));

  Grob *commonx = me->get_bound (LEFT)->common_refpoint (me->get_bound (RIGHT), X_AXIS);
  commonx = me->common_refpoint (commonx, X_AXIS);

  Drul_array<Offset> span_points;

  for (LEFT_and_RIGHT (d))
    {
      Offset z (from_scm<double> (ly_assoc_get (ly_symbol2scm ("X"),
                                                bounds[d], SCM_BOOL_F), 0.0),
                from_scm<double> (ly_assoc_get (ly_symbol2scm ("Y"),
                                                bounds[d], SCM_BOOL_F), 0.0));

      span_points[d] = z;
    }

  Drul_array<Real> gaps (0, 0);
  Drul_array<bool> arrows (0, 0);
  Drul_array<const Stencil *> stencils (0, 0);
  Drul_array<Grob *> common_y (0, 0);

  // For scaling of 'padding and 'stencil-offset
  Real magstep
    = pow (2, from_scm<double> (get_property (me, "font-size"), 0.0) / 6);

  for (LEFT_and_RIGHT (d))
    {
      gaps[d] = from_scm<double> (ly_assoc_get (ly_symbol2scm ("padding"),
                                                bounds[d], SCM_BOOL_F), 0.0);
      arrows[d] = from_scm<bool> (ly_assoc_get (ly_symbol2scm ("arrow"),
                                                bounds[d], SCM_BOOL_F));
      stencils[d] = unsmob<const Stencil> (ly_assoc_get (ly_symbol2scm ("stencil"),
                                                         bounds[d], SCM_BOOL_F));
      common_y[d] = unsmob<Grob> (ly_assoc_get (ly_symbol2scm ("common-Y"),
                                                bounds[d], SCM_BOOL_F));
      if (!common_y[d])
        common_y[d] = me;
    }

  Grob *my_common_y = common_y[LEFT]->common_refpoint (common_y[RIGHT], Y_AXIS);

  if (!simple_y)
    {
      for (LEFT_and_RIGHT (d))
        span_points[d][Y_AXIS] += common_y[d]->relative_coordinate (my_common_y, Y_AXIS);
    }

  Interval normalized_endpoints = from_scm (get_property (me, "normalized-endpoints"), Interval (0, 1));
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
  for (LEFT_and_RIGHT (d))
    {
      span_points[d] += -d * gaps[d] * magstep * dz.direction ();

      if (stencils[d])
        {
          Stencil s = stencils[d]->translated (span_points[d]);
          SCM align = ly_assoc_get (ly_symbol2scm ("stencil-align-dir-y"),
                                    bounds[d], SCM_BOOL_F);
          SCM off = ly_assoc_get (ly_symbol2scm ("stencil-offset"),
                                  bounds[d], SCM_BOOL_F);

          if (scm_is_number (align))
            s.align_to (Y_AXIS, scm_to_double (align));

          if (is_number_pair (off))
            s.translate (from_scm<Offset> (off) * magstep);

          line.add_stencil (s);
        }
    }

  for (LEFT_and_RIGHT (d))
    {
      if (stencils[d] && ! stencils[d]->is_empty ())
        span_points[d] += dz_dir *
                          (stencils[d]->extent (X_AXIS)[-d] / dz_dir[X_AXIS]);
    }

  Offset adjust = dz.direction () * Staff_symbol_referencer::staff_space (me);
  Offset line_left = span_points[LEFT] + (arrows[LEFT] ? adjust * 1.4 : Offset (0, 0));
  Offset line_right = span_points[RIGHT] - (arrows[RIGHT] ? adjust * 0.55 : Offset (0, 0));

  if (line_right[X_AXIS] > line_left[X_AXIS])
    {
      line.add_stencil (Line_interface::line (me, line_left, line_right));

      line.add_stencil (Line_interface::arrows (me,
                                                span_points[LEFT],
                                                span_points[RIGHT],
                                                arrows[LEFT],
                                                arrows[RIGHT]));
    }

  line.translate (Offset (-me->relative_coordinate (commonx, X_AXIS),
                          simple_y ? 0.0 : -me->relative_coordinate (my_common_y, Y_AXIS)));

  return line.smobbed_copy ();
}

ADD_INTERFACE (Line_spanner,
               "Generic line drawn between two objects, e.g., for use with"
               " glissandi.\n"
               "\n"
               "@code{bound-details} is a nested alist.  It's possible to"
               " specify settings for the sub-properties: @code{left},"
               " @code{left-broken}, @code{right} and @code{right-broken}.\n"
               "\n"
               "Values for the following keys may be set:\n"
               "\n"
               "@table @code\n"
               "@item Y\n"
               "Sets the Y@tie{}coordinate of the end point, in staff-spaces"
               " offset from the staff center line.  By default, it is the"
               " center of the bound object, so a glissando points to the"
               " vertical center of the note head.  For horizontal spanners,"
               " such as text spanners and trill spanners, it is hardcoded"
               " to 0.\n"
               "@item attach-dir\n"
               "Determines where the line starts and ends in the"
               " X@tie{}direction, relative to the bound object."
               " So, a value of -1 (or @code{LEFT}) makes the line start/end"
               " at the left side of the note head it is attached to.\n"
               "@item X\n"
               "This is the absolute X@tie{}coordinate of the end point."
               " Usually computed on the fly.\n"
               "@item stencil\n"
               "Line spanners may have symbols at the beginning or end, which"
               " is contained in this sub-property.  For internal use.\n"
               "@item text\n"
               "This is a markup that is evaluated to yield the stencil.\n"
               "@item stencil-align-dir-y\n"
               "@itemx stencil-offset\n"
               "Without setting one of these, the stencil is simply put at the"
               " end-point, centered on the line, as defined by the @code{X}"
               " and @code{Y} sub-properties.  Setting"
               " @code{stencil-align-dir-y} moves the symbol at the edge"
               " vertically relative to the end point of the line.  With"
               " @code{stencil-offset}, expecting a number pair, the stencil"
               " is moved along the X@tie{}axis according to the first value,"
               " the second value moves the stencil along the Y@tie{}axis.\n"
               "@item arrow\n"
               "Produces an arrowhead at the end-points of the line.\n"
               "@item padding\n"
               "Controls the space between the specified end point of the"
               " line and the actual end.  Without padding, a glissando would"
               " start and end in the center of each note head.\n"
               "@end table\n",

               /* properties */
               "bound-details "
               "extra-dy "
               "gap "
               "left-bound-info "
               "note-columns "
               "right-bound-info "
               "simple-Y "
               "thickness "
               "to-barline "
              );
