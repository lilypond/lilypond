/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2012--2022 Mike Solomon <mike@mikesolomon.org>

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

// Needed because of extension definitions for POSIX functions.
#include "config.hh"

#include "bezier.hh"
#include "box.hh"
#include "dimensions.hh"
#include "font-metric.hh"
#include "freetype.hh"
#include "grob.hh"
#include "international.hh"
#include "interval.hh"
#include "lazy-skyline-pair.hh"
#include "lily-guile.hh"
#include "misc.hh"
#include "modified-font-metric.hh"
#include "offset.hh"
#include "open-type-font.hh"
#include "pango-font.hh"
#include "pointer-group-interface.hh"
#include "real.hh"
#include "rest.hh"
#include "skyline-pair.hh"
#include "skyline.hh"
#include "spanner.hh"
#include "stencil.hh"
#include "string-convert.hh"
#include "transform.hh"

#include <cmath>

using std::string;
using std::vector;

Real QUANTIZATION_UNIT = 0.2;

void add_partial_ellipse_segments (Lazy_skyline_pair *skyline,
                                   Transform const &transform, Offset rad,
                                   Real start, Real end, Real th, bool connect,
                                   bool fill);

void add_draw_bezier_segments (Lazy_skyline_pair *skyline,
                               Transform const &transform, Real,
                               Offset control[4]);

//// UTILITY FUNCTIONS

/*
  from a nested SCM list, return the first list of numbers
  useful for polygons
*/
SCM
get_number_list (SCM l)
{
  if (scm_is_pair (l))
    {
      if (scm_is_number (scm_car (l)))
        return l;
      SCM res = get_number_list (scm_car (l));
      if (scm_is_false (res))
        return get_number_list (scm_cdr (l));
      return res;
    }
  return SCM_BOOL_F;
}

/*
  from a nested SCM list, return the first list that looks like a path
  expression.
*/
SCM
get_path_list (SCM l)
{
  for (; scm_is_pair (l); l = scm_cdr (l))
    {
      SCM head = scm_car (l);
      if (scm_is_symbol (head)
          && (scm_is_eq (head, ly_symbol2scm ("moveto"))
              || scm_is_eq (head, ly_symbol2scm ("rmoveto"))
              || scm_is_eq (head, ly_symbol2scm ("lineto"))
              || scm_is_eq (head, ly_symbol2scm ("rlineto"))
              || scm_is_eq (head, ly_symbol2scm ("curveto"))
              || scm_is_eq (head, ly_symbol2scm ("rcurveto"))
              || scm_is_eq (head, ly_symbol2scm ("closepath"))))
        {
          return l;
        }
      SCM res = get_path_list (head);
      if (scm_is_true (res))
        return res;
    }
  return SCM_BOOL_F;
}

//// END UTILITY FUNCTIONS

void
add_draw_line_segments (Lazy_skyline_pair *skyline, Transform const &transform,
                        SCM expr)
{
  Real thick = from_scm<double> (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real x0 = from_scm<double> (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real y0 = from_scm<double> (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real x1 = from_scm<double> (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real y1 = from_scm<double> (scm_car (expr), 0.0);

  Offset left (x0, y0);
  Offset right (x1, y1);

  skyline->add_segment (transform, left, right, thick);
}

void
add_partial_ellipse_segments_scm (Lazy_skyline_pair *skyline,
                                  Transform const &transform, SCM expr)
{
  Real x_rad = from_scm<double> (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real y_rad = from_scm<double> (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Offset rad (x_rad, y_rad);
  Real start = from_scm<double> (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real end = from_scm<double> (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real th = from_scm<double> (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  bool connect = from_scm<bool> (scm_car (expr));
  expr = scm_cdr (expr);
  bool fill = from_scm<bool> (scm_car (expr));
  add_partial_ellipse_segments (skyline, transform, rad, start, end, th,
                                connect, fill);
}

void
add_partial_ellipse_segments (Lazy_skyline_pair *skyline,
                              Transform const &transform, Offset rad,
                              Real start, Real end, Real th, bool connect,
                              bool fill)
{
  if (end == start)
    end += 360;

  Real x_scale = sqrt (sqr (transform.get_xx ()) + sqr (transform.get_yx ()));
  Real y_scale = sqrt (sqr (transform.get_xy ()) + sqr (transform.get_yy ()));

  const auto quantization = static_cast<vsize> (
    std::max (1.0, ((rad[X_AXIS] * x_scale) + (rad[Y_AXIS] * y_scale)) * M_PI
                     / QUANTIZATION_UNIT));

  Offset last;
  Offset first;
  for (vsize i = 0; i <= quantization; i++)
    {
      Real ang = linear_interpolate (
        static_cast<Real> (i), 0, static_cast<Real> (quantization), start, end);
      Offset pt (offset_directed (ang).scale (rad));
      if (i > 0)
        skyline->add_segment (transform, last, pt, th);
      else
        first = pt;
      last = pt;
    }

  if (connect || fill)
    {
      skyline->add_segment (transform, first, last, th);
    }
}

void
add_round_filled_box_segments (Lazy_skyline_pair *skyline,
                               Transform const &transform, SCM expr)
{
  Real left = from_scm<double> (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real right = from_scm<double> (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real bottom = from_scm<double> (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real top = from_scm<double> (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real diameter = from_scm<double> (scm_car (expr), 0.0);

  Interval x (-left, right);
  Interval y (-bottom, top);
  if (x.is_empty () || y.is_empty ())
    {
      return;
    }

  Real x_scale = sqrt (sqr (transform.get_xx ()) + sqr (transform.get_yx ()));
  Real y_scale = sqrt (sqr (transform.get_xy ()) + sqr (transform.get_yy ()));
  bool rounded = (diameter * std::max (x_scale, y_scale) > 0.5);
  bool rotated = (transform.get_yx () || transform.get_xy ());

  vector<Box> boxes;
  if (!rotated && !rounded)
    {
      /* simple box */
      Box b (Interval (-left, right), Interval (-bottom, top));
      skyline->add_box (transform, b);
    }
  else
    {
      const auto quantization = static_cast<vsize> (
        std::max (0.0, rounded * diameter * (x_scale + y_scale) * M_PI
                         / QUANTIZATION_UNIT / 8));
      /* if there is no quantization, there is no need to draw
         rounded corners. >>> Set the effective skyline radius to 0 */
      Real radius = (quantization ? diameter / 2 : 0.);

      /* draw straight lines */
      Offset points[] = {
        Offset (-left, -bottom + radius), Offset (-left, top - radius),
        Offset (-left + radius, top),     Offset (right - radius, top),
        Offset (right, top - radius),     Offset (right, -bottom + radius),
        Offset (right - radius, -bottom), Offset (-left + radius, -bottom),
      };
      for (vsize i = 0; i < sizeof (points) / sizeof (Offset); i += 2)
        {
          skyline->add_contour_segment (transform, Orientation::CW, points[i],
                                        points[i + 1]);
        }

      /* draw rounded corners */
      if (radius)
        {
          Offset rad (radius, radius);
          Drul_array<Real> cx;
          Drul_array<Real> cy;

          cx[LEFT] = -left + radius;
          cx[RIGHT] = right - radius;
          cy[DOWN] = -bottom + radius;
          cy[UP] = top - radius;

          for (const auto v : {DOWN, UP})
            for (const auto h : {LEFT, RIGHT})
              {
                Offset last;
                for (vsize i = 0; i <= quantization; i++)
                  {
                    Real ang = linear_interpolate (
                      static_cast<Real> (i), 0,
                      static_cast<Real> (quantization), 0., 90.);
                    Offset pt (offset_directed (ang).scale (rad));
                    Offset inter (cx[h] + h * pt[X_AXIS],
                                  cy[v] + v * pt[Y_AXIS]);

                    if (i > 0)
                      {
                        skyline->add_segment (transform, last, inter);
                      }
                    last = inter;
                  }
              }
        }
    }
}

void
add_draw_bezier_segments_scm (Lazy_skyline_pair *skyline,
                              Transform const &transform, SCM expr)
{
  Real th = from_scm<double> (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real x0 = from_scm<double> (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real y0 = from_scm<double> (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real x1 = from_scm<double> (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real y1 = from_scm<double> (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real x2 = from_scm<double> (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real y2 = from_scm<double> (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real x3 = from_scm<double> (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real y3 = from_scm<double> (scm_car (expr), 0.0);

  Offset ps[] = {
    Offset (x0, y0),
    Offset (x1, y1),
    Offset (x2, y2),
    Offset (x3, y3),
  };
  add_draw_bezier_segments (skyline, transform, th, ps);
}

void
add_draw_bezier_segments (Lazy_skyline_pair *skyline,
                          Transform const &transform, Real th,
                          Offset control[4])
{
  Bezier curve;

  Real len = 0.0;
  Offset last;
  for (int i = 0; i < 4; i++)
    {
      curve.control_[i] = control[i];
      Offset transformed = transform (control[i]);
      if (i > 0)
        {
          len += (transformed - last).length ();
        }
      last = transformed;
    }

  //////////////////////
  const auto quantization
    = static_cast<vsize> (std::max (0.0, len / QUANTIZATION_UNIT));

  vector<Offset> points;
  points.reserve (quantization + 1);

  last = curve.control_[0];
  for (vsize i = 1; i < quantization; i++)
    {
      Real t = static_cast<Real> (i) / static_cast<Real> (quantization);
      Offset pt = curve.curve_point (t);
      skyline->add_segment (transform, last, pt, th);
      last = pt;
    }

  skyline->add_segment (transform, last, curve.control_[3], th);
}

/*
  converts a path into lists of 4 (line) or 8 (curve) absolute coordinates
  for example:
  '(moveto 1 2 lineto 3 4 rlineto -1 -1 curveto
    3 3 5 5 6 6 rcurveto -1 -1 -1 -1 -1 -1 closepath)
  becomes
  '((1 2 3 4)
    (3 4 2 3)
    (2 3 3 3 5 5 6 6)
    (6 6 5 5 4 4 3 3)
    (3 3 1 2))
*/

SCM
all_commands_to_absolute_and_group (SCM expr)
{
  SCM out = SCM_EOL;
  Offset start (0, 0);
  Offset current (0, 0);
  bool first = true;
  while (scm_is_pair (expr))
    {
      if (scm_is_eq (scm_car (expr), ly_symbol2scm ("moveto"))
          || (scm_is_eq (scm_car (expr), ly_symbol2scm ("rmoveto")) && first))
        {
          Real x = from_scm<double> (scm_cadr (expr), 0.0);
          Real y = from_scm<double> (scm_caddr (expr), 0.0);
          start = Offset (x, y);
          current = start;
          expr = scm_cdddr (expr);
        }
      if (scm_is_eq (scm_car (expr), ly_symbol2scm ("rmoveto")))
        {
          Real x = from_scm<double> (scm_cadr (expr), 0.0);
          Real y = from_scm<double> (scm_caddr (expr), 0.0);
          start = (Offset (x, y) + current);
          current = start;
          expr = scm_cdddr (expr);
        }
      else if (scm_is_eq (scm_car (expr), ly_symbol2scm ("lineto")))
        {
          Real x = from_scm<double> (scm_cadr (expr), 0.0);
          Real y = from_scm<double> (scm_caddr (expr), 0.0);
          out = scm_cons (ly_list (to_scm (current[X_AXIS]),
                                   to_scm (current[Y_AXIS]), to_scm (x),
                                   to_scm (y)),
                          out);
          current = Offset (x, y);
          expr = scm_cdddr (expr);
        }
      else if (scm_is_eq (scm_car (expr), ly_symbol2scm ("rlineto")))
        {
          Real x = from_scm<double> (scm_cadr (expr), 0.0);
          Real y = from_scm<double> (scm_caddr (expr), 0.0);
          out = scm_cons (ly_list (to_scm (current[X_AXIS]),
                                   to_scm (current[Y_AXIS]),
                                   to_scm (x + current[X_AXIS]),
                                   to_scm (y + current[Y_AXIS])),
                          out);
          current = (Offset (x, y) + current);
          expr = scm_cdddr (expr);
        }
      else if (scm_is_eq (scm_car (expr), ly_symbol2scm ("curveto")))
        {
          Real x1 = from_scm<double> (scm_cadr (expr), 0.0);
          expr = scm_cddr (expr);
          Real y1 = from_scm<double> (scm_car (expr), 0.0);
          expr = scm_cdr (expr);
          Real x2 = from_scm<double> (scm_car (expr), 0.0);
          expr = scm_cdr (expr);
          Real y2 = from_scm<double> (scm_car (expr), 0.0);
          expr = scm_cdr (expr);
          Real x3 = from_scm<double> (scm_car (expr), 0.0);
          expr = scm_cdr (expr);
          Real y3 = from_scm<double> (scm_car (expr), 0.0);
          expr = scm_cdr (expr);
          out = scm_cons (ly_list (to_scm (current[X_AXIS]),
                                   to_scm (current[Y_AXIS]), to_scm (x1),
                                   to_scm (y1), to_scm (x2), to_scm (y2),
                                   to_scm (x3), to_scm (y3)),
                          out);
          current = Offset (x3, y3);
        }
      else if (scm_is_eq (scm_car (expr), ly_symbol2scm ("rcurveto")))
        {
          Real x1 = from_scm<double> (scm_cadr (expr), 0.0);
          expr = scm_cddr (expr);
          Real y1 = from_scm<double> (scm_car (expr), 0.0);
          expr = scm_cdr (expr);
          Real x2 = from_scm<double> (scm_car (expr), 0.0);
          expr = scm_cdr (expr);
          Real y2 = from_scm<double> (scm_car (expr), 0.0);
          expr = scm_cdr (expr);
          Real x3 = from_scm<double> (scm_car (expr), 0.0);
          expr = scm_cdr (expr);
          Real y3 = from_scm<double> (scm_car (expr), 0.0);
          expr = scm_cdr (expr);
          out = scm_cons (
            ly_list (
              to_scm (current[X_AXIS]), to_scm (current[Y_AXIS]),
              to_scm (x1 + current[X_AXIS]), to_scm (y1 + current[Y_AXIS]),
              to_scm (x2 + current[X_AXIS]), to_scm (y2 + current[Y_AXIS]),
              to_scm (x3 + current[X_AXIS]), to_scm (y3 + current[Y_AXIS])),
            out);
          current = (Offset (x3, y3) + current);
        }
      else if (scm_is_eq (scm_car (expr), ly_symbol2scm ("closepath")))
        {
          if ((current[X_AXIS] != start[X_AXIS])
              || (current[Y_AXIS] != start[Y_AXIS]))
            {
              out = scm_cons (
                ly_list (to_scm (current[X_AXIS]), to_scm (current[Y_AXIS]),
                         to_scm (start[X_AXIS]), to_scm (start[Y_AXIS])),
                out);
              current = start;
            }
          expr = scm_cdr (expr);
        }
      else
        {
          warning (_ ("Malformed path for path stencil."));
          return out;
        }
      first = false;
    }
  return scm_reverse_x (out, SCM_EOL);
}

void
internal_add_path_segments (Lazy_skyline_pair *skyline,
                            Transform const &transform, SCM expr)
{
  SCM blot = scm_car (expr);
  expr = scm_cdr (expr);
  SCM path = all_commands_to_absolute_and_group (expr);
  // note that expr has more stuff that we don't need after this - simply ignore it
  //////////////////////
  for (SCM s = path; scm_is_pair (s); s = scm_cdr (s))
    {
      from_scm<int> (scm_length (scm_car (s))) == 4
        ? add_draw_line_segments (skyline, transform,
                                  scm_cons (blot, scm_car (s)))
        : add_draw_bezier_segments_scm (skyline, transform,
                                        scm_cons (blot, scm_car (s)));
    }
}

void
add_path_segments (Lazy_skyline_pair *skyline, Transform const &transform,
                   SCM expr)
{
  return internal_add_path_segments (
    skyline, transform,
    scm_cons (scm_car (expr), get_path_list (scm_cdr (expr))));
}

void
add_polygon_segments (Lazy_skyline_pair *skyline, Transform const &transform,
                      SCM expr)
{
  SCM coords = get_number_list (scm_car (expr));
  expr = scm_cdr (expr);
  SCM blot_diameter = scm_car (expr);

  bool first = true;
  SCM l = SCM_EOL;
  for (SCM s = coords; scm_is_pair (s); s = scm_cddr (s))
    {
      l = scm_cons (first ? ly_symbol2scm ("moveto") : ly_symbol2scm ("lineto"),
                    l);
      l = scm_cons (scm_car (s), l);
      l = scm_cons (scm_cadr (s), l);
      first = false;
    }
  l = scm_cons (ly_symbol2scm ("closepath"), l);
  internal_add_path_segments (
    skyline, transform, scm_cons (blot_diameter, scm_reverse_x (l, SCM_EOL)));
}

void
add_named_glyph_segments (Lazy_skyline_pair *skyline,
                          Transform const &transform, SCM expr)
{
  SCM fm_scm = scm_car (expr);
  Font_metric *fm = unsmob<Font_metric> (fm_scm);
  expr = scm_cdr (expr);
  SCM glyph = scm_car (expr);
  string glyph_s = ly_scm2string (glyph);

  Open_type_font *open_fm = dynamic_cast<Open_type_font *> (
    dynamic_cast<Modified_font_metric *> (fm)->original_font ());
  SCM_ASSERT_TYPE (open_fm, fm_scm, SCM_ARG1, __FUNCTION__, "OpenType font");

  size_t gidx = open_fm->name_to_index (glyph_s);
  // Bbox is the best approximation of the width based on how it would be
  // calculated in open-type-font.cc if it were based on real extents
  Box bbox = open_fm->get_unscaled_indexed_char_dimensions (gidx);
  bbox.scale (dynamic_cast<Modified_font_metric *> (fm)->get_magnification ()
              * open_fm->design_size () / open_fm->get_units_per_EM ());
  // Real bbox is the real bbox of the object
  Box real_bbox = open_fm->get_glyph_outline_bbox (gidx);

  Real scale = bbox[X_AXIS].length () / real_bbox[X_AXIS].length ();

  Transform local = transform;
  local.scale (scale, scale);

  open_fm->add_outline_to_skyline (skyline, local, gidx);
}

void
add_glyph_string_segments (Lazy_skyline_pair *skyline,
                           Transform const &transform, SCM expr)
{
  SCM fm_scm = scm_car (expr);
  Font_metric *fm = unsmob<Font_metric> (fm_scm);
  expr = scm_cdr (expr);
  expr = scm_cdr (expr); // font-name
  expr = scm_cdr (expr); // size
  expr = scm_cdr (expr); // cid?
  SCM whxy = scm_car (expr);
  vector<Real> widths;
  vector<Interval> heights;
  vector<Real> xos;
  vector<Real> yos;
  vector<string> char_ids;

  Pango_font *pango_fm = dynamic_cast<Pango_font *> (fm);
  SCM_ASSERT_TYPE (pango_fm, fm_scm, SCM_ARG1, __FUNCTION__, "Pango font");

  for (SCM s = whxy; scm_is_pair (s); s = scm_cdr (s))
    {
      SCM now = scm_car (s);
      widths.push_back (from_scm<double> (scm_car (now), 0.0));
      now = scm_cdr (now);
      heights.push_back (from_scm (scm_car (now), Interval (0, 0)));
      now = scm_cdr (now);
      xos.push_back (from_scm<double> (scm_car (now), 0.0));
      now = scm_cdr (now);
      yos.push_back (from_scm<double> (scm_car (now), 0.0));
      now = scm_cdr (now);
      char_ids.push_back (robust_scm2string (scm_car (now), ""));
    }
  Real cumulative_x = 0.0;
  for (vsize i = 0; i < widths.size (); i++)
    {
      Offset pt0 (cumulative_x + xos[i], heights[i][DOWN] + yos[i]);
      Offset pt1 (cumulative_x + widths[i] + xos[i], heights[i][UP] + yos[i]);
      cumulative_x += widths[i];

      Box kerned_bbox;
      kerned_bbox.add_point (pt0);
      kerned_bbox.add_point (pt1);
      size_t gidx = pango_fm->name_to_index (char_ids[i]);
      Box real_bbox = pango_fm->get_scaled_indexed_char_dimensions (gidx);
      Box bbox = pango_fm->get_unscaled_indexed_char_dimensions (gidx);

      // scales may have rounding error but should be close
      Real xlen = real_bbox[X_AXIS].length () / bbox[X_AXIS].length ();
      Real ylen = real_bbox[Y_AXIS].length () / bbox[Y_AXIS].length ();

      /*
        TODO:

        The value will be nan for whitespace, in which case we just want
        filler, so the kerned bbox is ok.

        However, if the value is inf, this likely means that LilyPond is
        using a font that is currently difficult to get the measurements
        from the Pango_font.  This should eventually be fixed.  The solution
        for now is just to use the bounding box.
      */
      if (std::isnan (xlen) || std::isnan (ylen) || std::isinf (xlen)
          || std::isinf (ylen))
        {
          skyline->add_box (transform, kerned_bbox);
        }
      else
        {
          assert (abs (xlen - ylen) < 10e-3);

          Real scale_factor = std::max (xlen, ylen);
          // the three operations below move the stencil from its original coordinates to current coordinates
          // FIXME: this looks extremely fishy.
          Transform transcopy = transform;
          transcopy
            .translate (
              Offset (kerned_bbox[X_AXIS][LEFT],
                      kerned_bbox[Y_AXIS][DOWN] - real_bbox[Y_AXIS][DOWN]))
            .translate (
              Offset (real_bbox[X_AXIS][LEFT], real_bbox[Y_AXIS][DOWN]))
            .scale (scale_factor, scale_factor)
            .translate (-Offset (bbox[X_AXIS][LEFT], bbox[Y_AXIS][DOWN]));
          pango_fm->add_outline_to_skyline (skyline, transcopy, gidx);
        }
    }
}

/*
  receives a stencil expression and a transform matrix
  depending on the stencil name, dispatches it to the appropriate function
*/
static void
interpret_stencil_for_skyline (Lazy_skyline_pair *skyline,
                               Transform const &transform, SCM expr)
{
  if (scm_is_null (expr)
      || (scm_is_string (expr) && scm_is_true (scm_string_null_p (expr))))
    return;

  SCM head = scm_car (expr);
  if (scm_is_eq (head, ly_symbol2scm ("combine-stencil")))
    {
      for (SCM s = scm_cdr (expr); scm_is_pair (s); s = scm_cdr (s))
        interpret_stencil_for_skyline (skyline, transform, scm_car (s));
    }
  else if (scm_is_eq (head, ly_symbol2scm ("translate-stencil")))
    {
      Offset p = from_scm (scm_cadr (expr), Offset (0.0, 0.0));
      Transform local = transform;
      local.translate (p);
      interpret_stencil_for_skyline (skyline, local, scm_caddr (expr));
    }
  else if (scm_is_eq (head, ly_symbol2scm ("scale-stencil")))
    {
      Real x = from_scm<double> (scm_caadr (expr), 0.0);
      Real y = from_scm<double> (scm_cadadr (expr), 0.0);
      Transform local = transform;
      local.scale (x, y);
      interpret_stencil_for_skyline (skyline, local, scm_caddr (expr));
    }
  else if (scm_is_eq (head, ly_symbol2scm ("rotate-stencil")))
    {
      Real ang = from_scm<double> (scm_caadr (expr), 0.0);
      Offset center = from_scm (scm_cadadr (expr), Offset (0.0, 0.0));
      Transform local = transform;
      local.rotate (ang, center);
      interpret_stencil_for_skyline (skyline, local, scm_caddr (expr));
    }
  else if (scm_is_eq (head, ly_symbol2scm ("grob-cause")))
    interpret_stencil_for_skyline (skyline, transform, scm_caddr (expr));
  else if (scm_is_eq (head, ly_symbol2scm ("color")))
    interpret_stencil_for_skyline (skyline, transform, scm_caddr (expr));
  else if (scm_is_eq (head, ly_symbol2scm ("output-attributes")))
    interpret_stencil_for_skyline (skyline, transform, scm_caddr (expr));
  else if (scm_is_eq (head, ly_symbol2scm ("utf-8-string")))
    {
      SCM orig = scm_car (scm_cdddr (expr));
      interpret_stencil_for_skyline (skyline, transform, orig);
    }
  else if (scm_is_eq (head, ly_symbol2scm ("with-outline")))
    interpret_stencil_for_skyline (skyline, transform, scm_cadr (expr));
  else if (scm_is_eq (head, ly_symbol2scm ("draw-line")))
    add_draw_line_segments (skyline, transform, scm_cdr (expr));
  else if (scm_is_eq (head, ly_symbol2scm ("dashed-line")))
    {
      expr = scm_cdr (expr);
      SCM th = scm_car (expr);
      expr = scm_cdr (expr);
      expr = scm_cdr (expr); // on
      expr = scm_cdr (expr); // off
      SCM x1 = scm_car (expr);
      expr = scm_cdr (expr);
      SCM x2 = scm_car (expr);

      skyline->add_segment (
        transform, Offset (0.0, 0.0),
        Offset (from_scm<double> (x1, 0.0), from_scm<double> (x2, 0.0)),
        from_scm<double> (th, 0.0));
    }
  else if (scm_is_eq (head, ly_symbol2scm ("circle")))
    {
      expr = scm_cdr (expr);
      Real rad = from_scm<double> (scm_car (expr), 0);
      expr = scm_cdr (expr);
      Real th = from_scm<double> (scm_car (expr), 0);
      add_partial_ellipse_segments (skyline, transform, Offset (rad, rad), 0.0,
                                    360.0, th, false, true);
    }
  else if (scm_is_eq (head, ly_symbol2scm ("ellipse")))
    {
      expr = scm_cdr (expr);
      Real x_rad = from_scm<double> (scm_car (expr), 0);

      expr = scm_cdr (expr);
      Real y_rad = from_scm<double> (scm_car (expr), 0);

      expr = scm_cdr (expr);
      Real th = from_scm<double> (scm_car (expr), 0);
      add_partial_ellipse_segments (skyline, transform, Offset (x_rad, y_rad),
                                    0, 360, th, false, true);
    }
  else if (scm_is_eq (head, ly_symbol2scm ("partial-ellipse")))
    add_partial_ellipse_segments_scm (skyline, transform, scm_cdr (expr));
  else if (scm_is_eq (head, ly_symbol2scm ("round-filled-box")))
    add_round_filled_box_segments (skyline, transform, scm_cdr (expr));
  else if (scm_is_eq (head, ly_symbol2scm ("named-glyph")))
    add_named_glyph_segments (skyline, transform, scm_cdr (expr));
  else if (scm_is_eq (head, ly_symbol2scm ("polygon")))
    add_polygon_segments (skyline, transform, scm_cdr (expr));
  else if (scm_is_eq (head, ly_symbol2scm ("path")))
    add_path_segments (skyline, transform, scm_cdr (expr));
  else if (scm_is_eq (head, ly_symbol2scm ("glyph-string")))
    add_glyph_string_segments (skyline, transform, scm_cdr (expr));
  else
    {
      /*
        We don't issue a warning here, as we assume that stencil-expression.cc
        is doing stencil-checking correctly.
      */
    }
}

SCM
Grob::maybe_pure_internal_simple_skylines_from_extents (Grob *me, Axis a,
                                                        bool pure, int beg,
                                                        int end, bool ignore_x,
                                                        bool ignore_y)
{
  vector<Box> boxes;
  // we don't know how far spanners stretch along the X axis before
  // line breaking. better have them take up the whole thing
  Interval xex
    = ignore_x ? Interval (-infinity_f, infinity_f) : me->extent (me, X_AXIS);

  // If we're looking at the x exent of a cross staff grob, it could be
  // very early on in the computation process.  We won't know its height
  // until way later, so we give a brute force approximation.
  Interval yex = ignore_y ? Interval (-infinity_f, infinity_f)
                          : me->maybe_pure_extent (me, Y_AXIS, pure, beg, end);

  if (xex.is_empty () || yex.is_empty ())
    return to_scm (Skyline_pair ());

  boxes.push_back (Box (xex, yex));
  return to_scm (Skyline_pair (boxes, a));
}

MAKE_SCHEME_CALLBACK (Grob, pure_simple_vertical_skylines_from_extents,
                      "ly:grob::pure-simple-vertical-skylines-from-extents", 3);
SCM
Grob::pure_simple_vertical_skylines_from_extents (SCM smob, SCM begscm,
                                                  SCM endscm)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  int beg = from_scm (begscm, 0);
  int end = from_scm (endscm, INT_MAX);
  // We cannot measure the widths before line breaking,
  // so we assume that the width is infinite: pass ignore_x=true
  return maybe_pure_internal_simple_skylines_from_extents (
    me, X_AXIS, true, beg, end, true, false);
}

MAKE_SCHEME_CALLBACK (Grob, simple_vertical_skylines_from_extents,
                      "ly:grob::simple-vertical-skylines-from-extents", 1);
SCM
Grob::simple_vertical_skylines_from_extents (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  return maybe_pure_internal_simple_skylines_from_extents (me, X_AXIS, false, 0,
                                                           0, false, false);
}

MAKE_SCHEME_CALLBACK (Grob, pure_simple_horizontal_skylines_from_extents,
                      "ly:grob::pure-simple-horizontal-skylines-from-extents",
                      3);
SCM
Grob::pure_simple_horizontal_skylines_from_extents (SCM smob, SCM begscm,
                                                    SCM endscm)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  int beg = from_scm (begscm, 0);
  int end = from_scm (endscm, INT_MAX);
  // If the grob is cross staff, we cannot measure its Y-extent before
  // wayyyy downstream (after spacing of axis groups is done).
  // Thus, we assume that the Y extent is infinite for cross staff grobs.
  return maybe_pure_internal_simple_skylines_from_extents (
    me, Y_AXIS, true, beg, end, false,
    from_scm<bool> (get_property (me, "cross-staff")));
}

MAKE_SCHEME_CALLBACK (Grob, simple_horizontal_skylines_from_extents,
                      "ly:grob::simple-horizontal-skylines-from-extents", 1);
SCM
Grob::simple_horizontal_skylines_from_extents (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  // See comment in function above.
  return maybe_pure_internal_simple_skylines_from_extents (
    me, Y_AXIS, false, 0, 0, false,
    from_scm<bool> (get_property (me, "cross-staff")));
}

Skyline_pair
skylines_from_stencil (SCM sten, SCM rot, Axis a)
{
  auto *s = unsmob<const Stencil> (sten);

  Lazy_skyline_pair lazy (a);
  if (!s)
    return lazy.to_pair ();

  Stencil maybe_rotated (*s);
  if (scm_is_pair (rot))
    {
      Real angle = from_scm<double> (scm_car (rot), 0.0);
      // `rot` is the Grob 'rotation property, so these are relative to
      // the Stencil extent.
      Real x = from_scm<double> (scm_cadr (rot), 0.0);
      Real y = from_scm<double> (scm_caddr (rot), 0.0);

      maybe_rotated.rotate_degrees (angle, Offset (x, y));
    }

  interpret_stencil_for_skyline (&lazy, Transform::identity,
                                 maybe_rotated.expr ());
  if (lazy.empty () && !s->is_empty (X_AXIS) && !s->is_empty (Y_AXIS))
    {
      // Fallback, if we can't decide extents based on the
      // expression. This happens with embedded-ps, for example.
      lazy.add_box (Transform::identity, s->extent_box ());
    }
  return lazy.to_pair ();
}

MAKE_SCHEME_CALLBACK (Grob, vertical_skylines_from_stencil,
                      "ly:grob::vertical-skylines-from-stencil", 1);
SCM
Grob::vertical_skylines_from_stencil (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  Skyline_pair p (skylines_from_stencil (
    get_property (me, "stencil"), get_property (me, "rotation"), X_AXIS));
  p.pad (
    from_scm<double> (get_property (me, "skyline-horizontal-padding"), 0.0));
  return to_scm (p);
}

MAKE_SCHEME_CALLBACK (Grob, horizontal_skylines_from_stencil,
                      "ly:grob::horizontal-skylines-from-stencil", 1);
SCM
Grob::horizontal_skylines_from_stencil (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  Skyline_pair p = skylines_from_stencil (
    get_property (me, "stencil"), get_property (me, "rotation"), Y_AXIS);
  p.pad (from_scm<double> (get_property (me, "skyline-vertical-padding"), 0.0));
  return to_scm (p);
}

SCM
Grob::internal_skylines_from_element_stencils (Grob *me, Axis a, bool pure,
                                               int beg, int end)
{
  extract_grob_set (me, "elements", elts);
  vector<Real> x_pos;
  vector<Real> y_pos;
  Grob *x_common = common_refpoint_of_array (elts, me, X_AXIS);
  Grob *y_common = common_refpoint_of_array (elts, me, Y_AXIS);
  for (vsize i = 0; i < elts.size (); i++)
    {
      x_pos.push_back (elts[i]->relative_coordinate (x_common, X_AXIS));
      y_pos.push_back (
        elts[i]->maybe_pure_coordinate (y_common, Y_AXIS, pure, beg, end));
    }
  Real my_x = me->relative_coordinate (x_common, X_AXIS);
  Real my_y = me->maybe_pure_coordinate (y_common, Y_AXIS, pure, beg, end);

  Skyline_pair res;
  for (vsize i = 0; i < elts.size (); i++)
    {
      SCM sym = ((a == X_AXIS) ? ly_symbol2scm ("vertical-skylines")
                               : ly_symbol2scm ("horizontal-skylines"));
      SCM skyp_scm = get_maybe_pure_property (elts[i], sym, pure, beg, end);
      if (is_scm<Skyline_pair> (skyp_scm))
        {
          Skyline_pair skyp = from_scm<Skyline_pair> (skyp_scm);
          /*
            It took Mike about 6 months of his life to flip the
            coordinates below.  This is what was causing the problems
            in the shifting with all of the tests. RIP 6 months!
          */
          Offset off (x_pos[i] - my_x, y_pos[i] - my_y);
          skyp.shift (off[a]);
          skyp.raise (off[other_axis (a)]);
          res.merge (skyp);
        }
    }
  return to_scm (res);
}

MAKE_SCHEME_CALLBACK (Grob, vertical_skylines_from_element_stencils,
                      "ly:grob::vertical-skylines-from-element-stencils", 1);
SCM
Grob::vertical_skylines_from_element_stencils (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  return internal_skylines_from_element_stencils (me, X_AXIS, false, 0,
                                                  INT_MAX);
}

MAKE_SCHEME_CALLBACK (Grob, horizontal_skylines_from_element_stencils,
                      "ly:grob::horizontal-skylines-from-element-stencils", 1);
SCM
Grob::horizontal_skylines_from_element_stencils (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  return internal_skylines_from_element_stencils (me, Y_AXIS, false, 0,
                                                  INT_MAX);
}

MAKE_SCHEME_CALLBACK (Grob, pure_vertical_skylines_from_element_stencils,
                      "ly:grob::pure-vertical-skylines-from-element-stencils",
                      3);
SCM
Grob::pure_vertical_skylines_from_element_stencils (SCM smob, SCM beg_scm,
                                                    SCM end_scm)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  int beg = from_scm (beg_scm, 0);
  int end = from_scm (end_scm, 0);
  return internal_skylines_from_element_stencils (me, X_AXIS, true, beg, end);
}

MAKE_SCHEME_CALLBACK (Grob, pure_horizontal_skylines_from_element_stencils,
                      "ly:grob::pure-horizontal-skylines-from-element-stencils",
                      3);
SCM
Grob::pure_horizontal_skylines_from_element_stencils (SCM smob, SCM beg_scm,
                                                      SCM end_scm)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  int beg = from_scm (beg_scm, 0);
  int end = from_scm (end_scm, 0);
  return internal_skylines_from_element_stencils (me, Y_AXIS, true, beg, end);
}
