/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2012--2020 Mike Solomon <mike@mikesolomon.org>

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

void make_partial_ellipse_boxes (Lazy_skyline_pair *skyline,
                                 Transform const &transform, Offset rad,
                                 Real start, Real end, Real th, bool connect,
                                 bool fill);

void make_draw_bezier_boxes (Lazy_skyline_pair *skyline,
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
make_draw_line_boxes (Lazy_skyline_pair *skyline, Transform const &transform,
                      SCM expr)
{
  Real thick = robust_scm2double (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real x0 = robust_scm2double (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real y0 = robust_scm2double (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real x1 = robust_scm2double (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real y1 = robust_scm2double (scm_car (expr), 0.0);

  Offset left (x0, y0);
  Offset right (x1, y1);

  skyline->add_segment (transform, left, right, thick);
}

void
make_partial_ellipse_boxes_scm (Lazy_skyline_pair *skyline,
                                Transform const &transform, SCM expr)
{
  Real x_rad = robust_scm2double (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real y_rad = robust_scm2double (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Offset rad (x_rad, y_rad);
  Real start = robust_scm2double (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real end = robust_scm2double (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real th = robust_scm2double (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  bool connect = to_boolean (scm_car (expr));
  expr = scm_cdr (expr);
  bool fill = to_boolean (scm_car (expr));
  make_partial_ellipse_boxes (skyline, transform, rad, start, end, th, connect,
                              fill);
}

void
make_partial_ellipse_boxes (Lazy_skyline_pair *skyline,
                            Transform const &transform, Offset rad, Real start,
                            Real end, Real th, bool connect, bool fill)
{
  if (end == start)
    end += 360;
  Real x_scale = sqrt (sqr (transform.get_xx ()) + sqr (transform.get_yx ()));
  Real y_scale = sqrt (sqr (transform.get_xy ()) + sqr (transform.get_yy ()));

  vector<Offset> points;
  int quantization
    = std::max (1, (int) (((rad[X_AXIS] * x_scale) + (rad[Y_AXIS] * y_scale))
                          * M_PI / QUANTIZATION_UNIT));

  Offset last;
  Offset first;
  for (vsize i = 0; i <= (vsize) quantization; i++)
    {
      Real ang = linear_interpolate (static_cast<Real> (i), 0, quantization,
                                     start, end);
      Offset pt (offset_directed (ang).scale (rad));
      if (i > 0)
        skyline->add_segment (transform, last, pt, th);
      else
        first = pt;
      points.push_back (pt);
      last = pt;
    }

  if (connect || fill)
    {
      skyline->add_segment (transform, first, last, th);
    }
}

void
make_round_filled_box_boxes (Lazy_skyline_pair *skyline,
                             Transform const &transform, SCM expr)
{
  Real left = robust_scm2double (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real right = robust_scm2double (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real bottom = robust_scm2double (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real top = robust_scm2double (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real diameter = robust_scm2double (scm_car (expr), 0.0);

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
      int quantization = (int) (rounded * diameter * (x_scale + y_scale)
                                * M_PI / QUANTIZATION_UNIT / 8);
      /* if there is no quantization, there is no need to draw
         rounded corners. >>> Set the effective skyline radius to 0 */
      Real radius = (quantization ? diameter / 2 : 0.);

      /* draw straight lines */
      Offset points[] = {
        Offset (-left, -bottom + radius), Offset (-left, top - radius),
        Offset (-left + radius, -bottom), Offset (right - radius, -bottom),
        Offset (right, -bottom + radius), Offset (right, top - radius),
        Offset (-left + radius, top),     Offset (right - radius, top),
      };
      for (vsize i = 0; i < ARRAYSIZE (points); i += 2)
        {
          skyline->add_segment (transform, points[i], points[i + 1]);
        }

      /* draw rounded corners */
      if (radius)
        {
          Offset rad (radius, 0);
          Drul_array<Real> cx;
          Drul_array<Real> cy;

          cx[LEFT] = -left + radius;
          cx[RIGHT] = right - radius;
          cy[DOWN] = -bottom + radius;
          cy[UP] = top - radius;

          for (DOWN_and_UP (v))
            for (LEFT_and_RIGHT (h))
              {
                Offset last;
                for (vsize i = 0; i <= (vsize) quantization; i++)
                  {
                    Real ang = linear_interpolate (static_cast<Real> (i), 0,
                                                   quantization, 0., 90.);
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
make_draw_bezier_boxes_scm (Lazy_skyline_pair *skyline,
                            Transform const &transform, SCM expr)
{
  Real th = robust_scm2double (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real x0 = robust_scm2double (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real y0 = robust_scm2double (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real x1 = robust_scm2double (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real y1 = robust_scm2double (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real x2 = robust_scm2double (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real y2 = robust_scm2double (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real x3 = robust_scm2double (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real y3 = robust_scm2double (scm_car (expr), 0.0);

  Offset ps[] = {
    Offset (x0, y0),
    Offset (x1, y1),
    Offset (x2, y2),
    Offset (x3, y3),
  };
  make_draw_bezier_boxes (skyline, transform, th, ps);
}

void
make_draw_bezier_boxes (Lazy_skyline_pair *skyline, Transform const &transform,
                        Real th, Offset control[4])
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
  int quantization = int (len / QUANTIZATION_UNIT);

  vector<Offset> points;
  points.reserve (quantization + 1);

  last = curve.control_[0];
  for (vsize i = 1; i < (vsize) quantization; i++)
    {
      Real t = static_cast<Real> (i) / quantization;
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
          Real x = robust_scm2double (scm_cadr (expr), 0.0);
          Real y = robust_scm2double (scm_caddr (expr), 0.0);
          start = Offset (x, y);
          current = start;
          expr = scm_cdddr (expr);
        }
      if (scm_is_eq (scm_car (expr), ly_symbol2scm ("rmoveto")))
        {
          Real x = robust_scm2double (scm_cadr (expr), 0.0);
          Real y = robust_scm2double (scm_caddr (expr), 0.0);
          start = (Offset (x, y) + current);
          current = start;
          expr = scm_cdddr (expr);
        }
      else if (scm_is_eq (scm_car (expr), ly_symbol2scm ("lineto")))
        {
          Real x = robust_scm2double (scm_cadr (expr), 0.0);
          Real y = robust_scm2double (scm_caddr (expr), 0.0);
          out = scm_cons (scm_list_4 (scm_from_double (current[X_AXIS]),
                                      scm_from_double (current[Y_AXIS]),
                                      scm_from_double (x),
                                      scm_from_double (y)),
                          out);
          current = Offset (x, y);
          expr = scm_cdddr (expr);
        }
      else if (scm_is_eq (scm_car (expr), ly_symbol2scm ("rlineto")))
        {
          Real x = robust_scm2double (scm_cadr (expr), 0.0);
          Real y = robust_scm2double (scm_caddr (expr), 0.0);
          out = scm_cons (scm_list_4 (scm_from_double (current[X_AXIS]),
                                      scm_from_double (current[Y_AXIS]),
                                      scm_from_double (x + current[X_AXIS]),
                                      scm_from_double (y + current[Y_AXIS])),
                          out);
          current = (Offset (x, y) + current);
          expr = scm_cdddr (expr);
        }
      else if (scm_is_eq (scm_car (expr), ly_symbol2scm ("curveto")))
        {
          Real x1 = robust_scm2double (scm_cadr (expr), 0.0);
          expr = scm_cddr (expr);
          Real y1 = robust_scm2double (scm_car (expr), 0.0);
          expr = scm_cdr (expr);
          Real x2 = robust_scm2double (scm_car (expr), 0.0);
          expr = scm_cdr (expr);
          Real y2 = robust_scm2double (scm_car (expr), 0.0);
          expr = scm_cdr (expr);
          Real x3 = robust_scm2double (scm_car (expr), 0.0);
          expr = scm_cdr (expr);
          Real y3 = robust_scm2double (scm_car (expr), 0.0);
          expr = scm_cdr (expr);
          out = scm_cons (scm_list_n (scm_from_double (current[X_AXIS]),
                                      scm_from_double (current[Y_AXIS]),
                                      scm_from_double (x1),
                                      scm_from_double (y1),
                                      scm_from_double (x2),
                                      scm_from_double (y2),
                                      scm_from_double (x3),
                                      scm_from_double (y3),
                                      SCM_UNDEFINED),
                          out);
          current = Offset (x3, y3);
        }
      else if (scm_is_eq (scm_car (expr), ly_symbol2scm ("rcurveto")))
        {
          Real x1 = robust_scm2double (scm_cadr (expr), 0.0);
          expr = scm_cddr (expr);
          Real y1 = robust_scm2double (scm_car (expr), 0.0);
          expr = scm_cdr (expr);
          Real x2 = robust_scm2double (scm_car (expr), 0.0);
          expr = scm_cdr (expr);
          Real y2 = robust_scm2double (scm_car (expr), 0.0);
          expr = scm_cdr (expr);
          Real x3 = robust_scm2double (scm_car (expr), 0.0);
          expr = scm_cdr (expr);
          Real y3 = robust_scm2double (scm_car (expr), 0.0);
          expr = scm_cdr (expr);
          out = scm_cons (scm_list_n (scm_from_double (current[X_AXIS]),
                                      scm_from_double (current[Y_AXIS]),
                                      scm_from_double (x1 + current[X_AXIS]),
                                      scm_from_double (y1 + current[Y_AXIS]),
                                      scm_from_double (x2 + current[X_AXIS]),
                                      scm_from_double (y2 + current[Y_AXIS]),
                                      scm_from_double (x3 + current[X_AXIS]),
                                      scm_from_double (y3 + current[Y_AXIS]),
                                      SCM_UNDEFINED),
                          out);
          current = (Offset (x3, y3) + current);
        }
      else if (scm_is_eq (scm_car (expr), ly_symbol2scm ("closepath")))
        {
          if ((current[X_AXIS] != start[X_AXIS])
              || (current[Y_AXIS] != start[Y_AXIS]))
            {
              out = scm_cons (scm_list_4 (scm_from_double (current[X_AXIS]),
                                          scm_from_double (current[Y_AXIS]),
                                          scm_from_double (start[X_AXIS]),
                                          scm_from_double (start[Y_AXIS])),
                              out);
              current = start;
            }
          expr = scm_cdr (expr);
        }
      else
        {
          warning ("Malformed path for path stencil.");
          return out;
        }
      first = false;
    }
  return scm_reverse_x (out, SCM_EOL);
}

void
internal_make_path_boxes (Lazy_skyline_pair *skyline,
                          Transform const &transform, SCM expr)
{
  SCM blot = scm_car (expr);
  expr = scm_cdr (expr);
  SCM path = all_commands_to_absolute_and_group (expr);
  // note that expr has more stuff that we don't need after this - simply ignore it
  //////////////////////
  for (SCM s = path; scm_is_pair (s); s = scm_cdr (s))
    {
      scm_to_int (scm_length (scm_car (s))) == 4
        ? make_draw_line_boxes (skyline, transform,
                                scm_cons (blot, scm_car (s)))
        : make_draw_bezier_boxes_scm (skyline, transform,
                                      scm_cons (blot, scm_car (s)));
    }
}

void
make_path_boxes (Lazy_skyline_pair *skyline, Transform const &transform,
                 SCM expr)
{
  return internal_make_path_boxes (
    skyline, transform,
    scm_cons (scm_car (expr), get_path_list (scm_cdr (expr))));
}

void
make_polygon_boxes (Lazy_skyline_pair *skyline, Transform const &transform,
                    SCM expr)
{
  SCM coords = get_number_list (scm_car (expr));
  expr = scm_cdr (expr);
  SCM blot_diameter = scm_car (expr);

  bool first = true;
  SCM l = SCM_EOL;
  for (SCM s = coords; scm_is_pair (s); s = scm_cddr (s))
    {
      l = scm_cons (first ? ly_symbol2scm ("moveto") : ly_symbol2scm ("lineto"), l);
      l = scm_cons (scm_car (s), l);
      l = scm_cons (scm_cadr (s), l);
      first = false;
    }
  l = scm_cons (ly_symbol2scm ("closepath"), l);
  internal_make_path_boxes (
    skyline, transform, scm_cons (blot_diameter, scm_reverse_x (l, SCM_EOL)));
}

void
make_named_glyph_boxes (Lazy_skyline_pair *skyline, Transform const &transform,
                        SCM expr)
{
  SCM fm_scm = scm_car (expr);
  Font_metric *fm = unsmob<Font_metric> (fm_scm);
  expr = scm_cdr (expr);
  SCM glyph = scm_car (expr);
  string glyph_s = ly_scm2string (glyph);

  Open_type_font *open_fm
    = dynamic_cast<Open_type_font *>
      (dynamic_cast<Modified_font_metric *>(fm)->original_font ());
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

  SCM outline = open_fm->get_glyph_outline (gidx);
  for (SCM s = outline;
       scm_is_pair (s);
       s = scm_cdr (s))
    {
      scm_to_int (scm_length (scm_car (s))) == 4
        ? make_draw_line_boxes (skyline, local,
                                scm_cons (scm_from_double (0), scm_car (s)))
        : make_draw_bezier_boxes_scm (
          skyline, local, scm_cons (scm_from_double (0), scm_car (s)));
    }
}

void
make_glyph_string_boxes (Lazy_skyline_pair *skyline, Transform const &transform,
                         SCM expr)
{
  SCM fm_scm = scm_car (expr);
  Font_metric *fm = unsmob<Font_metric> (fm_scm);
  expr = scm_cdr (expr);
  expr = scm_cdr (expr); // font-name
  expr = scm_cdr (expr); // size
  expr = scm_cdr (expr); // cid?
  SCM whxy = scm_cadar (expr);
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
      widths.push_back (robust_scm2double (scm_car (now), 0.0));
      now = scm_cdr (now);
      heights.push_back (robust_scm2interval (scm_car (now), Interval (0, 0)));
      now = scm_cdr (now);
      xos.push_back (robust_scm2double (scm_car (now), 0.0));
      now = scm_cdr (now);
      yos.push_back (robust_scm2double (scm_car (now), 0.0));
      now = scm_cdr (now);
      char_ids.push_back (robust_scm2string (scm_car (now), ""));
    }
  Real cumulative_x = 0.0;
  for (vsize i = 0; i < widths.size (); i++)
    {
      Transform transcopy = transform;
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
          SCM outline = pango_fm->get_glyph_outline (gidx);
          assert (abs (xlen - ylen) < 10e-3);

          Real scale_factor = std::max (xlen, ylen);
          // the three operations below move the stencil from its original coordinates to current coordinates
          // FIXME: this looks extremely fishy.
          transcopy
            .translate (
              Offset (kerned_bbox[X_AXIS][LEFT],
                      kerned_bbox[Y_AXIS][DOWN] - real_bbox[Y_AXIS][DOWN]))
            .translate (
              Offset (real_bbox[X_AXIS][LEFT], real_bbox[Y_AXIS][DOWN]))
            .scale (scale_factor, scale_factor)
            .translate (-Offset (bbox[X_AXIS][LEFT], bbox[Y_AXIS][DOWN]));

          for (SCM s = outline; scm_is_pair (s); s = scm_cdr (s))
            {
              scm_to_int (scm_length (scm_car (s))) == 4
                ? make_draw_line_boxes (
                  skyline, transcopy,
                  scm_cons (scm_from_double (0), scm_car (s)))
                : make_draw_bezier_boxes_scm (
                  skyline, transcopy,
                  scm_cons (scm_from_double (0), scm_car (s)));
            }
        }
    }
}

/*
  receives a stencil expression and a transform matrix
  depending on the stencil name, dispatches it to the appropriate function
*/

void
stencil_dispatcher (Lazy_skyline_pair *skyline, Transform const &transform,
                    SCM expr)
{
  if (!scm_is_pair (expr))
    return;

  SCM head = scm_car (expr);
  if (scm_is_eq (head, ly_symbol2scm ("draw-line")))
    make_draw_line_boxes (skyline, transform, scm_cdr (expr));
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
      make_draw_line_boxes (
        skyline, transform,
        scm_list_5 (th, scm_from_double (0.0), scm_from_double (0.0), x1, x2));
    }
  else if (scm_is_eq (head, ly_symbol2scm ("circle")))
    {
      expr = scm_cdr (expr);
      SCM rad = scm_car (expr);
      expr = scm_cdr (expr);
      SCM th = scm_car (expr);
      make_partial_ellipse_boxes_scm (
        skyline, transform,
        scm_list_n (rad, rad, scm_from_double (0.0), scm_from_double (360.0),
                    th, SCM_BOOL_F, SCM_BOOL_T, SCM_UNDEFINED));
    }
  else if (scm_is_eq (head, ly_symbol2scm ("ellipse")))
    {
      expr = scm_cdr (expr);
      SCM x_rad = scm_car (expr);
      expr = scm_cdr (expr);
      SCM y_rad = scm_car (expr);
      expr = scm_cdr (expr);
      SCM th = scm_car (expr);
      make_partial_ellipse_boxes_scm (
        skyline, transform,
        scm_list_n (x_rad, y_rad, scm_from_double (0.0),
                    scm_from_double (360.0), th, SCM_BOOL_F, SCM_BOOL_T,
                    SCM_UNDEFINED));
    }
  else if (scm_is_eq (head, ly_symbol2scm ("partial-ellipse")))
    make_partial_ellipse_boxes_scm (skyline, transform, scm_cdr (expr));
  else if (scm_is_eq (head, ly_symbol2scm ("round-filled-box")))
    make_round_filled_box_boxes (skyline, transform, scm_cdr (expr));
  else if (scm_is_eq (head, ly_symbol2scm ("named-glyph")))
    make_named_glyph_boxes (skyline, transform, scm_cdr (expr));
  else if (scm_is_eq (head, ly_symbol2scm ("polygon")))
    make_polygon_boxes (skyline, transform, scm_cdr (expr));
  else if (scm_is_eq (head, ly_symbol2scm ("path")))
    make_path_boxes (skyline, transform, scm_cdr (expr));
  else if (scm_is_eq (head, ly_symbol2scm ("glyph-string")))
    make_glyph_string_boxes (skyline, transform, scm_cdr (expr));
  else
    {
      /*
        We don't issue a warning here, as we assume that stencil-expression.cc
        is doing stencil-checking correctly.
      */
    }
}

/*
  traverses a stencil expression, returning a reversed list of Scheme
  pairs, consisting of a Transform indicating where to move a stencil
  and the stencil expression to show how to construct the stencil

  The given "tail" is appended.
*/
SCM
stencil_traverser (Transform const &transform, SCM expr, SCM tail)
{
  if (scm_is_null (expr)
      || (scm_is_string (expr) && scm_is_true (scm_string_null_p (expr))))
    return tail;

  SCM head = scm_car (expr);
  if (scm_is_eq (head, ly_symbol2scm ("combine-stencil")))
    {
      for (SCM s = scm_cdr (expr); scm_is_pair (s); s = scm_cdr (s))
        tail = stencil_traverser (transform, scm_car (s), tail);
      return tail;
    }
  else if (scm_is_eq (head, ly_symbol2scm ("footnote")))
    return tail;
  else if (scm_is_eq (head, ly_symbol2scm ("translate-stencil")))
    {
      Offset p = robust_scm2offset (scm_cadr (expr), Offset (0.0, 0.0));
      Transform local = transform;
      local.translate (p);
      return stencil_traverser (local, scm_caddr (expr), tail);
    }
  else if (scm_is_eq (head, ly_symbol2scm ("scale-stencil")))
    {
      Real x = robust_scm2double (scm_caadr (expr), 0.0);
      Real y = robust_scm2double (scm_cadadr (expr), 0.0);
      Transform local = transform;
      local.scale (x, y);
      return stencil_traverser (local, scm_caddr (expr), tail);
    }
  else if (scm_is_eq (head, ly_symbol2scm ("rotate-stencil")))
    {
      Real ang = robust_scm2double (scm_caadr (expr), 0.0);
      Offset center = robust_scm2offset (scm_cadadr (expr), Offset (0.0, 0.0));
      Transform local = transform;
      local.rotate (ang, center);
      return stencil_traverser (local, scm_caddr (expr), tail);
    }
  else if (scm_is_eq (head, ly_symbol2scm ("delay-stencil-evaluation")))
    // should not use the place-holder text, but no need for the warning below
    return tail;
  else if (scm_is_eq (head, ly_symbol2scm ("grob-cause")))
    return stencil_traverser (transform, scm_caddr (expr), tail);
  else if (scm_is_eq (head, ly_symbol2scm ("color")))
    return stencil_traverser (transform, scm_caddr (expr), tail);
  else if (scm_is_eq (head, ly_symbol2scm ("output-attributes")))
    return stencil_traverser (transform, scm_caddr (expr), tail);
  else if (scm_is_eq (head, ly_symbol2scm ("with-outline")))
    return stencil_traverser (transform, scm_cadr (expr), tail);
  else
    {
      return scm_acons (transform.smobbed_copy (), expr, tail);
    }
  warning ("Stencil expression not supported by the vertical skylines.");
  return tail;
}

SCM
Grob::maybe_pure_internal_simple_skylines_from_extents (Grob *me, Axis a, bool pure, int beg, int end, bool ignore_x, bool ignore_y)
{
  vector<Box> boxes;
  // we don't know how far spanners stretch along the X axis before
  // line breaking. better have them take up the whole thing
  Interval xex = ignore_x
                 ? Interval (-infinity_f, infinity_f)
                 : me->extent (me, X_AXIS);

  // If we're looking at the x exent of a cross staff grob, it could be
  // very early on in the computation process.  We won't know its height
  // until way later, so we give a brute force approximation.
  Interval yex = ignore_y
                 ? Interval (-infinity_f, infinity_f)
                 : me->maybe_pure_extent (me, Y_AXIS, pure, beg, end);

  if (xex.is_empty () || yex.is_empty ())
    return Skyline_pair ().smobbed_copy ();

  boxes.push_back (Box (xex, yex));
  return Skyline_pair (boxes, a).smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Grob, pure_simple_vertical_skylines_from_extents, 3);
SCM
Grob::pure_simple_vertical_skylines_from_extents (SCM smob, SCM begscm, SCM endscm)
{
  Grob *me = unsmob<Grob> (smob);
  int beg = robust_scm2int (begscm, 0);
  int end = robust_scm2int (endscm, INT_MAX);
  // We cannot measure the widths before line breaking,
  // so we assume that the width is infinite: pass ignore_x=true
  return maybe_pure_internal_simple_skylines_from_extents (me, X_AXIS, true, beg, end, true, false);
}

MAKE_SCHEME_CALLBACK (Grob, simple_vertical_skylines_from_extents, 1);
SCM
Grob::simple_vertical_skylines_from_extents (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);
  return maybe_pure_internal_simple_skylines_from_extents (me, X_AXIS, false, 0, 0, false, false);
}

MAKE_SCHEME_CALLBACK (Grob, pure_simple_horizontal_skylines_from_extents, 3);
SCM
Grob::pure_simple_horizontal_skylines_from_extents (SCM smob, SCM begscm, SCM endscm)
{
  Grob *me = unsmob<Grob> (smob);
  int beg = robust_scm2int (begscm, 0);
  int end = robust_scm2int (endscm, INT_MAX);
  // If the grob is cross staff, we cannot measure its Y-extent before
  // wayyyy downstream (after spacing of axis groups is done).
  // Thus, we assume that the Y extent is infinite for cross staff grobs.
  return maybe_pure_internal_simple_skylines_from_extents (me, Y_AXIS, true, beg, end, false, to_boolean (get_property (me, "cross-staff")));
}

MAKE_SCHEME_CALLBACK (Grob, simple_horizontal_skylines_from_extents, 1);
SCM
Grob::simple_horizontal_skylines_from_extents (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);
  // See comment in function above.
  return maybe_pure_internal_simple_skylines_from_extents (me, Y_AXIS, false, 0, 0, false, to_boolean (get_property (me, "cross-staff")));
}

SCM
Stencil::skylines_from_stencil (SCM sten, Real pad, SCM rot, Axis a)
{
  Stencil *s = unsmob<Stencil> (sten);

  if (!s)
    return Skyline_pair ().smobbed_copy ();

  if (scm_is_pair (rot))
    {
      Real angle = robust_scm2double (scm_car (rot), 0.0);
      Real x = robust_scm2double (scm_cadr (rot), 0.0);
      Real y = robust_scm2double (scm_caddr (rot), 0.0);

      // incorporate rotation into a stencil copy
      sten = s->smobbed_copy ();
      s = unsmob<Stencil> (sten);
      s->rotate_degrees (angle, Offset (x, y));
    }

  SCM data = scm_reverse_x (
    stencil_traverser (Transform::identity, s->expr (), SCM_EOL), SCM_EOL);

  Lazy_skyline_pair lazy (a);
  for (SCM s = scm_reverse_x (data, SCM_EOL); scm_is_pair (s); s = scm_cdr (s))
    stencil_dispatcher (&lazy, robust_scm2transform (scm_caar (s)),
                        scm_cdar (s));

  Skyline_pair out;
  for (DOWN_and_UP (d))
    out[d] = lazy.to_pair ()[d].padded (pad);

  return out.smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Grob, vertical_skylines_from_stencil, 1);
SCM
Grob::vertical_skylines_from_stencil (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);
  Real pad = robust_scm2double (get_property (me, "skyline-horizontal-padding"), 0.0);
  SCM rot = get_property (me, "rotation");
  SCM out = Stencil::skylines_from_stencil (get_property (me, "stencil"),
                                            pad, rot, X_AXIS);
  return out;
}

MAKE_SCHEME_CALLBACK (Grob, horizontal_skylines_from_stencil, 1);
SCM
Grob::horizontal_skylines_from_stencil (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);
  Real pad = robust_scm2double (get_property (me, "skyline-vertical-padding"), 0.0);
  SCM rot = get_property (me, "rotation");
  SCM out = Stencil::skylines_from_stencil (get_property (me, "stencil"),
                                            pad, rot, Y_AXIS);

  return out;
}

SCM
Grob::internal_skylines_from_element_stencils (Grob *me, Axis a, bool pure, int beg, int end)
{

  extract_grob_set (me, "elements", elts);
  vector<Real> x_pos;
  vector<Real> y_pos;
  Grob *x_common = common_refpoint_of_array (elts, me, X_AXIS);
  Grob *y_common = common_refpoint_of_array (elts, me, Y_AXIS);
  for (vsize i = 0; i < elts.size (); i++)
    {
      x_pos.push_back (elts[i]->relative_coordinate (x_common, X_AXIS));
      y_pos.push_back (elts[i]->maybe_pure_coordinate (y_common, Y_AXIS, pure, beg, end));
    }
  Real my_x = me->relative_coordinate (x_common, X_AXIS);
  Real my_y = me->maybe_pure_coordinate (y_common, Y_AXIS, pure, beg, end);

  Skyline_pair res;
  for (vsize i = 0; i < elts.size (); i++)
    {
      Skyline_pair *skyp = unsmob<Skyline_pair> (get_maybe_pure_property (elts[i], a == X_AXIS ? "vertical-skylines" : "horizontal-skylines", pure, beg, end));
      if (skyp)
        {
          /*
            Here, copying is essential.  Otherwise, the skyline pair will
            get doubly shifted!
          */
          /*
            It took Mike about 6 months of his life to add the `else' clause
            below.  For horizontal skylines, the raise and shift calls need
            to be reversed.  This is what was causing the problems in the
            shifting with all of the tests. RIP 6 months!
          */
          Skyline_pair copy = Skyline_pair (*skyp);
          if (a == X_AXIS)
            {
              copy.shift (x_pos[i] - my_x);
              copy.raise (y_pos[i] - my_y);
            }
          else
            {
              copy.raise (x_pos[i] - my_x);
              copy.shift (y_pos[i] - my_y);
            }
          res.merge (copy);
        }
    }
  return res.smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Grob, vertical_skylines_from_element_stencils, 1);
SCM
Grob::vertical_skylines_from_element_stencils (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);
  return internal_skylines_from_element_stencils (me, X_AXIS, false, 0, INT_MAX);
}

MAKE_SCHEME_CALLBACK (Grob, horizontal_skylines_from_element_stencils, 1);
SCM
Grob::horizontal_skylines_from_element_stencils (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);
  return internal_skylines_from_element_stencils (me, Y_AXIS, false, 0, INT_MAX);
}

MAKE_SCHEME_CALLBACK (Grob, pure_vertical_skylines_from_element_stencils, 3);
SCM
Grob::pure_vertical_skylines_from_element_stencils (SCM smob, SCM beg_scm, SCM end_scm)
{
  Grob *me = unsmob<Grob> (smob);
  int beg = robust_scm2int (beg_scm, 0);
  int end = robust_scm2int (end_scm, 0);
  return internal_skylines_from_element_stencils (me, X_AXIS, true, beg, end);
}

MAKE_SCHEME_CALLBACK (Grob, pure_horizontal_skylines_from_element_stencils, 3);
SCM
Grob::pure_horizontal_skylines_from_element_stencils (SCM smob, SCM beg_scm, SCM end_scm)
{
  Grob *me = unsmob<Grob> (smob);
  int beg = robust_scm2int (beg_scm, 0);
  int end = robust_scm2int (end_scm, 0);
  return internal_skylines_from_element_stencils (me, Y_AXIS, true, beg, end);
}
