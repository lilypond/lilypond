/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2012 Mike Solomon <mike@mikesolomon.org>

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

/*
tools for transform-matrices following the standard at
http://www.w3.org/TR/SVG/coords.html

a list in the form
(list a b c d e f g)
becomes this matrix:
[ a c e ]
[ b d f ]
[ 0 0 1 ]
when this transforms a point (x,y), the point is written as matrix:
[ x ]
[ y ]
[ 1 ]
*/

#include <pango/pango-matrix.h>
#include <complex>
#include "box.hh"
#include "bezier.hh"
#include "dimensions.hh"
#include "font-metric.hh"
#include "grob.hh"
#include "interval.hh"
#include "freetype.hh"
#include "misc.hh"
#include "offset.hh"
#include "modified-font-metric.hh"
#include "open-type-font.hh"
#include "pango-font.hh"
#include "pointer-group-interface.hh"
#include "lily-guile.hh"
#include "real.hh"
#include "rest.hh"
#include "stencil.hh"
#include "string-convert.hh"
#include "skyline.hh"
#include "skyline-pair.hh"
#include "spanner.hh"
using namespace std;

Real QUANTIZATION_UNIT = 0.2;

void create_path_cap (vector<Box> &boxes, vector<Drul_array<Offset> > &buildings, PangoMatrix trans, Offset pt, Real rad, Real slope, Direction d);

struct Transform_matrix_and_expression
{
  PangoMatrix tm_;
  SCM expr_;

  Transform_matrix_and_expression (PangoMatrix tm, SCM expr);
};

Transform_matrix_and_expression::Transform_matrix_and_expression (PangoMatrix tm, SCM expr)
{
  tm_ = tm;
  expr_ = expr;
}

PangoMatrix
make_transform_matrix (Real p0, Real p1, Real p2, Real p3, Real p4, Real p5)
{
  PangoMatrix out;
  out.xx = p0;
  out.xy = p1;
  out.yx = p2;
  out.yy = p3;
  out.x0 = p4;
  out.y0 = p5;
  return out;
}

//// UTILITY FUNCTIONS

/*
  map x's placement between orig_l and orig_r onto
  the interval final_l final_r
*/
Real
linear_map (Real final_l, Real final_r, Real orig_l, Real orig_r, Real x)
{
  return final_l + ((final_r - final_l) * ((x - orig_l) / (orig_r - orig_l)));
}

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
      if (res == SCM_BOOL_F)
        return get_number_list (scm_cdr (l));
      return res;
    }
  return SCM_BOOL_F;
}

/*
  from a nested SCM list, return the first list of numbers
  useful for paths
*/
SCM
get_path_list (SCM l)
{
  if (scm_is_pair (l))
    {
      if (scm_memv (scm_car (l),
                    scm_list_n (ly_symbol2scm ("moveto"),
                                ly_symbol2scm ("rmoveto"),
                                ly_symbol2scm ("lineto"),
                                ly_symbol2scm ("rlineto"),
                                ly_symbol2scm ("curveto"),
                                ly_symbol2scm ("rcurveto"),
                                ly_symbol2scm ("closepath"),
                                SCM_UNDEFINED))
          != SCM_BOOL_F)
        return l;
      SCM res = get_path_list (scm_car (l));
      if (res == SCM_BOOL_F)
        return get_path_list (scm_cdr (l));
      return res;
    }
  return SCM_BOOL_F;
}

Real
perpendicular_slope (Real s)
{
  if (s == 0.0)
    return infinity_f;
  if (s == infinity_f)
    return 0.0;
  return -1.0 / s;
}

//// END UTILITY FUNCTIONS

/*
  below, for all of the functions make_X_boxes, the expression
  is always unpacked into variables.
  then, after a line of /////, there are manipulations of these variables
  (there may be no manipulations necessary depending on the function)
  afterwards, there is another ///// followed by the creation of points
  and boxes
*/

void
make_draw_line_boxes (vector<Box> &boxes, vector<Drul_array<Offset> > &buildings, PangoMatrix trans, SCM expr, bool use_building)
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
  Real slope = x1 == x0 ? infinity_f : (y1 - y0) / (x1 - x0);
  //////////////////////
  if (x1 < x0)
    {
      swap (x0, x1);
      swap (y0, y1);
    }
  Offset left (x0, y0);
  Offset right (x1, y1);
  Direction d = DOWN;
  do
    {
      Offset inter_l = get_point_in_y_direction (left, perpendicular_slope (slope), thick / 2, d);
      Offset inter_r = get_point_in_y_direction (right, perpendicular_slope (slope), thick / 2, d);
      pango_matrix_transform_point (&trans, &inter_l[X_AXIS], &inter_l[Y_AXIS]);
      pango_matrix_transform_point (&trans, &inter_r[X_AXIS], &inter_r[Y_AXIS]);
      if ((inter_l[X_AXIS] == inter_r[X_AXIS]) || (inter_l[Y_AXIS] == inter_r[Y_AXIS]))
        {
          Box b;
          b.add_point (inter_l);
          b.add_point (inter_r);
          boxes.push_back (b);
        }
      else if (use_building)
        buildings.push_back (Drul_array<Offset> (inter_l, inter_r));
      else
        {
          Offset inter_l = get_point_in_y_direction (left, perpendicular_slope (slope), thick / 2, d);
          Offset inter_r = get_point_in_y_direction (right, perpendicular_slope (slope), thick / 2, d);
          pango_matrix_transform_point (&trans, &inter_l[X_AXIS], &inter_l[Y_AXIS]);
          pango_matrix_transform_point (&trans, &inter_r[X_AXIS], &inter_r[Y_AXIS]);
          Real length = sqrt (((inter_l[X_AXIS] - inter_r[X_AXIS]) * (inter_l[X_AXIS] - inter_r[X_AXIS])) + ((inter_l[Y_AXIS] - inter_r[Y_AXIS]) * (inter_l[Y_AXIS] - inter_r[Y_AXIS])));

          vsize passes = (vsize) ((length * 2) + 1);
          vector<Offset> points;

          for (vsize i = 0; i < 1 + passes; i++)
            {
              Offset pt (linear_map (x0, x1, 0, passes, i),
                         linear_map (y0, y1, 0, passes, i));
              Offset inter = get_point_in_y_direction (pt, perpendicular_slope (slope), thick / 2, d);
              pango_matrix_transform_point (&trans, &inter[X_AXIS], &inter[Y_AXIS]);
              points.push_back (inter);
            }
          for (vsize i = 0; i < points.size () - 1; i++)
            {
              Box b;
              b.add_point (points[i]);
              b.add_point (points[i + 1]);
              boxes.push_back (b);
            }
        }
    }
  while (flip (&d) != DOWN);

  if (thick > 0.0)
    {
      // beg line cap
      create_path_cap (boxes,
                       buildings,
                       trans,
                       Offset (x0, y0),
                       thick / 2,
                       perpendicular_slope (slope),
                       Direction (sign (slope)));

      // end line cap
      create_path_cap (boxes,
                       buildings,
                       trans,
                       Offset (x1, y1),
                       thick / 2,
                       perpendicular_slope (slope),
                       Direction (sign (-slope)));
    }
}

void
make_partial_ellipse_boxes (vector<Box> &boxes, vector<Drul_array<Offset> > &buildings, PangoMatrix trans, SCM expr)
{
  Real x_rad = robust_scm2double (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real y_rad = robust_scm2double (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real start = robust_scm2double (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real end = robust_scm2double (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real th = robust_scm2double (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  bool connect = to_boolean (scm_car (expr));
  expr = scm_cdr (expr);
  bool fill = to_boolean (scm_car (expr));
  //////////////////////
  start = M_PI * start / 180;
  end = M_PI * end / 180;
  if (end == start)
    end += (2 * M_PI);
  complex<Real> sunit = polar (1.0, start);
  complex<Real> eunit = polar (1.0, end);
  Offset sp (real (sunit) * x_rad, imag (sunit) * y_rad);
  Offset ep (real (eunit) * x_rad, imag (eunit) * y_rad);
  //////////////////////
  Drul_array<vector<Offset> > points;
  Direction d = DOWN;
  int quantization = max (1, (int) (((x_rad * trans.xx) + (y_rad * trans.yy)) * M_PI / QUANTIZATION_UNIT));
  do
    {
      for (vsize i = 0; i < 1 + quantization; i++)
        {
          Real ang = linear_map (start, end, 0, quantization, i);
          complex<Real> coord = polar (1.0, ang);
          Offset pt (real (coord) * x_rad,
                     imag (coord) * y_rad);
          Real slope = pt[Y_AXIS] / pt[X_AXIS];
          Offset inter = get_point_in_y_direction (pt, perpendicular_slope (slope), th / 2, d);
          pango_matrix_transform_point (&trans, &inter[X_AXIS], &inter[Y_AXIS]);
          points[d].push_back (inter);
        }
    }
  while (flip (&d) != DOWN);

  for (vsize i = 0; i < points[DOWN].size () - 1; i++)
    {
      Box b;
      do
        {
          b.add_point (points[d][i]);
          b.add_point (points[d][i + 1]);
        }
      while (flip (&d) != DOWN);
      boxes.push_back (b);
    }

  if (connect || fill)
    {
      make_draw_line_boxes (boxes, buildings, trans, scm_list_5 (scm_from_double (th),
                                                                 scm_from_double (sp[X_AXIS]),
                                                                 scm_from_double (sp[Y_AXIS]),
                                                                 scm_from_double (ep[X_AXIS]),
                                                                 scm_from_double (ep[Y_AXIS])),
                            false);
    }

  if (th > 0.0)
    {
      // beg line cap
      complex<Real> coord = polar (1.0, start);
      Offset pt (real (coord) * x_rad,
                 imag (coord) * y_rad);
      Real slope = pt[Y_AXIS] / pt[X_AXIS];
      create_path_cap (boxes,
                       buildings,
                       trans,
                       pt,
                       th / 2,
                       perpendicular_slope (slope),
                       Direction (sign (slope)));

      // end line cap
      coord = polar (1.0, start);
      pt = Offset (real (coord) * x_rad,
                   imag (coord) * y_rad);
      slope = pt[Y_AXIS] / pt[X_AXIS];
      create_path_cap (boxes,
                       buildings,
                       trans,
                       pt,
                       th / 2,
                       perpendicular_slope (slope),
                       Direction (sign (-slope)));
    }
}

void
make_round_filled_box_boxes (vector<Box> &boxes, PangoMatrix trans, SCM expr)
{
  Real left = robust_scm2double (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real right = robust_scm2double (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real bottom = robust_scm2double (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real top = robust_scm2double (scm_car (expr), 0.0);
  expr = scm_cdr (expr);
  Real th = robust_scm2double (scm_car (expr), 0.0);
  //////////////////////
  vector<Offset> points;
  Box b;
  Offset p0 = Offset (-left - (th / 2), -bottom - (th / 2));
  Offset p1 = Offset (right + (th / 2), top + (th / 2));
  pango_matrix_transform_point (&trans, &p0[X_AXIS], &p0[Y_AXIS]);
  pango_matrix_transform_point (&trans, &p1[X_AXIS], &p1[Y_AXIS]);
  b.add_point (p0);
  b.add_point (p1);
  boxes.push_back (b);
}

void
create_path_cap (vector<Box> &boxes, vector<Drul_array<Offset> > &buildings, PangoMatrix trans, Offset pt, Real rad, Real slope, Direction d)
{
  Real angle = atan (slope) * 180 / M_PI;
  Real other = angle > 180 ? angle - 180 : angle + 180;
  if (angle < other)
    {
      Real holder = other;
      other = angle;
      angle = holder;
    }
  other = (slope >= 0 && d == DOWN) || (slope < 0 && d == UP)
          ? other + 360.0
          : other;
  PangoMatrix new_trans (trans);
  pango_matrix_translate (&new_trans, pt[X_AXIS], pt[Y_AXIS]);
  make_partial_ellipse_boxes (boxes, buildings, new_trans,
                              scm_list_n (scm_from_double (rad),
                                          scm_from_double (rad),
                                          scm_from_double (angle),
                                          scm_from_double (other),
                                          scm_from_double (0.0),
                                          SCM_BOOL_F,
                                          SCM_BOOL_F,
                                          SCM_UNDEFINED));
}

void
make_draw_bezier_boxes (vector<Box> &boxes, vector<Drul_array<Offset> > &buildings, PangoMatrix trans, SCM expr)
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
  //////////////////////
  Bezier curve;
  curve.control_[0] = Offset (x0, y0);
  curve.control_[1] = Offset (x1, y1);
  curve.control_[2] = Offset (x2, y2);
  curve.control_[3] = Offset (x3, y3);
  Offset temp0 (x0, y0);
  Offset temp1 (x1, y1);
  Offset temp2 (x2, y2);
  Offset temp3 (x3, y3);
  pango_matrix_transform_point (&trans, &temp0[X_AXIS], &temp0[Y_AXIS]);
  pango_matrix_transform_point (&trans, &temp1[X_AXIS], &temp1[Y_AXIS]);
  pango_matrix_transform_point (&trans, &temp2[X_AXIS], &temp2[Y_AXIS]);
  pango_matrix_transform_point (&trans, &temp3[X_AXIS], &temp3[Y_AXIS]);
  //////////////////////
  Drul_array<vector<Offset> > points;
  Direction d = DOWN;
  int quantization = int (((temp1 - temp0).length ()
                           + (temp2 - temp1).length ()
                           + (temp3 - temp2).length ())
                          / QUANTIZATION_UNIT);
  do
    {
      Offset first = get_point_in_y_direction (curve.control_[0], perpendicular_slope (curve.slope_at_point (0.0)), th / 2, d);
      pango_matrix_transform_point (&trans, &first[X_AXIS], &first[Y_AXIS]);
      points[d].push_back (first);
      for (vsize i = 1; i < quantization; i++)
        {
          Real pt = (i * 1.0) / quantization;
          Offset inter = get_point_in_y_direction (curve.curve_point (pt), perpendicular_slope (curve.slope_at_point (pt)), th / 2, d);
          pango_matrix_transform_point (&trans, &inter[X_AXIS], &inter[Y_AXIS]);
          points[d].push_back (inter);
        }
      Offset last = get_point_in_y_direction (curve.control_[3], curve.slope_at_point (1.0), th / 2, d);
      pango_matrix_transform_point (&trans, &last[X_AXIS], &last[Y_AXIS]);
      points[d].push_back (last);
    }
  while (flip (&d) != DOWN);

  for (vsize i = 0; i < points[DOWN].size () - 1; i++)
    {
      Box b;
      do
        {
          b.add_point (points[d][i]);
          b.add_point (points[d][i + 1]);
        }
      while (flip (&d) != DOWN);
      boxes.push_back (b);
    }

  // beg line cap
  if (th >= 0)
    {
      Real slope = curve.slope_at_point (0.0);
      d = Direction (sign (slope == 0.0 || abs (slope) == infinity_f
                           ? curve.slope_at_point (0.0001)
                           : slope));

      create_path_cap (boxes,
                       buildings,
                       trans,
                       curve.control_[0],
                       th / 2,
                       perpendicular_slope (curve.slope_at_point (0.0)),
                       d);

      // end line cap
      slope = curve.slope_at_point (1.0);
      d = Direction (sign (slope == 0.0 || abs (slope) == infinity_f
                           ? curve.slope_at_point (0.9999)
                           : slope));

      create_path_cap (boxes,
                       buildings,
                       trans,
                       curve.control_[3],
                       th / 2,
                       perpendicular_slope (curve.slope_at_point (1.0)),
                       d);
    }
}

/*
  converts a path into lists of 4 (line) or 8 (curve) absolute coordinates
  for example:
  '(moveto 1 2 lineto 3 4 rlineto -1 -1 curveto 3 3 5 5 6 6 rcurveto -1 -1 -1 -1 -1 -1 closepath)
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
      if (scm_car (expr) == ly_symbol2scm ("moveto")
          || (scm_car (expr) == ly_symbol2scm ("rmoveto") && first))
        {
          Real x = robust_scm2double (scm_cadr (expr), 0.0);
          Real y = robust_scm2double (scm_caddr (expr), 0.0);
          start = Offset (x, y);
          current = start;
          expr = scm_cdddr (expr);
        }
      if (scm_car (expr) == ly_symbol2scm ("rmoveto"))
        {
          Real x = robust_scm2double (scm_cadr (expr), 0.0);
          Real y = robust_scm2double (scm_caddr (expr), 0.0);
          start = (Offset (x, y) + current);
          current = start;
          expr = scm_cdddr (expr);
        }
      else if (scm_car (expr) == ly_symbol2scm ("lineto"))
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
      else if (scm_car (expr) == ly_symbol2scm ("rlineto"))
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
      else if (scm_car (expr) == ly_symbol2scm ("curveto"))
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
      else if (scm_car (expr) == ly_symbol2scm ("rcurveto"))
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
      else if (scm_car (expr) == ly_symbol2scm ("closepath"))
        {
          if ((current[X_AXIS] != start[X_AXIS]) || (current[Y_AXIS] != start[Y_AXIS]))
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
internal_make_path_boxes (vector<Box> &boxes, vector<Drul_array<Offset> > &buildings, PangoMatrix trans, SCM expr, bool use_building)
{
  SCM blot = scm_car (expr);
  expr = scm_cdr (expr);
  SCM path = all_commands_to_absolute_and_group (expr);
  // note that expr has more stuff that we don't need after this - simply ignore it
  //////////////////////
  for (SCM s = path; scm_is_pair (s); s = scm_cdr (s))
    {
      scm_to_int (scm_length (scm_car (s))) == 4
      ? make_draw_line_boxes (boxes, buildings, trans, scm_cons (blot, scm_car (s)), use_building)
      : make_draw_bezier_boxes (boxes, buildings, trans, scm_cons (blot, scm_car (s)));
    }
}

void
make_path_boxes (vector<Box> &boxes, vector<Drul_array<Offset> > &buildings, PangoMatrix trans, SCM expr)
{
  return internal_make_path_boxes (boxes, buildings, trans, scm_cons (scm_car (expr), get_path_list (scm_cdr (expr))), false);
}

void
make_polygon_boxes (vector<Box> &boxes, vector<Drul_array<Offset> > &buildings, PangoMatrix trans, SCM expr)
{
  SCM coords = get_number_list (scm_car (expr));
  expr = scm_cdr (expr);
  SCM blot_diameter = scm_car (expr);
  //////////////////////
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
  internal_make_path_boxes (boxes, buildings, trans, scm_cons (blot_diameter, scm_reverse_x (l, SCM_EOL)), true);
}

void
make_named_glyph_boxes (vector<Box> &boxes, vector<Drul_array<Offset> > &buildings, PangoMatrix trans, SCM expr)
{
  SCM fm_scm = scm_car (expr);
  Font_metric *fm = unsmob_metrics (fm_scm);
  expr = scm_cdr (expr);
  SCM glyph = scm_car (expr);
  string glyph_s = ly_scm2string (glyph);

  //////////////////////
  Open_type_font *open_fm
    = dynamic_cast<Open_type_font *>
      (dynamic_cast<Modified_font_metric *>(fm)->original_font ());
  SCM_ASSERT_TYPE (open_fm, fm_scm, SCM_ARG1, __FUNCTION__, "OpenType font");

  size_t gidx = open_fm->name_to_index (glyph_s);
  // Bbox is the best approximation of the width based on how it would be
  // calculated in open-type-font.cc if it were based on real extents
  Box bbox = open_fm->get_unscaled_indexed_char_dimensions (gidx);
  bbox.scale (dynamic_cast<Modified_font_metric *>(fm)->get_magnification () * open_fm->design_size () / open_fm->get_units_per_EM ());
  // Real bbox is the real bbox of the object
  Box real_bbox = open_fm->get_glyph_outline_bbox (gidx);

  Real scale = bbox[X_AXIS].length () / real_bbox[X_AXIS].length ();

  pango_matrix_scale (&trans, scale, scale);

  SCM outline = open_fm->get_glyph_outline (gidx);
  //////////////////////
  for (SCM s = outline;
       scm_is_pair (s);
       s = scm_cdr (s))
    {
      scm_to_int (scm_length (scm_car (s))) == 4
      ? make_draw_line_boxes (boxes, buildings, trans, scm_cons (scm_from_double (0), scm_car (s)), false)
      : make_draw_bezier_boxes (boxes, buildings, trans, scm_cons (scm_from_double (0), scm_car (s)));
    }
}

void
make_glyph_string_boxes (vector<Box> &boxes, vector<Drul_array<Offset> > &buildings, PangoMatrix trans, SCM expr)
{
  SCM fm_scm = scm_car (expr);
  Font_metric *fm = unsmob_metrics (fm_scm);
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
  //////////////////////
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
      PangoMatrix transcopy (trans);
      Offset pt0 (cumulative_x + xos[i], heights[i][DOWN] + yos[i]);
      Offset pt1 (cumulative_x + widths[i] + xos[i], heights[i][UP] + yos[i]);
      cumulative_x += widths[i];

      Box kerned_bbox;
      kerned_bbox.add_point (pt0);
      kerned_bbox.add_point (pt1);
      size_t gidx = pango_fm->name_to_index (char_ids[i]);
      Box real_bbox = pango_fm->get_scaled_indexed_char_dimensions (gidx);
      Box bbox = pango_fm->get_unscaled_indexed_char_dimensions (gidx);
      SCM outline = pango_fm->get_glyph_outline (gidx);

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
      if (isnan (xlen) || isnan (ylen) || isinf (xlen) || isinf (ylen))
        outline = box_to_scheme_lines (kerned_bbox);
      else
        {
          assert (abs (xlen - ylen) < 10e-3);

          Real scale_factor = max (xlen, ylen);
          // the three operations below move the stencil from its original coordinates to current coordinates
          pango_matrix_translate (&transcopy, kerned_bbox[X_AXIS][LEFT], kerned_bbox[Y_AXIS][DOWN] - real_bbox[Y_AXIS][DOWN]);
          pango_matrix_translate (&transcopy, real_bbox[X_AXIS][LEFT], real_bbox[Y_AXIS][DOWN]);
          pango_matrix_scale (&transcopy, scale_factor, scale_factor);
          pango_matrix_translate (&transcopy, -bbox[X_AXIS][LEFT], -bbox[Y_AXIS][DOWN]);
        }
      //////////////////////
      for (SCM s = outline;
           scm_is_pair (s);
           s = scm_cdr (s))
        {
          scm_to_int (scm_length (scm_car (s))) == 4
          ? make_draw_line_boxes (boxes, buildings, transcopy, scm_cons (scm_from_double (0), scm_car (s)), false)
          : make_draw_bezier_boxes (boxes, buildings, transcopy, scm_cons (scm_from_double (0), scm_car (s)));
        }
    }
}

/*
  receives a stencil expression and a transform matrix
  depending on the stencil name, dispatches it to the appropriate function
*/

void
stencil_dispatcher (vector<Box> &boxes, vector<Drul_array<Offset> > &buildings, PangoMatrix trans, SCM expr)
{
  if (not scm_is_pair (expr))
    return;
  if (scm_car (expr) == ly_symbol2scm ("draw-line"))
    make_draw_line_boxes (boxes, buildings, trans, scm_cdr (expr), true);
  else if (scm_car (expr) == ly_symbol2scm ("dashed-line"))
    {
      expr = scm_cdr (expr);
      SCM th = scm_car (expr);
      expr = scm_cdr (expr);
      expr = scm_cdr (expr); // on
      expr = scm_cdr (expr); // off
      SCM x1 = scm_car (expr);
      expr = scm_cdr (expr);
      SCM x2 = scm_car (expr);
      make_draw_line_boxes (boxes, buildings, trans, scm_list_5 (th, scm_from_double (0.0), scm_from_double (0.0), x1, x2), true);
    }
  else if (scm_car (expr) == ly_symbol2scm ("circle"))
    {
      expr = scm_cdr (expr);
      SCM rad = scm_car (expr);
      expr = scm_cdr (expr);
      SCM th = scm_car (expr);
      make_partial_ellipse_boxes (boxes, buildings, trans,
                                  scm_list_n (rad,
                                              rad,
                                              scm_from_double (0.0),
                                              scm_from_double (360.0),
                                              th,
                                              SCM_BOOL_F,
                                              SCM_BOOL_T,
                                              SCM_UNDEFINED));
    }
  else if (scm_car (expr) == ly_symbol2scm ("ellipse"))
    {
      expr = scm_cdr (expr);
      SCM x_rad = scm_car (expr);
      expr = scm_cdr (expr);
      SCM y_rad = scm_car (expr);
      expr = scm_cdr (expr);
      SCM th = scm_car (expr);
      make_partial_ellipse_boxes (boxes, buildings, trans,
                                  scm_list_n (x_rad,
                                              y_rad,
                                              scm_from_double (0.0),
                                              scm_from_double (360.0),
                                              th,
                                              SCM_BOOL_F,
                                              SCM_BOOL_T,
                                              SCM_UNDEFINED));
    }
  else if (scm_car (expr) == ly_symbol2scm ("partial-ellipse"))
    make_partial_ellipse_boxes (boxes, buildings, trans, scm_cdr (expr));
  else if (scm_car (expr) == ly_symbol2scm ("round-filled-box"))
    make_round_filled_box_boxes (boxes, trans, scm_cdr (expr));
  else if (scm_car (expr) == ly_symbol2scm ("named-glyph"))
    make_named_glyph_boxes (boxes, buildings, trans, scm_cdr (expr));
  else if (scm_car (expr) == ly_symbol2scm ("polygon"))
    make_polygon_boxes (boxes, buildings, trans, scm_cdr (expr));
  else if (scm_car (expr) == ly_symbol2scm ("path"))
    make_path_boxes (boxes, buildings, trans, scm_cdr (expr));
  else if (scm_car (expr) == ly_symbol2scm ("glyph-string"))
    make_glyph_string_boxes (boxes, buildings, trans, scm_cdr (expr));
  else
    {
#if 0
      warning ("Stencil expression not supported by the veritcal skylines.");
#endif
      /*
        We don't issue a warning here, as we assume that stencil-expression.cc
        is doing stencil-checking correctly.
      */
    }
}

/*
  traverses a stencil expression, returning a vector of Transform_matrix_and_expression
  the struct Transform_matrix_and_expression contains two members,
  a Transform_matrix that indicates where to move a stencil and the stencil expression
  to show how to construct the stencil
*/
vector<Transform_matrix_and_expression>
stencil_traverser (PangoMatrix trans, SCM expr)
{
  if (scm_is_null (expr))
    return vector<Transform_matrix_and_expression> ();
  else if (expr == ly_string2scm (""))
    return vector<Transform_matrix_and_expression> ();
  else if (scm_car (expr) == ly_symbol2scm ("combine-stencil"))
    {
      vector<Transform_matrix_and_expression> out;
      for (SCM s = scm_cdr (expr); scm_is_pair (s); s = scm_cdr (s))
        {
          vector<Transform_matrix_and_expression> res = stencil_traverser (trans, scm_car (s));
          out.insert (out.end (), res.begin (), res.end ());
        }
      return out;
    }
  else if (scm_car (expr) == ly_symbol2scm ("footnote"))
    return vector<Transform_matrix_and_expression> ();
  else if (scm_car (expr) == ly_symbol2scm ("translate-stencil"))
    {
      Real x = robust_scm2double (scm_caadr (expr), 0.0);
      Real y = robust_scm2double (scm_cdadr (expr), 0.0);
      pango_matrix_translate (&trans, x, y);
      return stencil_traverser (trans, scm_caddr (expr));
    }
  else if (scm_car (expr) == ly_symbol2scm ("scale-stencil"))
    {
      Real x = robust_scm2double (scm_caadr (expr), 0.0);
      Real y = robust_scm2double (scm_cadadr (expr), 0.0);
      pango_matrix_scale (&trans, x, y);
      return stencil_traverser (trans, scm_caddr (expr));
    }
  else if (scm_car (expr) == ly_symbol2scm ("rotate-stencil"))
    {
      Real ang = robust_scm2double (scm_caadr (expr), 0.0);
      Real x = robust_scm2double (scm_car (scm_cadadr (expr)), 0.0);
      Real y = robust_scm2double (scm_cdr (scm_cadadr (expr)), 0.0);
      pango_matrix_translate (&trans, x, y);
      pango_matrix_rotate (&trans, -ang);
      pango_matrix_translate (&trans, -x, -y);
      return stencil_traverser (trans, scm_caddr (expr));
    }
  else if (scm_car (expr) == ly_symbol2scm ("delay-stencil-evaluation"))
    return stencil_traverser (trans, scm_force (scm_cadr (expr)));
  else if (scm_car (expr) == ly_symbol2scm ("grob-cause"))
    return stencil_traverser (trans, scm_caddr (expr));
  else if (scm_car (expr) == ly_symbol2scm ("color"))
    return stencil_traverser (trans, scm_caddr (expr));
  else if (scm_car (expr) == ly_symbol2scm ("id"))
    return stencil_traverser (trans, scm_caddr (expr));
  else
    {
      vector<Transform_matrix_and_expression> out;
      out.push_back (Transform_matrix_and_expression (trans, expr));
      return out;
    }
  warning ("Stencil expression not supported by the veritcal skylines.");
  return vector<Transform_matrix_and_expression> ();
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

  // In horizontal spacing, there are grobs like SystemStartBracket
  // that take up no vertical spcae.  So, if the y extent is empty,
  // we use the entire Y extent ot make the X a sort of horizontal wall.
  // Ditto for vertical spacing and grobs like BassFigureAlginmentPositioning.
  if (a == Y_AXIS && yex.is_empty ())
    yex.set_full ();

  if (a == X_AXIS && xex.is_empty ())
    xex.set_full ();

  if (xex.is_empty () || yex.is_empty ())
    return Skyline_pair ().smobbed_copy ();

  boxes.push_back (Box (xex, yex));
  return Skyline_pair (boxes, a).smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Grob, pure_simple_vertical_skylines_from_extents, 3);
SCM
Grob::pure_simple_vertical_skylines_from_extents (SCM smob, SCM begscm, SCM endscm)
{
  Grob *me = unsmob_grob (smob);
  int beg = robust_scm2int (begscm, 0);
  int end = robust_scm2int (endscm, INT_MAX);
  return maybe_pure_internal_simple_skylines_from_extents (me, X_AXIS, true, beg, end, dynamic_cast<Spanner *> (me), false);
}

MAKE_SCHEME_CALLBACK (Grob, simple_vertical_skylines_from_extents, 1);
SCM
Grob::simple_vertical_skylines_from_extents (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  return maybe_pure_internal_simple_skylines_from_extents (me, X_AXIS, false, 0, 0, false, false);
}

MAKE_SCHEME_CALLBACK (Grob, pure_simple_horizontal_skylines_from_extents, 3);
SCM
Grob::pure_simple_horizontal_skylines_from_extents (SCM smob, SCM begscm, SCM endscm)
{
  Grob *me = unsmob_grob (smob);
  int beg = robust_scm2int (begscm, 0);
  int end = robust_scm2int (endscm, INT_MAX);
  return maybe_pure_internal_simple_skylines_from_extents (me, Y_AXIS, true, beg, end, false, to_boolean (me->get_property ("cross-staff")));
}

MAKE_SCHEME_CALLBACK (Grob, simple_horizontal_skylines_from_extents, 1);
SCM
Grob::simple_horizontal_skylines_from_extents (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  return maybe_pure_internal_simple_skylines_from_extents (me, Y_AXIS, false, 0, 0, false, to_boolean (me->get_property ("cross-staff")));
}

SCM
Stencil::skylines_from_stencil (SCM sten, Real pad, Axis a)
{
  Stencil *s = unsmob_stencil (sten);
  if (!s)
    return Skyline_pair ().smobbed_copy ();

  vector<Transform_matrix_and_expression> data
    = stencil_traverser (make_transform_matrix (1.0, 0.0, 0.0, 1.0, 0.0, 0.0),
                         s->expr ());
  vector<Box> boxes;
  vector<Drul_array<Offset> > buildings;
  for (vsize i = 0; i < data.size (); i++)
    stencil_dispatcher (boxes, buildings, data[i].tm_, data[i].expr_);

  // we use the bounding box if there are no boxes
  if (!boxes.size () && !buildings.size ())
    boxes.push_back (Box (s->extent (X_AXIS), s->extent (Y_AXIS)));

  Skyline_pair out (boxes, a);
  out.merge (Skyline_pair (buildings, a));

  for (DOWN_and_UP (d))
    out[d] = out[d].padded (pad);

  out.deholify ();
  return out.smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Grob, vertical_skylines_from_stencil, 1);
SCM
Grob::vertical_skylines_from_stencil (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  Real pad = robust_scm2double (me->get_property ("skyline-horizontal-padding"), 0.0);
  SCM out = Stencil::skylines_from_stencil (me->get_property ("stencil"), pad, X_AXIS);

  return out;
}

MAKE_SCHEME_CALLBACK (Grob, horizontal_skylines_from_stencil, 1);
SCM
Grob::horizontal_skylines_from_stencil (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  Real pad = robust_scm2double (me->get_property ("skyline-vertical-padding"), 0.0);
  SCM out = Stencil::skylines_from_stencil (me->get_property ("stencil"), pad, Y_AXIS);

  return out;
}

SCM
Grob::internal_skylines_from_element_stencils (SCM smob, Axis a)
{
  Grob *me = unsmob_grob (smob);

  extract_grob_set (me, "elements", elts);
  vector<Real> x_pos;
  vector<Real> y_pos;
  Grob *x_common = common_refpoint_of_array (elts, me, X_AXIS);
  Grob *y_common = common_refpoint_of_array (elts, me, Y_AXIS);
  for (vsize i = 0; i < elts.size (); i++)
    {
      x_pos.push_back (elts[i]->relative_coordinate (x_common, X_AXIS));
      y_pos.push_back (elts[i]->relative_coordinate (y_common, Y_AXIS));
    }
  Real my_x = me->relative_coordinate (x_common, X_AXIS);
  Real my_y = me->relative_coordinate (y_common, Y_AXIS);
  Skyline_pair res;
  for (vsize i = 0; i < elts.size (); i++)
    {
      Skyline_pair *skyp = Skyline_pair::unsmob (elts[i]->get_property (a == X_AXIS ? "vertical-skylines" : "horizontal-skylines"));
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
  return internal_skylines_from_element_stencils (smob, X_AXIS);
}

MAKE_SCHEME_CALLBACK (Grob, horizontal_skylines_from_element_stencils, 1);
SCM
Grob::horizontal_skylines_from_element_stencils (SCM smob)
{
  return internal_skylines_from_element_stencils (smob, Y_AXIS);
}
