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

// Needed because of extension definitions for POSIX functions.
#include "config.hh"

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
#include "transform.hh"

#include <cmath>

using std::string;
using std::vector;

Real QUANTIZATION_UNIT = 0.2;

void create_path_cap (vector<Box> &boxes,
                      vector<Drul_array<Offset> > &buildings,
                      SCM trans, Offset pt, Real rad, Offset dir);

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

/*
  below, for all of the functions make_X_boxes, the expression
  is always unpacked into variables.
  then, after a line of /////, there are manipulations of these variables
  (there may be no manipulations necessary depending on the function)
  afterwards, there is another ///// followed by the creation of points
  and boxes
*/

void
make_draw_line_boxes (vector<Box> &boxes, vector<Drul_array<Offset> > &buildings, SCM trans, SCM expr, bool use_building)
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

  //////////////////////
  if (x1 < x0)
    {
      std::swap (x0, x1);
      std::swap (y0, y1);
    }
  Offset left (x0, y0);
  Offset right (x1, y1);
  Offset dir = (right - left).direction ();
  for (DOWN_and_UP (d))
    {
      Offset outward = (d * thick / 2) * dir.normal ();
      Offset inter_l = scm_transform (trans, left + outward);
      Offset inter_r = scm_transform (trans, right + outward);
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
          Real length = (inter_l - inter_r).length ();

          vsize passes = (vsize) ((length * 2) + 1);
          vector<Offset> points;

          for (vsize i = 0; i < 1 + passes; i++)
            {
              Offset pt (
                linear_interpolate (static_cast<Real> (i), 0,
                                    static_cast<Real> (passes), x0, x1),
                linear_interpolate (static_cast<Real> (i), 0,
                                    static_cast<Real> (passes), y0, y1));
              Offset inter = scm_transform (trans, pt + outward);
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

  if (thick > 0.0)
    {
      // beg line cap
      create_path_cap (boxes,
                       buildings,
                       trans,
                       left,
                       thick / 2,
                       -dir);

      // end line cap
      create_path_cap (boxes,
                       buildings,
                       trans,
                       right,
                       thick / 2,
                       dir);
    }
}

void
make_partial_ellipse_boxes (vector<Box> &boxes,
                            vector<Drul_array<Offset> > &buildings,
                            SCM trans, SCM expr)
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
  //////////////////////
  if (end == start)
    end += 360;
  Offset sp (offset_directed (start).scale (rad));
  Offset ep (offset_directed (end).scale (rad));
  Transform t = robust_scm2transform (trans);
  Real x_scale = sqrt (sqr (t.get_xx ()) + sqr (t.get_yx ()));
  Real y_scale = sqrt (sqr (t.get_xy ()) + sqr (t.get_yy ()));
  //////////////////////
  Drul_array<vector<Offset> > points;
  int quantization = std::max (1, (int) (((x_rad * x_scale) + (y_rad * y_scale))
                                         * M_PI / QUANTIZATION_UNIT));
  for (DOWN_and_UP (d))
    {
      for (vsize i = 0; i <= (vsize) quantization; i++)
        {
          Real ang = linear_interpolate (static_cast<Real> (i), 0, quantization,
                                         start, end);
          Offset pt (offset_directed (ang).scale (rad));
          Offset inter = t (pt + (d * th / 2) * pt.direction ().normal ());
          points[d].push_back (inter);
        }
    }

  for (vsize i = 0; i < points[DOWN].size () - 1; i++)
    {
      Box b;
      for (DOWN_and_UP (d))
        {
          b.add_point (points[d][i]);
          b.add_point (points[d][i + 1]);
        }
      boxes.push_back (b);
    }

  if (connect || fill)
    {
      make_draw_line_boxes (boxes, buildings, trans,
                            scm_list_5 (scm_from_double (th),
                                        scm_from_double (sp[X_AXIS]),
                                        scm_from_double (sp[Y_AXIS]),
                                        scm_from_double (ep[X_AXIS]),
                                        scm_from_double (ep[Y_AXIS])),
                            false);
    }

  if (th > 0.0)
    {
      // beg line cap
      Offset pt (offset_directed (start).scale (rad));
      create_path_cap (boxes, buildings, trans, pt, th / 2, -pt.normal ());

      // end line cap
      pt = offset_directed (end).scale (rad);
      create_path_cap (boxes, buildings, trans, pt, th / 2, pt.normal ());
    }
}

void
make_round_filled_box_boxes (vector<Box> &boxes,
                             vector<Drul_array<Offset> > &buildings,
                             SCM trans, SCM expr)
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
  //////////////////////
  Transform t = robust_scm2transform (trans);
  Real x_scale = sqrt (sqr (t.get_xx ()) + sqr (t.get_yx ()));
  Real y_scale = sqrt (sqr (t.get_xy ()) + sqr (t.get_yy ()));
  bool rounded = (diameter * std::max (x_scale, y_scale) > 0.5);
  bool rotated = (t.get_yx () || t.get_xy ());
  //////////////////////

  if (!rotated && !rounded)
    {
      /* simple box */
      Box b;
      Offset p0 (-left, -bottom);
      Offset p1 (right, top);
      b.add_point (scm_transform (trans, p0));
      b.add_point (scm_transform (trans, p1));
      boxes.push_back (b);
    }
  else
    {
      int quantization = (int) (rounded * diameter * (x_scale + y_scale)
                                * M_PI / QUANTIZATION_UNIT / 8);
      /* if there is no quantization, there is no need to draw
         rounded corners. >>> Set the effective skyline radius to 0 */
      Real radius = (quantization ? diameter / 2 : 0.);

      /* draw straight lines */
      vector<Offset> points;

      points.push_back (Offset (-left, -bottom + radius));
      points.push_back (Offset (-left, top - radius));
      points.push_back (Offset (-left + radius, -bottom));
      points.push_back (Offset (right - radius, -bottom));
      points.push_back (Offset (right, -bottom + radius));
      points.push_back (Offset (right, top - radius));
      points.push_back (Offset (-left + radius, top));
      points.push_back (Offset (right - radius, top));

      for (vsize i = 0; i < (vsize) points.size () - 1; i += 2)
        {
          Offset p0 = t (points[i]);
          Offset p1 = t (points[i + 1]);
          if (p0[Y_AXIS] == p1[Y_AXIS])
            {
              Box b;
              b.add_point (p0);
              b.add_point (p1);
              boxes.push_back (b);
            }
          else if (p0[X_AXIS] != p1[X_AXIS])
            buildings.push_back (Drul_array<Offset> (p0, p1));
        }

      /* draw rounded corners */
      if (radius)
        {
          vector<Offset> points;
          Offset rad (radius, radius);
          Drul_array<Real> cx;
          Drul_array<Real> cy;

          cx[LEFT] = -left + radius;
          cx[RIGHT] = right - radius;
          cy[DOWN] = -bottom + radius;
          cy[UP] = top - radius;

          for (vsize i = 0; i <= (vsize) quantization; i++)
            for (DOWN_and_UP (v))
              for (LEFT_and_RIGHT (h))
                {
                  Real ang = linear_interpolate (static_cast<Real> (i), 0,
                                                 quantization, 0., 90.);
                  Offset pt (offset_directed (ang).scale (rad));
                  Offset inter (cx[h] + h * pt[X_AXIS],
                                cy[v] + v * pt[Y_AXIS]);
                  points.push_back (t (inter));
                }

          for (vsize i = 0; i < points.size () - 4; i++)
            {
              Box b;
              b.add_point (points[i]);
              b.add_point (points[i + 4]);
              boxes.push_back (b);
            }
        }
    }
}

void
create_path_cap (vector<Box> &boxes,
                 vector<Drul_array<Offset> > &buildings,
                 SCM trans, Offset pt, Real rad, Offset dir)
{
  Real angle = dir.angle_degrees ();
  Transform t (pt);
  t = scm_transform (trans, t);
  SCM new_trans = t.smobbed_copy ();
  make_partial_ellipse_boxes (boxes, buildings, new_trans,
                              scm_list_n (scm_from_double (rad),
                                          scm_from_double (rad),
                                          scm_from_double (angle - 90.01),
                                          scm_from_double (angle + 90.01),
                                          scm_from_double (0.0),
                                          SCM_BOOL_F,
                                          SCM_BOOL_F,
                                          SCM_UNDEFINED));
}

// TODO - remove once https://codereview.appspot.com/583750044/ is in.
Offset
get_normal (Offset o)
{
  return o.normal ();
}

void
make_draw_bezier_boxes (vector<Box> &boxes,
                        vector<Drul_array<Offset> > &buildings,
                        SCM trans, SCM expr)
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
  Offset temp0 (scm_transform (trans, curve.control_[0]));
  Offset temp1 (scm_transform (trans, curve.control_[1]));
  Offset temp2 (scm_transform (trans, curve.control_[2]));
  Offset temp3 (scm_transform (trans, curve.control_[3]));

  //////////////////////
  Drul_array<vector<Offset> > points;
  int quantization = int (((temp1 - temp0).length ()
                           + (temp2 - temp1).length ()
                           + (temp3 - temp2).length ())
                          / QUANTIZATION_UNIT);

  Offset d0;
  Offset d1;

  Offset normal;
  if (th > 0)
    {
      d0 = curve.dir_at_point (0.0);
      normal = get_normal ((th / 2) * d0);
    }

  for (DOWN_and_UP (d))
    {
      if (th == 0.0 && d == UP)
        break;
      points[d].push_back (
        scm_transform (trans, curve.control_[0] + d * normal));
    }

  for (vsize i = 1; i < (vsize) quantization; i++)
    {
      Real pt = static_cast<Real> (i) / quantization;
      Offset norm = get_normal ((th / 2) * curve.dir_at_point (pt));

      for (DOWN_and_UP (d))
        {
          if (th == 0.0 && d == UP)
            break;
          points[d].push_back (
            scm_transform (trans, curve.curve_point (pt) + d * norm));
        }
    }

  if (th > 0)
    {
      d1 = curve.dir_at_point (1.0);
      normal = get_normal ((th / 2) * d1);
    }

  for (DOWN_and_UP (d))
    {
      if (th == 0.0 && d == UP)
        break;
      points[d].push_back (
        scm_transform (trans, curve.control_[3] + d * normal));
    }

  for (vsize i = 0; i < points[DOWN].size () - 1; i++)
    {
      Box b;
      for (DOWN_and_UP (d))
        {
          if (th == 0.0 && d == UP)
            break;
          b.add_point (points[d][i]);
          b.add_point (points[d][i + 1]);
        }
      boxes.push_back (b);
    }

  if (th > 0)
    {
      // beg line cap
      create_path_cap (boxes, buildings, trans, curve.control_[0], th / 2, -d0);

      // end line cap
      create_path_cap (boxes, buildings, trans, curve.control_[3], th / 2, d1);
    }
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
internal_make_path_boxes (vector<Box> &boxes,
                          vector<Drul_array<Offset> > &buildings,
                          SCM trans, SCM expr, bool use_building)
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
make_path_boxes (vector<Box> &boxes,
                 vector<Drul_array<Offset> > &buildings,
                 SCM trans, SCM expr)
{
  return internal_make_path_boxes (boxes, buildings, trans, scm_cons (scm_car (expr), get_path_list (scm_cdr (expr))), false);
}

void
make_polygon_boxes (vector<Box> &boxes,
                    vector<Drul_array<Offset> > &buildings,
                    SCM trans, SCM expr)
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
  internal_make_path_boxes (boxes, buildings, trans,
                            scm_cons (blot_diameter, scm_reverse_x (l, SCM_EOL)), true);
}

void
make_named_glyph_boxes (vector<Box> &boxes,
                        vector<Drul_array<Offset> > &buildings,
                        SCM trans, SCM expr)
{
  SCM fm_scm = scm_car (expr);
  Font_metric *fm = unsmob<Font_metric> (fm_scm);
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
  bbox.scale (dynamic_cast<Modified_font_metric *> (fm)->get_magnification ()
              * open_fm->design_size () / open_fm->get_units_per_EM ());
  // Real bbox is the real bbox of the object
  Box real_bbox = open_fm->get_glyph_outline_bbox (gidx);

  Real scale = bbox[X_AXIS].length () / real_bbox[X_AXIS].length ();

  trans = robust_scm2transform (trans).scale (scale, scale).smobbed_copy ();

  SCM outline = open_fm->get_glyph_outline (gidx);
  //////////////////////
  for (SCM s = outline;
       scm_is_pair (s);
       s = scm_cdr (s))
    {
      scm_to_int (scm_length (scm_car (s))) == 4
      ? make_draw_line_boxes (boxes, buildings, trans,
                              scm_cons (scm_from_double (0), scm_car (s)),
                              false)
      : make_draw_bezier_boxes (boxes, buildings, trans,
                                scm_cons (scm_from_double (0), scm_car (s)));
    }
}

void
make_glyph_string_boxes (vector<Box> &boxes,
                         vector<Drul_array<Offset> > &buildings,
                         SCM trans, SCM expr)
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
      SCM transcopy = trans;
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
      if (std::isnan (xlen) || std::isnan (ylen) || std::isinf (xlen) || std::isinf (ylen))
        outline = box_to_scheme_lines (kerned_bbox);
      else
        {
          assert (abs (xlen - ylen) < 10e-3);

          Real scale_factor = std::max (xlen, ylen);
          // the three operations below move the stencil from its original coordinates to current coordinates
          // FIXME: this looks extremely fishy.
          transcopy
            = robust_scm2transform (transcopy)
              .translate (Offset (kerned_bbox[X_AXIS][LEFT],
                                  kerned_bbox[Y_AXIS][DOWN] - real_bbox[Y_AXIS][DOWN]))
              .translate (Offset (real_bbox[X_AXIS][LEFT], real_bbox[Y_AXIS][DOWN]))
              .scale (scale_factor, scale_factor)
              .translate (-Offset (bbox[X_AXIS][LEFT], bbox[Y_AXIS][DOWN]))
              .smobbed_copy ();
        }
      //////////////////////
      for (SCM s = outline;
           scm_is_pair (s);
           s = scm_cdr (s))
        {
          scm_to_int (scm_length (scm_car (s))) == 4
          ? make_draw_line_boxes (boxes, buildings, transcopy,
                                  scm_cons (scm_from_double (0), scm_car (s)),
                                  false)
          : make_draw_bezier_boxes (boxes, buildings, transcopy,
                                    scm_cons (scm_from_double (0), scm_car (s)));
        }
    }
}

/*
  receives a stencil expression and a transform matrix
  depending on the stencil name, dispatches it to the appropriate function
*/

void
stencil_dispatcher (vector<Box> &boxes,
                    vector<Drul_array<Offset> > &buildings,
                    SCM trans, SCM expr)
{
  if (not scm_is_pair (expr))
    return;

  SCM head = scm_car (expr);
  if (scm_is_eq (head, ly_symbol2scm ("draw-line")))
    make_draw_line_boxes (boxes, buildings, trans, scm_cdr (expr), true);
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
      make_draw_line_boxes (boxes, buildings, trans,
                            scm_list_5 (th, scm_from_double (0.0),
                                        scm_from_double (0.0), x1, x2), true);
    }
  else if (scm_is_eq (head, ly_symbol2scm ("circle")))
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
  else if (scm_is_eq (head, ly_symbol2scm ("ellipse")))
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
  else if (scm_is_eq (head, ly_symbol2scm ("partial-ellipse")))
    make_partial_ellipse_boxes (boxes, buildings, trans, scm_cdr (expr));
  else if (scm_is_eq (head, ly_symbol2scm ("round-filled-box")))
    make_round_filled_box_boxes (boxes, buildings, trans, scm_cdr (expr));
  else if (scm_is_eq (head, ly_symbol2scm ("named-glyph")))
    make_named_glyph_boxes (boxes, buildings, trans, scm_cdr (expr));
  else if (scm_is_eq (head, ly_symbol2scm ("polygon")))
    make_polygon_boxes (boxes, buildings, trans, scm_cdr (expr));
  else if (scm_is_eq (head, ly_symbol2scm ("path")))
    make_path_boxes (boxes, buildings, trans, scm_cdr (expr));
  else if (scm_is_eq (head, ly_symbol2scm ("glyph-string")))
    make_glyph_string_boxes (boxes, buildings, trans, scm_cdr (expr));
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
stencil_traverser (SCM trans, SCM expr, SCM tail)
{
  if (scm_is_null (expr)
      || (scm_is_string (expr) && scm_is_true (scm_string_null_p (expr))))
    return tail;

  SCM head = scm_car (expr);
  if (scm_is_eq (head, ly_symbol2scm ("combine-stencil")))
    {
      for (SCM s = scm_cdr (expr); scm_is_pair (s); s = scm_cdr (s))
        tail = stencil_traverser (trans, scm_car (s), tail);
      return tail;
    }
  else if (scm_is_eq (head, ly_symbol2scm ("footnote")))
    return tail;
  else if (scm_is_eq (head, ly_symbol2scm ("translate-stencil")))
    {
      Offset p = robust_scm2offset (scm_cadr (expr), Offset (0.0, 0.0));
      trans = robust_scm2transform (trans).translate (p).smobbed_copy ();
      return stencil_traverser (trans, scm_caddr (expr), tail);
    }
  else if (scm_is_eq (head, ly_symbol2scm ("scale-stencil")))
    {
      Real x = robust_scm2double (scm_caadr (expr), 0.0);
      Real y = robust_scm2double (scm_cadadr (expr), 0.0);
      trans = robust_scm2transform (trans).scale (x, y).smobbed_copy ();
      return stencil_traverser (trans, scm_caddr (expr), tail);
    }
  else if (scm_is_eq (head, ly_symbol2scm ("rotate-stencil")))
    {
      Real ang = robust_scm2double (scm_caadr (expr), 0.0);
      Offset center = robust_scm2offset (scm_cadadr (expr), Offset (0.0, 0.0));
      trans = robust_scm2transform (trans).rotate (ang, center).smobbed_copy ();
      return stencil_traverser (trans, scm_caddr (expr), tail);
    }
  else if (scm_is_eq (head, ly_symbol2scm ("delay-stencil-evaluation")))
    // should not use the place-holder text, but no need for the warning below
    return tail;
  else if (scm_is_eq (head, ly_symbol2scm ("grob-cause")))
    return stencil_traverser (trans, scm_caddr (expr), tail);
  else if (scm_is_eq (head, ly_symbol2scm ("color")))
    return stencil_traverser (trans, scm_caddr (expr), tail);
  else if (scm_is_eq (head, ly_symbol2scm ("output-attributes")))
    return stencil_traverser (trans, scm_caddr (expr), tail);
  else if (scm_is_eq (head, ly_symbol2scm ("with-outline")))
    return stencil_traverser (trans, scm_cadr (expr), tail);
  else
    {
      return scm_acons (trans, expr, tail);
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
  return maybe_pure_internal_simple_skylines_from_extents (me, Y_AXIS, true, beg, end, false, to_boolean (me->get_property ("cross-staff")));
}

MAKE_SCHEME_CALLBACK (Grob, simple_horizontal_skylines_from_extents, 1);
SCM
Grob::simple_horizontal_skylines_from_extents (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);
  // See comment in function above.
  return maybe_pure_internal_simple_skylines_from_extents (me, Y_AXIS, false, 0, 0, false, to_boolean (me->get_property ("cross-staff")));
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

  SCM data = scm_reverse_x (stencil_traverser (SCM_EOL, s->expr (), SCM_EOL), SCM_EOL);
  vector<Box> boxes;
  vector<Drul_array<Offset> > buildings;
  for (SCM s = scm_reverse_x (data, SCM_EOL); scm_is_pair (s); s = scm_cdr (s))
    stencil_dispatcher (boxes, buildings, scm_caar (s), scm_cdar (s));

  // we use the bounding box if there are no boxes
  // FIXME: Rotation?
  if (!boxes.size () && !buildings.size ())
    boxes.push_back (s->extent_box ());

  scm_remember_upto_here (sten);

  Skyline_pair out (boxes, a);
  out.merge (Skyline_pair (buildings, a));

  for (DOWN_and_UP (d))
    out[d] = out[d].padded (pad);

  return out.smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Grob, vertical_skylines_from_stencil, 1);
SCM
Grob::vertical_skylines_from_stencil (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);
  Real pad = robust_scm2double (me->get_property ("skyline-horizontal-padding"), 0.0);
  SCM rot = me->get_property ("rotation");
  SCM out = Stencil::skylines_from_stencil (me->get_property ("stencil"),
                                            pad, rot, X_AXIS);
  return out;
}

MAKE_SCHEME_CALLBACK (Grob, horizontal_skylines_from_stencil, 1);
SCM
Grob::horizontal_skylines_from_stencil (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);
  Real pad = robust_scm2double (me->get_property ("skyline-vertical-padding"), 0.0);
  SCM rot = me->get_property ("rotation");
  SCM out = Stencil::skylines_from_stencil (me->get_property ("stencil"),
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
      Skyline_pair *skyp = unsmob<Skyline_pair> (elts[i]->get_maybe_pure_property (a == X_AXIS ? "vertical-skylines" : "horizontal-skylines", pure, beg, end));
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
