/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

  Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "lookup.hh"

#include "line-interface.hh"
#include "warn.hh"
#include "international.hh"
#include "dimensions.hh"
#include "bezier.hh"
#include "file-path.hh"
#include "lily-guile.hh"

#include <cctype>
#include <cmath>

using std::vector;

Stencil
Lookup::beam (Real slope, Real width, Real thick, Real blot)
{
  Box b;

  Offset p;

  p = Offset (0, thick / 2);
  b.add_point (p);
  p += Offset (1, -1) * (blot / 2);

  SCM points = SCM_EOL;

  points = scm_cons (to_scm (p[X_AXIS]), scm_cons (to_scm (p[Y_AXIS]), points));

  p = Offset (0, -thick / 2);
  b.add_point (p);
  p += Offset (1, 1) * (blot / 2);

  points = scm_cons (to_scm (p[X_AXIS]), scm_cons (to_scm (p[Y_AXIS]), points));

  p = Offset (width, width * slope - thick / 2);
  b.add_point (p);
  p += Offset (-1, 1) * (blot / 2);

  points = scm_cons (to_scm (p[X_AXIS]), scm_cons (to_scm (p[Y_AXIS]), points));

  p = Offset (width, width * slope + thick / 2);
  b.add_point (p);
  p += Offset (-1, -1) * (blot / 2);

  points = scm_cons (to_scm (p[X_AXIS]), scm_cons (to_scm (p[Y_AXIS]), points));

  SCM expr
    = ly_list (ly_symbol2scm ("polygon"), points, to_scm (blot), SCM_BOOL_T);

  return Stencil (b, expr);
}

Stencil
Lookup::rotated_box (Real slope, Real width, Real thick, Real blot)
{
  vector<Offset> pts;
  Offset rot = Offset (1, slope).direction ();

  pts.push_back (Offset (0, -thick / 2) * rot);
  pts.push_back (Offset (width, -thick / 2) * rot);
  pts.push_back (Offset (width, thick / 2) * rot);
  pts.push_back (Offset (0, thick / 2) * rot);
  return Lookup::round_polygon (pts, blot, -1.0, true);
}

Stencil
Lookup::horizontal_line (Interval w, Real th)
{
  SCM at = ly_list (ly_symbol2scm ("draw-line"), to_scm (th), to_scm (w[LEFT]),
                    to_scm (0), to_scm (w[RIGHT]), to_scm (0));

  Box box;
  box[X_AXIS] = w;
  box[Y_AXIS] = Interval (-th / 2, th / 2);

  return Stencil (box, at);
}

Stencil
Lookup::blank (Box b)
{
  return Stencil (b, scm_string (SCM_EOL));
}

Stencil
Lookup::circle (Real rad, Real thick, bool filled)
{
  Box b (Interval (-rad, rad), Interval (-rad, rad));
  return Stencil (b, ly_list (ly_symbol2scm ("circle"), to_scm (rad),
                              to_scm (thick), to_scm (filled)));
}

Stencil
Lookup::filled_box (Box b)
{
  return round_filled_box (b, 0.0);
}

/*
 * round filled box:
 *
 *   __________________________________
 *  /     \  ^           /     \      ^
 * |         |blot              |     |
 * |       | |dia       |       |     |
 * |         |meter             |     |
 * |\ _ _ /  v           \ _ _ /|     |
 * |                            |     |
 * |                            |     | Box
 * |                    <------>|     | extent
 * |                      blot  |     | (Y_AXIS)
 * |                    diameter|     |
 * |                            |     |
 * |  _ _                  _ _  |     |
 * |/     \              /     \|     |
 * |                            |     |
 * |       |            |       |     |
 * |                            |     |
 * x\_____/______________\_____/|_____v
 * |(0, 0)                       |
 * |                            |
 * |                            |
 * |<-------------------------->|
 *       Box extent (X_AXIS)
 */
Stencil
Lookup::round_filled_box (Box b, Real blotdiameter)
{
  Real width = b.x ().length ();
  blotdiameter = std::min (blotdiameter, width);
  Real height = b.y ().length ();
  blotdiameter = std::min (blotdiameter, height);

  if (blotdiameter < 0.0)
    {
      if (!std::isinf (blotdiameter))
        warning (_f ("Not drawing a box with negative dimension, %.2f by %.2f.",
                     width, height));
      return Stencil (b, SCM_EOL);
    }

  SCM at
    = ly_list (ly_symbol2scm ("round-filled-box"), to_scm (-b[X_AXIS][LEFT]),
               to_scm (b[X_AXIS][RIGHT]), to_scm (-b[Y_AXIS][DOWN]),
               to_scm (b[Y_AXIS][UP]), to_scm (blotdiameter));

  return Stencil (b, at);
}

/*
 * Create Stencil that represents a polygon with round edges.
 *
 * LIMITATIONS:
 *
 * (a) Only outer (convex) edges are rounded.
 *
 * (b) This algorithm works as expected only for polygons whose edges
 * do not intersect.  For example, the polygon ((0, 0), (q, 0), (0,
 * q), (q, q)) has an intersection at point (q/2, q/2) and therefore
 * will give a strange result.  Even non-adjacent edges that just
 * touch each other will in general not work as expected for non-null
 * blotdiameter.
 *
 * (c) Given a polygon ((x0, y0), (x1, y1), ... , (x (n-1), y (n-1))),
 * if there is a natural number k such that blotdiameter is greater
 * than the maximum of { | (x (k mod n), y (k mod n)) - (x ((k+1) mod n),
 * y ((k+1) mod n)) |, | (x (k mod n), y (k mod n)) - (x ((k+2) mod n),
 * y ((k+2) mod n)) |, | (x ((k+1) mod n), y ((k+1) mod n)) - (x ((k+2)
 * mod n), y ((k+2) mod n)) | }, then the outline of the rounded
 * polygon will exceed the outline of the core polygon.  In other
 * words: Do not draw rounded polygons that have a leg smaller or
 * thinner than blotdiameter (or set blotdiameter to a sufficiently
 * small value -- maybe even 0.0)!
 *
 * NOTE: Limitations (b) and (c) arise from the fact that round edges
 * are made by moulding sharp edges to round ones rather than adding
 * to a core polygon.  For details of these two different
 * approaches, see the thread upon the ledger lines patch that started
 * on March 25, 2002 on the devel mailing list.  The below version of
 * round_polygon () sticks to the moulding model, which the
 * majority of the list participants finally voted for.  This,
 * however, results in the above limitations and a much increased
 * complexity of the algorithm, since it has to compute a shrinked
 * polygon -- which is not trivial define precisely and unambigously.
 * With the other approach, one simply could move a circle of size
 * blotdiameter along all edges of the polygon (which is what the
 * postscript routine in the backend effectively does, but on the
 * shrinked polygon). --jr
 *
 * An extra parameter "extroversion" has been added since staying just
 * inside of a polygon will reduce its visual size when tracing a
 * rounded path.  If extroversion is zero, the polygon is just traced
 * as-is.  If it is -1 (the default) the drawing will stay just within
 * the given polygon.  If it is 1, the traced line will stay just
 * outside of the given polygon.
 */
Stencil
Lookup::round_polygon (vector<Offset> const &points, Real blotdiameter,
                       Real extroversion, bool filled)
{
  /* TODO: Maybe print a warning if one of the above limitations
     applies to the given polygon.  However, this is quite complicated
     to check. */

#ifdef DEBUG
  const Real epsilon = 0.01;

  /* remove consecutive duplicate points */
  for (vsize i = 0; i < points.size (); i++)
    {
      const auto next = (i + 1) % points.size ();
      Real d = (points[i] - points[next]).length ();
      if (d < epsilon)
        programming_error ("Polygon should not have duplicate points");
    }
#endif

  /* special cases: degenerated polygons */
  if (points.size () == 0)
    return Stencil ();
  if (points.size () == 1)
    {
      Stencil circ
        = circle (0.5 * (1.0 + extroversion) * blotdiameter, 0, true);
      circ.translate (points[0]);
      return circ;
    }
  if (points.size () == 2)
    return Line_interface::make_line ((1.0 + extroversion) * blotdiameter,
                                      points[0], points[1]);

  vector<Offset> shrunk_points;

  if (extroversion == 0.0)
    {
      shrunk_points = points;
    }
  else
    {
      /* shrink polygon in size by 0.5 * blotdiameter */

      // first we need to determine the orientation of the polygon in
      // order to decide whether shrinking means moving the polygon to the
      // left or to the right of the outline.  We do that by calculating
      // (double) the oriented area of the polygon.  We first determine the
      // center and do the area calculations relative to it.
      // Mathematically, the result is not affected by this shift, but
      // numerically a lot of cancellation is going on and this keeps its
      // effects in check.

      Offset center;
      for (vsize i = 0; i < points.size (); i++)
        center += points[i];
      center /= static_cast<Real> (points.size ());

      Real area = 0.0;
      Offset last = points.back () - center;

      for (vsize i = 0; i < points.size (); i++)
        {
          Offset here = points[i] - center;
          area += cross_product (last, here);
          last = here;
        }

      bool ccw
        = area >= 0.0; // true if whole shape is counterclockwise oriented

      shrunk_points.resize (points.size ());

      for (vsize i = 0; i < points.size (); i++)
        {
          vsize i0 = i;
          vsize i1 = (i + 1) % points.size ();
          vsize i2 = (i + 2) % points.size ();
          Offset p0 = points[i0];
          Offset p1 = points[i1];
          Offset p2 = points[i2];
          Offset p01 = p1 - p0;
          Offset p12 = p2 - p1;
          Offset inward0 = Offset (-p01[Y_AXIS], p01[X_AXIS]).direction ();
          Offset inward2 = Offset (-p12[Y_AXIS], p12[X_AXIS]).direction ();

          if (!ccw)
            {
              inward0 = -inward0;
              inward2 = -inward2;
            }

          Offset middle = 0.5 * (inward0 + inward2);

          // "middle" now is a vector in the right direction for the
          // shrinkage.  Its size needs to be large enough that the
          // projection on either of the inward vectors has a size of 1.

          Real proj = dot_product (middle, inward0);

          // What's the size of proj?  Assuming that we have a corner
          // angle of phi where 0 corresponds to a continuing line, the
          // length of middle is 0.5 |(1+cos phi, sin phi)| = cos (phi/2),
          // so its projection has length
          // cos^2 (phi/2) = 0.5 + 0.5 cos (phi).
          // We don't really want to move inwards more than 3 blob
          // diameters corresponding to 6 blob radii.  So
          // cos (phi/2) = 1/6 gives phi ~ 161, meaning that a 20 degree
          // corner necessitates moving 3 blob diameters from the corner
          // in order to stay inside the lines.  Ruler and circle agree.
          // 0.03 is close enough to 1/36.  Basically we want to keep the
          // shape from inverting from pulling too far inward.
          // 3 diameters is pretty much a handwaving guess.

          if (abs (proj) < 0.03)
            proj = proj < 0 ? -0.03 : 0.03;

          shrunk_points[i1]
            = p1 - (0.5 * blotdiameter / proj) * middle * extroversion;
        }
    }

  /* build scm expression and bounding box */
  SCM shrunk_points_scm = SCM_EOL;
  Box box;
  Box shrunk_box;
  for (vsize i = 0; i < shrunk_points.size (); i++)
    {
      SCM x = to_scm (shrunk_points[i][X_AXIS]);
      SCM y = to_scm (shrunk_points[i][Y_AXIS]);
      shrunk_points_scm = scm_cons (x, scm_cons (y, shrunk_points_scm));
      box.add_point (points[i]);
      shrunk_box.add_point (shrunk_points[i]);
    }
  shrunk_box.widen (0.5 * blotdiameter, 0.5 * blotdiameter);
  box.unite (shrunk_box);
  SCM polygon_scm = ly_list (ly_symbol2scm ("polygon"), shrunk_points_scm,
                             to_scm (blotdiameter), to_scm (filled));

  Stencil polygon = Stencil (box, polygon_scm);
  return polygon;
}

/*
  TODO: deprecate?
*/
Stencil
Lookup::frame (Box b, Real thick, Real blot)
{
  Stencil m;
  for (const auto a : {X_AXIS, Y_AXIS})
    {
      const auto o = other_axis (a);
      for (const auto d : {LEFT, RIGHT})
        {
          Box edges;
          edges[a] = b[a][d] + 0.5 * thick * Interval (-1, 1);
          edges[o][DOWN] = b[o][DOWN] - thick / 2;
          edges[o][UP] = b[o][UP] + thick / 2;

          m.add_stencil (round_filled_box (edges, blot));
        }
    }
  return m;
}

/*
  Make a smooth curve along the points
*/
Stencil
Lookup::slur (Bezier curve, Real curvethick, Real linethick, SCM dash_details)
{
  Stencil return_value;

  /*
      calculate the offset for the two beziers that make the sandwich
      for the slur
  */
  Offset dir = (curve.control_[3] - curve.control_[0]).direction ();
  Bezier back = curve;
  Offset perp = 0.5 * curvethick * Offset (-dir[Y_AXIS], dir[X_AXIS]);
  back.control_[1] += perp;
  back.control_[2] += perp;

  curve.control_[1] -= perp;
  curve.control_[2] -= perp;

  if (!scm_is_pair (dash_details))
    {
      /* solid slur  */
      return_value = bezier_sandwich (back, curve, linethick);
    }
  else
    {
      /* dashed or combination slur */
      int num_segments = from_scm<int> (scm_length (dash_details));
      for (int i = 0; i < num_segments; i++)
        {
          SCM dash_pattern = scm_list_ref (dash_details, to_scm (i));
          Real t_min = from_scm<double> (scm_car (dash_pattern), 0);
          Real t_max = from_scm<double> (scm_cadr (dash_pattern), 1.0);
          Real dash_fraction = from_scm<double> (scm_caddr (dash_pattern), 1.0);
          Real dash_period = from_scm<double> (scm_cadddr (dash_pattern), 0.75);
          Bezier back_segment = back.extract (t_min, t_max);
          Bezier curve_segment = curve.extract (t_min, t_max);
          if (dash_fraction == 1.0)
            return_value.add_stencil (
              bezier_sandwich (back_segment, curve_segment, linethick));
          else
            {
              Bezier back_dash, curve_dash;
              Real seg_length
                = (back_segment.control_[3] - back_segment.control_[0])
                    .length ();
              const auto pattern_count
                = static_cast<int> (seg_length / dash_period);
              Real pattern_length = 1.0 / (pattern_count + dash_fraction);
              Real start_t, end_t;
              for (int p = 0; p <= pattern_count; p++)
                {
                  start_t = p * pattern_length;
                  end_t = (p + dash_fraction) * pattern_length;
                  back_dash = back_segment.extract (start_t, end_t);
                  curve_dash = curve_segment.extract (start_t, end_t);
                  return_value.add_stencil (
                    bezier_sandwich (back_dash, curve_dash, linethick));
                }
            }
        }
    }
  return return_value;
}

/*
 * Bezier Sandwich:
 *
 *                               .|
 *                        .       |
 *              top .             |
 *              . curve           |
 *          .                     |
 *       .                        |
 *     .                          |
 *    |                           |
 *    |                          .|
 *    |                     .
 *    |         bottom .
 *    |            . curve
 *    |         .
 *    |      .
 *    |   .
 *    | .
 *    |.
 *    |
 *
 */
Stencil
Lookup::bezier_sandwich (Bezier top_curve, Bezier bottom_curve, Real thickness)
{
  SCM commands = ly_list (
    ly_symbol2scm ("moveto"), to_scm (top_curve.control_[0][X_AXIS]),
    to_scm (top_curve.control_[0][Y_AXIS]), ly_symbol2scm ("curveto"),
    to_scm (top_curve.control_[1][X_AXIS]),
    to_scm (top_curve.control_[1][Y_AXIS]),
    to_scm (top_curve.control_[2][X_AXIS]),
    to_scm (top_curve.control_[2][Y_AXIS]),
    to_scm (top_curve.control_[3][X_AXIS]),
    to_scm (top_curve.control_[3][Y_AXIS]), ly_symbol2scm ("lineto"),
    to_scm (bottom_curve.control_[3][X_AXIS]),
    to_scm (bottom_curve.control_[3][Y_AXIS]), ly_symbol2scm ("curveto"),
    to_scm (bottom_curve.control_[2][X_AXIS]),
    to_scm (bottom_curve.control_[2][Y_AXIS]),
    to_scm (bottom_curve.control_[1][X_AXIS]),
    to_scm (bottom_curve.control_[1][Y_AXIS]),
    to_scm (bottom_curve.control_[0][X_AXIS]),
    to_scm (bottom_curve.control_[0][Y_AXIS]), ly_symbol2scm ("closepath"));

  SCM horizontal_bend
    = ly_list (ly_symbol2scm ("path"), to_scm (thickness), commands,
               ly_symbol2scm ("round"), ly_symbol2scm ("round"), SCM_BOOL_T);

  Interval x_extent = top_curve.extent (X_AXIS);
  x_extent.unite (bottom_curve.extent (X_AXIS));
  Interval y_extent = top_curve.extent (Y_AXIS);
  y_extent.unite (bottom_curve.extent (Y_AXIS));
  Box b (x_extent, y_extent);

  b.widen (0.5 * thickness, 0.5 * thickness);
  return Stencil (b, horizontal_bend);
}

Stencil
Lookup::repeat_slash (Real w, Real s, Real t)
{

  Real x_width = hypot (t, t / s);
  Real height = w * s;

  SCM controls = ly_list (
    ly_symbol2scm ("moveto"), to_scm (0), to_scm (0), ly_symbol2scm ("rlineto"),
    to_scm (x_width), to_scm (0), ly_symbol2scm ("rlineto"), to_scm (w),
    to_scm (height), ly_symbol2scm ("rlineto"), to_scm (-x_width), to_scm (0),
    ly_symbol2scm ("closepath"));

  SCM slashnodot
    = ly_list (ly_symbol2scm ("path"), to_scm (0), controls,
               ly_symbol2scm ("round"), ly_symbol2scm ("round"), SCM_BOOL_T);

  Box b (Interval (0, w + x_width), Interval (0, height));

  return Stencil (b, slashnodot); //  http://slashnodot.org
}

Stencil
Lookup::bracket (Axis a, Interval iv, Real thick, Real protrude, Real blot)
{
  Box b;
  Axis other = other_axis (a);
  b[a] = iv;
  b[other] = Interval (-1, 1) * thick * 0.5;

  Stencil m = round_filled_box (b, blot);

  b[a] = Interval (iv[UP] - thick, iv[UP]);
  Interval oi = Interval (-thick / 2, thick / 2 + fabs (protrude));
  oi *= sign (protrude);
  b[other] = oi;
  m.add_stencil (round_filled_box (b, blot));
  b[a] = Interval (iv[DOWN], iv[DOWN] + thick);
  m.add_stencil (round_filled_box (b, blot));

  return m;
}

Stencil
Lookup::triangle (Interval iv, Real thick, Real protrude)
{
  Box b;
  b[X_AXIS] = Interval (0, iv.length ());
  b[Y_AXIS] = Interval (std::min (0., protrude), std::max (0.0, protrude));

  vector<Offset> points;
  points.push_back (Offset (iv[LEFT], 0));
  points.push_back (Offset (iv[RIGHT], 0));
  points.push_back (Offset (iv.center (), protrude));
  points.push_back (Offset (iv[LEFT], 0)); // close triangle

  return points_to_line_stencil (thick, points);
}

Stencil
Lookup::points_to_line_stencil (Real thick, vector<Offset> const &points)
{
  Stencil ret;
  for (vsize i = 1; i < points.size (); i++)
    {
      if (points[i - 1].is_sane () && points[i].is_sane ())
        {
          Stencil line
            = Line_interface::make_line (thick, points[i - 1], points[i]);
          ret.add_stencil (line);
        }
    }
  return ret;
}
