/*
  lookup.cc -- implement simple Lookup methods.

  source file of the GNU LilyPond music typesetter

  (c) 1997--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>

  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "lookup.hh"

#include <cmath>
#include <cctype>
using namespace std;

#include "line-interface.hh"
#include "warn.hh"
#include "dimensions.hh"
#include "bezier.hh"
#include "string-convert.hh"
#include "file-path.hh"
#include "main.hh"
#include "lily-guile.hh"
#include "font-metric.hh"

Stencil
Lookup::dot (Offset p, Real radius)
{
  SCM at = (scm_list_n (ly_symbol2scm ("dot"),
			scm_from_double (p[X_AXIS]),
			scm_from_double (p[Y_AXIS]),
			scm_from_double (radius),
			SCM_UNDEFINED));
  Box box;
  box.add_point (p - Offset (radius, radius));
  box.add_point (p + Offset (radius, radius));
  return Stencil (box, at);
}

Stencil
Lookup::beam (Real slope, Real width, Real thick, Real blot)
{
  Box b;

  Offset p;

  p = Offset (0, thick / 2);
  b.add_point (p);
  p += Offset (1, -1) * (blot / 2);

  SCM points = SCM_EOL;

  points = scm_cons (scm_from_double (p[X_AXIS]),
		     scm_cons (scm_from_double (p[Y_AXIS]),
			       points));

  p = Offset (0, -thick / 2);
  b.add_point (p);
  p += Offset (1, 1) * (blot / 2);

  points = scm_cons (scm_from_double (p[X_AXIS]),
		     scm_cons (scm_from_double (p[Y_AXIS]),
			       points));

  p = Offset (width, width * slope - thick / 2);
  b.add_point (p);
  p += Offset (-1, 1) * (blot / 2);

  points = scm_cons (scm_from_double (p[X_AXIS]),
		     scm_cons (scm_from_double (p[Y_AXIS]),
			       points));

  p = Offset (width, width * slope + thick / 2);
  b.add_point (p);
  p += Offset (-1, -1) * (blot / 2);

  points = scm_cons (scm_from_double (p[X_AXIS]),
		     scm_cons (scm_from_double (p[Y_AXIS]),
			       points));

  SCM expr = scm_list_n (ly_symbol2scm ("polygon"),
			 ly_quote_scm (points),
			 scm_from_double (blot),
			 SCM_BOOL_T,
			 SCM_UNDEFINED);

  return Stencil (b, expr);
}

Stencil
Lookup::dashed_slur (Bezier b, Real thick, Real dash_period, Real dash_fraction)
{
  SCM l = SCM_EOL;

  Real on = dash_fraction * dash_period;
  Real off = dash_period - on;

  for (int i = 4; i--;)
    l = scm_cons (ly_offset2scm (b.control_[i]), l);

  SCM at = (scm_list_n (ly_symbol2scm ("dashed-slur"),
			scm_from_double (thick),
			scm_from_double (on),
			scm_from_double (off),
			ly_quote_scm (l),
			SCM_UNDEFINED));

  Box box (b.extent (X_AXIS), b.extent (Y_AXIS));
  return Stencil (box, at);
}

Stencil
Lookup::rotated_box (Real slope, Real width, Real thick, Real blot)
{
  vector<Offset> pts;
  Offset rot (1, slope);

  thick -= 2*blot;
  width -= 2*blot;
  rot /= sqrt (1 + slope*slope);
  pts.push_back (Offset (0, -thick / 2) * rot);
  pts.push_back (Offset (width, -thick / 2) * rot);
  pts.push_back (Offset (width, thick / 2) * rot);
  pts.push_back (Offset (0, thick / 2) * rot);
  return Lookup::round_filled_polygon (pts, blot);
}

Stencil
Lookup::horizontal_line (Interval w, Real th)
{
  SCM at = scm_list_n (ly_symbol2scm ("draw-line"),
		       scm_from_double (th),
		       scm_from_double (w[LEFT]),
		       scm_from_double (0),
		       scm_from_double (w[RIGHT]),
		       scm_from_double (0),
		       SCM_UNDEFINED);

  Box box;
  box[X_AXIS] = w;
  box[Y_AXIS] = Interval (-th / 2, th / 2);

  return Stencil (box, at);
}

Stencil
Lookup::blank (Box b)
{
  return Stencil (b, scm_makfrom0str (""));
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
  if (b.x ().length () < blotdiameter)
    blotdiameter = b.x ().length ();
  if (b.y ().length () < blotdiameter)
    blotdiameter = b.y ().length ();

  SCM at = (scm_list_n (ly_symbol2scm ("round-filled-box"),
			scm_from_double (-b[X_AXIS][LEFT]),
			scm_from_double (b[X_AXIS][RIGHT]),
			scm_from_double (-b[Y_AXIS][DOWN]),
			scm_from_double (b[Y_AXIS][UP]),
			scm_from_double (blotdiameter),
			SCM_UNDEFINED));

  return Stencil (b, at);
}

/*
 * Create Stencil that represents a filled polygon with round edges.
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
 * to a core filled polygon.  For details of these two different
 * approaches, see the thread upon the ledger lines patch that started
 * on March 25, 2002 on the devel mailing list.  The below version of
 * round_filled_polygon () sticks to the moulding model, which the
 * majority of the list participants finally voted for.  This,
 * however, results in the above limitations and a much increased
 * complexity of the algorithm, since it has to compute a shrinked
 * polygon -- which is not trivial define precisely and unambigously.
 * With the other approach, one simply could move a circle of size
 * blotdiameter along all edges of the polygon (which is what the
 * postscript routine in the backend effectively does, but on the
 * shrinked polygon). --jr
 */
Stencil
Lookup::round_filled_polygon (vector<Offset> const &points,
			      Real blotdiameter)
{
  /* TODO: Maybe print a warning if one of the above limitations
     applies to the given polygon.  However, this is quite complicated
     to check. */

  const Real epsilon = 0.01;

#ifndef NDEBUG
  /* remove consecutive duplicate points */
  for (vsize i = 0; i < points.size (); i++)
    {
      int next_i = (i + 1) % points.size ();
      Real d = (points[i] - points[next_i]).length ();
      if (d < epsilon)
	programming_error ("Polygon should not have duplicate points");
    }
#endif

  /* special cases: degenerated polygons */
  if (points.size () == 0)
    return Stencil ();
  if (points.size () == 1)
    return dot (points[0], 0.5 * blotdiameter);
  if (points.size () == 2)
    return Line_interface::make_line (blotdiameter, points[0], points[1]);

  /* shrink polygon in size by 0.5 * blotdiameter */
  vector<Offset> shrunk_points;
  shrunk_points.resize (points.size ());
  bool ccw = 1; // true, if three adjacent points are counterclockwise ordered
  for (vsize i = 0; i < points.size (); i++)
    {
      int i0 = i;
      int i1 = (i + 1) % points.size ();
      int i2 = (i + 2) % points.size ();
      Offset p0 = points[i0];
      Offset p1 = points[i1];
      Offset p2 = points[i2];
      Offset p10 = p0 - p1;
      Offset p12 = p2 - p1;
      if (p10.length () != 0.0)
	{ // recompute ccw
	  Real phi = p10.arg ();
	  // rotate (p2 - p0) by (-phi)
	  Offset q = complex_multiply (p2 - p0, complex_exp (Offset (1.0, -phi)));

	  if (q[Y_AXIS] > 0)
	    ccw = 1;
	  else if (q[Y_AXIS] < 0)
	    ccw = 0;
	  else {} // keep ccw unchanged
	}
      else {} // keep ccw unchanged
      Offset p10n = (1.0 / p10.length ()) * p10; // normalize length to 1.0
      Offset p12n = (1.0 / p12.length ()) * p12;
      Offset p13n = 0.5 * (p10n + p12n);
      Offset p14n = 0.5 * (p10n - p12n);
      Offset p13;
      Real d = p13n.length () * p14n.length (); // distance p3n to line (p1..p0)
      if (d < epsilon)
	// special case: p0, p1, p2 are on a single line => build
	// vector orthogonal to (p2-p0) of length 0.5 blotdiameter
	{
	  p13[X_AXIS] = p10[Y_AXIS];
	  p13[Y_AXIS] = -p10[X_AXIS];
	  p13 = (0.5 * blotdiameter / p13.length ()) * p13;
	}
      else
	p13 = (0.5 * blotdiameter / d) * p13n;
      shrunk_points[i1] = p1 + ((ccw) ? p13 : -p13);
    }

  /* build scm expression and bounding box */
  SCM shrunk_points_scm = SCM_EOL;
  Box box;
  for (vsize i = 0; i < shrunk_points.size (); i++)
    {
      SCM x = scm_from_double (shrunk_points[i][X_AXIS]);
      SCM y = scm_from_double (shrunk_points[i][Y_AXIS]);
      shrunk_points_scm = scm_cons (x, scm_cons (y, shrunk_points_scm));
      box.add_point (points[i]);
    }
  SCM polygon_scm = scm_list_n (ly_symbol2scm ("polygon"),
				ly_quote_scm (shrunk_points_scm),
				scm_from_double (blotdiameter),
				SCM_BOOL_T,
				SCM_UNDEFINED);

  Stencil polygon = Stencil (box, polygon_scm);
  shrunk_points.clear ();
  return polygon;
}

/*
  TODO: deprecate?
*/
Stencil
Lookup::frame (Box b, Real thick, Real blot)
{
  Stencil m;
  Direction d = LEFT;
  for (Axis a = X_AXIS; a < NO_AXES; a = Axis (a + 1))
    {
      Axis o = Axis ((a + 1)%NO_AXES);
      do
	{
	  Box edges;
	  edges[a] = b[a][d] + 0.5 * thick * Interval (-1, 1);
	  edges[o][DOWN] = b[o][DOWN] - thick / 2;
	  edges[o][UP] = b[o][UP] + thick / 2;

	  m.add_stencil (round_filled_box (edges, blot));
	}
      while (flip (&d) != LEFT);
    }
  return m;
}

/*
  Make a smooth curve along the points
*/
Stencil
Lookup::slur (Bezier curve, Real curvethick, Real linethick)
{
  Real alpha = (curve.control_[3] - curve.control_[0]).arg ();
  Bezier back = curve;
  Offset perp = curvethick * complex_exp (Offset (0, alpha + M_PI / 2)) * 0.5;
  back.reverse ();
  back.control_[1] += perp;
  back.control_[2] += perp;

  curve.control_[1] -= perp;
  curve.control_[2] -= perp;

  SCM scontrols[8];

  for (int i = 0; i < 4; i++)
    scontrols[i] = ly_offset2scm (back.control_[i]);
  for (int i = 0; i < 4; i++)
    scontrols[i + 4] = ly_offset2scm (curve.control_[i]);

  /*
    Need the weird order b.o. the way PS want its arguments
  */
  int indices[] = {5, 6, 7, 4, 1, 2, 3, 0};
  SCM list = SCM_EOL;
  for (int i = 8; i--;)
    list = scm_cons (scontrols[indices[i]], list);

  SCM at = (scm_list_n (ly_symbol2scm ("bezier-sandwich"),
			ly_quote_scm (list),
			scm_from_double (linethick),
			SCM_UNDEFINED));
  Box b (curve.extent (X_AXIS),
	 curve.extent (Y_AXIS));

  b[X_AXIS].unite (back.extent (X_AXIS));
  b[Y_AXIS].unite (back.extent (Y_AXIS));

  return Stencil (b, at);
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
Lookup::bezier_sandwich (Bezier top_curve, Bezier bottom_curve)
{
  /*
    Need the weird order b.o. the way PS want its arguments
  */
  SCM list = SCM_EOL;
  list = scm_cons (ly_offset2scm (bottom_curve.control_[3]), list);
  list = scm_cons (ly_offset2scm (bottom_curve.control_[0]), list);
  list = scm_cons (ly_offset2scm (bottom_curve.control_[1]), list);
  list = scm_cons (ly_offset2scm (bottom_curve.control_[2]), list);
  list = scm_cons (ly_offset2scm (top_curve.control_[0]), list);
  list = scm_cons (ly_offset2scm (top_curve.control_[3]), list);
  list = scm_cons (ly_offset2scm (top_curve.control_[2]), list);
  list = scm_cons (ly_offset2scm (top_curve.control_[1]), list);

  SCM horizontal_bend = scm_list_n (ly_symbol2scm ("bezier-sandwich"),
				    ly_quote_scm (list),
				    scm_from_double (0.0),
				    SCM_UNDEFINED);

  Interval x_extent = top_curve.extent (X_AXIS);
  x_extent.unite (bottom_curve.extent (X_AXIS));
  Interval y_extent = top_curve.extent (Y_AXIS);
  y_extent.unite (bottom_curve.extent (Y_AXIS));
  Box b (x_extent, y_extent);

  return Stencil (b, horizontal_bend);
}

/*
  TODO: junk me.
*/
Stencil
Lookup::accordion (SCM s, Real staff_space, Font_metric *fm)
{
  Stencil m;
  string sym = ly_scm2string (scm_car (s));
  string reg = ly_scm2string (scm_car (scm_cdr (s)));

  if (sym == "Discant")
    {
      Stencil r = fm->find_by_name ("accordion.accDiscant");
      m.add_stencil (r);
      if (reg.substr (0, 1) == "F")
	{
	  Stencil d = fm->find_by_name ("accordion.accDot");
	  d.translate_axis (staff_space * 2.5 PT, Y_AXIS);
	  m.add_stencil (d);
	  reg = reg.substr (1);
	}
      int eflag = 0x00;
      if (reg.substr (0, 3) == "EEE")
	{
	  eflag = 0x07;
	  reg = reg.substr (3);
	}
      else if (reg.substr (0, 2) == "EE")
	{
	  eflag = 0x05;
	  reg = reg.substr (2);
	}
      else if (reg.substr (0, 2) == "Eh")
	{
	  eflag = 0x04;
	  reg = reg.substr (2);
	}
      else if (reg.substr (0, 1) == "E")
	{
	  eflag = 0x02;
	  reg = reg.substr (1);
	}
      if (eflag & 0x02)
	{
	  Stencil d = fm->find_by_name ("accordion.accDot");
	  d.translate_axis (staff_space * 1.5 PT, Y_AXIS);
	  m.add_stencil (d);
	}
      if (eflag & 0x04)
	{
	  Stencil d = fm->find_by_name ("accordion.accDot");
	  d.translate_axis (staff_space * 1.5 PT, Y_AXIS);
	  d.translate_axis (0.8 * staff_space PT, X_AXIS);
	  m.add_stencil (d);
	}
      if (eflag & 0x01)
	{
	  Stencil d = fm->find_by_name ("accordion.accDot");
	  d.translate_axis (staff_space * 1.5 PT, Y_AXIS);
	  d.translate_axis (-0.8 * staff_space PT, X_AXIS);
	  m.add_stencil (d);
	}
      if (reg.substr (0, 2) == "SS")
	{
	  Stencil d = fm->find_by_name ("accordion.accDot");
	  d.translate_axis (0.5 * staff_space PT, Y_AXIS);
	  d.translate_axis (0.4 * staff_space PT, X_AXIS);
	  m.add_stencil (d);
	  d.translate_axis (-0.8 * staff_space PT, X_AXIS);
	  m.add_stencil (d);
	  reg = reg.substr (2);
	}
      if (reg.substr (0, 1) == "S")
	{
	  Stencil d = fm->find_by_name ("accordion.accDot");
	  d.translate_axis (0.5 * staff_space PT, Y_AXIS);
	  m.add_stencil (d);
	  reg = reg.substr (1);
	}
    }
  else if (sym == "Freebase")
    {
      Stencil r = fm->find_by_name ("accordion.accFreebase");
      m.add_stencil (r);
      if (reg.substr (0, 1) == "F")
	{
	  Stencil d = fm->find_by_name ("accordion.accDot");
	  d.translate_axis (staff_space * 1.5 PT, Y_AXIS);
	  m.add_stencil (d);
	  reg = reg.substr (1);
	}
      if (reg == "E")
	{
	  Stencil d = fm->find_by_name ("accordion.accDot");
	  d.translate_axis (staff_space * 0.5 PT, Y_AXIS);
	  m.add_stencil (d);
	}
    }
  else if (sym == "Bayanbase")
    {
      Stencil r = fm->find_by_name ("accordion.accBayanbase");
      m.add_stencil (r);
      if (reg.substr (0, 1) == "T")
	{
	  Stencil d = fm->find_by_name ("accordion.accDot");
	  d.translate_axis (staff_space * 2.5 PT, Y_AXIS);
	  m.add_stencil (d);
	  reg = reg.substr (1);
	}
      /* include 4' reed just for completeness. You don't want to use this. */
      if (reg.substr (0, 1) == "F")
	{
	  Stencil d = fm->find_by_name ("accordion.accDot");
	  d.translate_axis (staff_space * 1.5 PT, Y_AXIS);
	  m.add_stencil (d);
	  reg = reg.substr (1);
	}
      if (reg.substr (0, 2) == "EE")
	{
	  Stencil d = fm->find_by_name ("accordion.accDot");
	  d.translate_axis (staff_space * 0.5 PT, Y_AXIS);
	  d.translate_axis (0.4 * staff_space PT, X_AXIS);
	  m.add_stencil (d);
	  d.translate_axis (-0.8 * staff_space PT, X_AXIS);
	  m.add_stencil (d);
	  reg = reg.substr (2);
	}
      if (reg.substr (0, 1) == "E")
	{
	  Stencil d = fm->find_by_name ("accordion.accDot");
	  d.translate_axis (staff_space * 0.5 PT, Y_AXIS);
	  m.add_stencil (d);
	  reg = reg.substr (1);
	}
    }
  else if (sym == "Stdbase")
    {
      Stencil r = fm->find_by_name ("accordion.accStdbase");
      m.add_stencil (r);
      if (reg.substr (0, 1) == "T")
	{
	  Stencil d = fm->find_by_name ("accordion.accDot");
	  d.translate_axis (staff_space * 3.5 PT, Y_AXIS);
	  m.add_stencil (d);
	  reg = reg.substr (1);
	}
      if (reg.substr (0, 1) == "F")
	{
	  Stencil d = fm->find_by_name ("accordion.accDot");
	  d.translate_axis (staff_space * 2.5 PT, Y_AXIS);
	  m.add_stencil (d);
	  reg = reg.substr (1);
	}
      if (reg.substr (0, 1) == "M")
	{
	  Stencil d = fm->find_by_name ("accordion.accDot");
	  d.translate_axis (staff_space * 2 PT, Y_AXIS);
	  d.translate_axis (staff_space PT, X_AXIS);
	  m.add_stencil (d);
	  reg = reg.substr (1);
	}
      if (reg.substr (0, 1) == "E")
	{
	  Stencil d = fm->find_by_name ("accordion.accDot");
	  d.translate_axis (staff_space * 1.5 PT, Y_AXIS);
	  m.add_stencil (d);
	  reg = reg.substr (1);
	}
      if (reg.substr (0, 1) == "S")
	{
	  Stencil d = fm->find_by_name ("accordion.accDot");
	  d.translate_axis (staff_space * 0.5 PT, Y_AXIS);
	  m.add_stencil (d);
	  reg = reg.substr (1);
	}
    }
  /* ugh maybe try to use regular font for S.B. and B.B and only use one font
     for the rectangle */
  else if (sym == "SB")
    {
      Stencil r = fm->find_by_name ("accordion.accSB");
      m.add_stencil (r);
    }
  else if (sym == "BB")
    {
      Stencil r = fm->find_by_name ("accordion.accBB");
      m.add_stencil (r);
    }
  else if (sym == "OldEE")
    {
      Stencil r = fm->find_by_name ("accordion.accOldEE");
      m.add_stencil (r);
    }
  else if (sym == "OldEES")
    {
      Stencil r = fm->find_by_name ("accordion.accOldEES");
      m.add_stencil (r);
    }
  return m;
}

Stencil
Lookup::repeat_slash (Real w, Real s, Real t)
{
#if 0 /*  TODO */
  vector<Offset> points;
  Real blotdiameter = 0.0;

  Offset p1 (0, 0);
  Offset p2 (w, w * s);

  return Lookup::round_filled_polygon (points, blotdiameter);
#endif

  SCM wid = scm_from_double (w);
  SCM sl = scm_from_double (s);
  SCM thick = scm_from_double (t);
  SCM slashnodot = scm_list_n (ly_symbol2scm ("repeat-slash"),
			       wid, sl, thick, SCM_UNDEFINED);

  Box b (Interval (0, w + sqrt (sqr (t / s) + sqr (t))),
	 Interval (0, w * s));

  return Stencil (b, slashnodot); //  http://slashnodot.org
}

Stencil
Lookup::bracket (Axis a, Interval iv, Real thick, Real protude, Real blot)
{
  Box b;
  Axis other = Axis ((a + 1)%2);
  b[a] = iv;
  b[other] = Interval (-1, 1) * thick * 0.5;

  Stencil m = round_filled_box (b, blot);

  b[a] = Interval (iv[UP] - thick, iv[UP]);
  Interval oi = Interval (-thick / 2, thick / 2 + fabs (protude));
  oi *= sign (protude);
  b[other] = oi;
  m.add_stencil (round_filled_box (b, blot));
  b[a] = Interval (iv[DOWN], iv[DOWN] + thick);
  m.add_stencil (round_filled_box (b, blot));

  return m;
}

Stencil
Lookup::triangle (Interval iv, Real thick, Real protude)
{
  Box b;
  b[X_AXIS] = Interval (0, iv.length ());
  b[Y_AXIS] = Interval (min (0., protude), max (0.0, protude));

  Offset z1 (iv[LEFT], 0);
  Offset z2 (iv[RIGHT], 0);
  Offset z3 ((z1 + z2)[X_AXIS] / 2, protude);

  /*
    TODO: move Triangle to Line_interface ?
  */
  Stencil tri = Line_interface::make_line (thick, z1, z2);
  tri.add_stencil (Line_interface::make_line (thick, z2, z3));
  tri.add_stencil (Line_interface::make_line (thick, z3, z1));

  return tri;
}

