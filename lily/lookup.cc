/*
  lookup.cc -- implement simple Lookup methods.

  source file of the GNU LilyPond music typesetter

  (c)  1997--2003 Han-Wen Nienhuys <hanwen@cs.uu.nl>

  Jan Nieuwenhuizen <janneke@gnu.org>

  TODO
      Glissando
*/
#include <math.h>
#include <ctype.h>

#include "warn.hh"
#include "dimensions.hh"
#include "bezier.hh"
#include "string-convert.hh"
#include "file-path.hh"
#include "main.hh"
#include "lily-guile.hh"
#include "molecule.hh"
#include "lookup.hh"
#include "font-metric.hh"
#include "interval.hh"

Molecule
Lookup::dot (Offset p, Real radius)
{
  SCM at = (scm_list_n (ly_symbol2scm ("dot"),
			gh_double2scm (p[X_AXIS]),
			gh_double2scm (p[Y_AXIS]),
			gh_double2scm (radius),
			SCM_UNDEFINED));
  Box box;
  box.add_point (p - Offset (radius, radius));
  box.add_point (p + Offset (radius, radius));
  return Molecule (box, at);
}

Molecule 
Lookup::beam (Real slope, Real width, Real thick) 
{
  Real height = slope * width; 
  Real min_y = (0 <? height) - thick/2;
  Real max_y = (0 >? height) + thick/2;

  

  Box b (Interval (0, width),
	 Interval (min_y, max_y));

  
  SCM at = scm_list_n (ly_symbol2scm ("beam"),
		    gh_double2scm (width),
		    gh_double2scm (slope),
		    gh_double2scm (thick),
		    SCM_UNDEFINED);
  return Molecule (b, at);
}

Molecule
Lookup::dashed_slur (Bezier b, Real thick, Real dash)
{
  SCM l = SCM_EOL;

  for (int i= 4; i -- ;)
    {
      l = gh_cons (ly_offset2scm (b.control_[i]), l);
    }

  SCM at = (scm_list_n (ly_symbol2scm ("dashed-slur"),
			       gh_double2scm (thick), 
			       gh_double2scm (dash),
			       ly_quote_scm (l),
			       SCM_UNDEFINED));

  Box box (Interval (0,0),Interval (0,0));
  return   Molecule (box, at);
}

Molecule
Lookup::line (Real th, Offset from, Offset to)
{
  SCM at = scm_list_n (ly_symbol2scm ("draw-line"),
			gh_double2scm (th), 
			gh_double2scm (from[X_AXIS]),
			gh_double2scm (from[Y_AXIS]),
			gh_double2scm (to[X_AXIS]),
			gh_double2scm (to[Y_AXIS]),
			SCM_UNDEFINED);

  Box box;
  box.add_point (from);
  box.add_point (to);

  box[X_AXIS].widen (th/2);
  box[Y_AXIS].widen (th/2);  

  return Molecule (box, at);
}

Molecule
Lookup::dashed_line (Real thick, Offset from, Offset to,
		     Real dash_period, Real dash_fraction)
{
  dash_fraction = (dash_fraction >? 0) <? 1.0;
  Real on = dash_fraction * dash_period + thick; 
  Real off = dash_period - on;
  
  SCM at = scm_list_n (ly_symbol2scm ("dashed-line"),
			gh_double2scm (thick), 
			gh_double2scm (on),
			gh_double2scm (off),
			gh_double2scm (to[X_AXIS] - from[X_AXIS]),
			gh_double2scm (to[Y_AXIS] - from[Y_AXIS]),
			SCM_UNDEFINED);
  
  Box box;
  box.add_point (Offset (0,0));
  box.add_point (to - from);

  box[X_AXIS].widen (thick/2);
  box[Y_AXIS].widen (thick/2);  

  Molecule m = Molecule (box, at);
  m.translate (from);
  return m;
}

Molecule
Lookup::horizontal_line (Interval w, Real th)
{
  SCM at = scm_list_n (ly_symbol2scm ("horizontal-line"),
		       gh_double2scm (w[LEFT]), 
		       gh_double2scm (w[RIGHT]),
		       gh_double2scm (th),
		       SCM_UNDEFINED);


  Box box ;
  box[X_AXIS] = w;
  box[Y_AXIS] = Interval (-th/2,th/2);

  return Molecule (box, at);
}


Molecule
Lookup::blank (Box b) 
{
  return Molecule (b, scm_makfrom0str (""));
}

Molecule
Lookup::filled_box (Box b) 
{
  SCM  at  = (scm_list_n (ly_symbol2scm ("filledbox"),
		     gh_double2scm (-b[X_AXIS][LEFT]),
		     gh_double2scm (b[X_AXIS][RIGHT]),		       
		     gh_double2scm (-b[Y_AXIS][DOWN]),
		     gh_double2scm (b[Y_AXIS][UP]),		       
		     SCM_UNDEFINED));

  return Molecule (b,at);
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
 * |(0,0)                       |
 * |                            |
 * |                            |
 * |<-------------------------->|
 *       Box extent(X_AXIS)
 */
Molecule
Lookup::round_filled_box (Box b, Real blotdiameter)
{
  if (b.x ().length () < blotdiameter)
    {
      programming_error (_f ("round filled box horizontal extent smaller than blot; decreasing blot"));
      blotdiameter = b.x ().length ();
    }
  if (b.y ().length () < blotdiameter)
    {
      programming_error (_f ("round filled box vertical extent smaller than blot; decreasing blot"));
      blotdiameter = b.y ().length ();
    }

  SCM at = (scm_list_n (ly_symbol2scm ("round-filled-box"),
			gh_double2scm (-b[X_AXIS][LEFT]),
			gh_double2scm (b[X_AXIS][RIGHT]),
			gh_double2scm (-b[Y_AXIS][DOWN]),
			gh_double2scm (b[Y_AXIS][UP]),
			gh_double2scm (blotdiameter),
			SCM_UNDEFINED));

  return Molecule (b,at);
}

	  

/*
 * Create Molecule that represents a filled polygon with round edges.
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
 * (c) Given a polygon ((x0, y0), (x1, y1), ... , (x(n-1), y(n-1))),
 * if there is a natural number k such that blotdiameter is greater
 * than the maximum of { | (x(k mod n), y(k mod n)) - (x((k+1) mod n),
 * y((k+1) mod n)) |, | (x(k mod n), y(k mod n)) - (x((k+2) mod n),
 * y((k+2) mod n)) |, | (x((k+1) mod n), y((k+1) mod n)) - (x((k+2)
 * mod n), y((k+2) mod n)) | }, then the outline of the rounded
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
 * round_filled_polygon() sticks to the moulding model, which the
 * majority of the list participants finally voted for.  This,
 * however, results in the above limitations and a much increased
 * complexity of the algorithm, since it has to compute a shrinked
 * polygon -- which is not trivial define precisely and unambigously.
 * With the other approach, one simply could move a circle of size
 * blotdiameter along all edges of the polygon (which is what the
 * postscript routine in the backend effectively does, but on the
 * shrinked polygon). --jr
 */
Molecule
Lookup::round_filled_polygon (Array<Offset> points, Real blotdiameter)
{
  /* TODO: Maybe print a warning if one of the above limitations
     applies to the given polygon.  However, this is quite complicated
     to check. */

  /* remove consecutive duplicate points */
  const Real epsilon = 0.01;
  for (int i = 0; i < points.size ();)
    {
      int next_i = (i + 1) % points.size ();
      Real d = (points[i] - points[next_i]).length ();
      if (d < epsilon)
	points.del (next_i);
      else
	i++;
    }

  /* special cases: degenerated polygons */
  if (points.size () == 0)
    return Molecule ();
  if (points.size () == 1)
    return dot (points[0], 0.5 * blotdiameter);
  if (points.size () == 2)
    return line (blotdiameter, points[0], points[1]);

  /* shrink polygon in size by 0.5 * blotdiameter */
  Array<Offset> shrinked_points;
  shrinked_points.set_size (points.size ());
  bool ccw = 1; // true, if three adjacent points are counterclockwise ordered
  for (int i = 0; i < points.size (); i++)
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
      Real d = p13n.length () * p14n.length (); // distance p3n to line(p1..p0)
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
      shrinked_points[i1] = p1 + ((ccw) ? p13 : -p13);
    }

  /* build scm expression and bounding box */
  SCM shrinked_points_scm = SCM_EOL;
  Box box;
  for (int i = 0; i < shrinked_points.size (); i++)
    {
      SCM x = gh_double2scm (shrinked_points[i][X_AXIS]);
      SCM y = gh_double2scm (shrinked_points[i][Y_AXIS]);
      shrinked_points_scm = gh_cons (x, gh_cons (y, shrinked_points_scm));
      box.add_point (points[i]);
    }
  SCM polygon_scm = scm_list_n (ly_symbol2scm ("polygon"),
				ly_quote_scm (shrinked_points_scm),
				gh_double2scm (blotdiameter),
				SCM_UNDEFINED);

  Molecule polygon = Molecule (box, polygon_scm);
  shrinked_points.clear ();
  return polygon;
}


/*
  TODO: deprecate?

  should use rounded corners.
 */
Molecule
Lookup::frame (Box b, Real thick, Real blot)
{
  Molecule m;
  Direction d = LEFT;
  for (Axis a = X_AXIS; a < NO_AXES; a = Axis (a + 1))
    {
      Axis o = Axis ((a+1)%NO_AXES);
      do
	{
	  Box edges;
	  edges[a] = b[a][d] + 0.5 * thick * Interval (-1, 1);
	  edges[o][DOWN] = b[o][DOWN] - thick/2;
	  edges[o][UP] = b[o][UP] + thick/2;	  
	  
	  m.add_molecule (round_filled_box (edges, blot));
	}
      while (flip (&d) != LEFT);
    }
  return m;
}

/*
  Make a smooth curve along the points 
 */
Molecule
Lookup::slur (Bezier curve, Real curvethick, Real linethick) 
{
  Real alpha = (curve.control_[3] - curve.control_[0]).arg ();
  Bezier back = curve;
  Offset perp = curvethick * complex_exp (Offset (0, alpha + M_PI/2)) * 0.5;
  back.reverse ();
  back.control_[1] += perp;
  back.control_[2] += perp;

  curve.control_[1] -= perp;
  curve.control_[2] -= perp;
  
  SCM scontrols[8];

  for (int i=4; i--;)
    scontrols[ i ] = ly_offset2scm (back.control_[i]);
  for (int i=4 ; i--;)
    scontrols[i+4] = ly_offset2scm (curve.control_[i]);

  /*
    Need the weird order b.o. the way PS want its arguments  
   */
  int indices[]= {5, 6, 7, 4, 1, 2, 3, 0};
  SCM list = SCM_EOL;
  for (int i= 8; i--;)
    {
      list = gh_cons (scontrols[indices[i]], list);
    }
  
  
  SCM at = (scm_list_n (ly_symbol2scm ("bezier-sandwich"),
		     ly_quote_scm (list),
		     gh_double2scm (linethick),
		     SCM_UNDEFINED));
  Box b(curve.extent (X_AXIS),
	curve.extent (Y_AXIS));

  b[X_AXIS].unite (back.extent (X_AXIS));
  b[Y_AXIS].unite (back.extent (Y_AXIS));

  return Molecule (b, at);
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
Molecule
Lookup::bezier_sandwich (Bezier top_curve, Bezier bottom_curve)
{
  /*
    Need the weird order b.o. the way PS want its arguments  
   */
  SCM list = SCM_EOL;
  list = gh_cons (ly_offset2scm (bottom_curve.control_[3]), list);
  list = gh_cons (ly_offset2scm (bottom_curve.control_[0]), list);
  list = gh_cons (ly_offset2scm (bottom_curve.control_[1]), list);
  list = gh_cons (ly_offset2scm (bottom_curve.control_[2]), list);
  list = gh_cons (ly_offset2scm (top_curve.control_[0]), list);
  list = gh_cons (ly_offset2scm (top_curve.control_[3]), list);
  list = gh_cons (ly_offset2scm (top_curve.control_[2]), list);
  list = gh_cons (ly_offset2scm (top_curve.control_[1]), list);

  SCM horizontal_bend = scm_list_n (ly_symbol2scm ("bezier-sandwich"),
				    ly_quote_scm (list),
				    gh_double2scm (0.0),
				    SCM_UNDEFINED);

  Interval x_extent = top_curve.extent (X_AXIS);
  x_extent.unite (bottom_curve.extent (X_AXIS));
  Interval y_extent = top_curve.extent (Y_AXIS);
  y_extent.unite (bottom_curve.extent (Y_AXIS));
  Box b (x_extent, y_extent);

  return Molecule (b, horizontal_bend);
}

/*
 * Horizontal Slope:
 *
 *            /|   ^
 *           / |   |
 *          /  |   | height
 *         /   |   |
 *        /    |   v
 *       |    /
 *       |   /
 * (0,0) x  /slope=dy/dx
 *       | /
 *       |/
 *
 *       <----->
 *        width
 */
Molecule
Lookup::horizontal_slope (Real width, Real slope, Real height)
{
  SCM width_scm = gh_double2scm (width);
  SCM slope_scm = gh_double2scm (slope);
  SCM height_scm = gh_double2scm (height);
  SCM horizontal_slope = scm_list_n (ly_symbol2scm ("beam"),
				     width_scm, slope_scm,
				     height_scm, SCM_UNDEFINED);
  Box b (Interval (0, width),
	 Interval (-height/2, height/2 + width*slope));
  return Molecule (b, horizontal_slope);
}

/*
  TODO: junk me.
 */
Molecule
Lookup::accordion (SCM s, Real staff_space, Font_metric *fm) 
{
  Molecule m;
  String sym = ly_scm2string (ly_car (s));
  String reg = ly_scm2string (ly_car (ly_cdr (s)));

  if (sym == "Discant")
    {
      Molecule r = fm->find_by_name ("accordion-accDiscant");
      m.add_molecule (r);
      if (reg.left_string (1) == "F")
	{
	  Molecule d = fm->find_by_name ("accordion-accDot");
	  d.translate_axis (staff_space * 2.5 PT, Y_AXIS);
	  m.add_molecule (d);
	  reg = reg.right_string (reg.length ()-1);
	}
      int eflag = 0x00;
      if (reg.left_string (3) == "EEE")
	{
	  eflag = 0x07;
	  reg = reg.right_string (reg.length ()-3);
	}
      else if (reg.left_string (2) == "EE")
	{
	  eflag = 0x05;
	  reg = reg.right_string (reg.length ()-2);
	}
      else if (reg.left_string (2) == "Eh")
	{
	  eflag = 0x04;
	  reg = reg.right_string (reg.length ()-2);
	}
      else if (reg.left_string (1) == "E")
	{
	  eflag = 0x02;
	  reg = reg.right_string (reg.length ()-1);
	}
      if (eflag & 0x02)
	{
	  Molecule d = fm->find_by_name ("accordion-accDot");
	  d.translate_axis (staff_space * 1.5 PT, Y_AXIS);
	  m.add_molecule (d);
	}
      if (eflag & 0x04)
	{
	  Molecule d = fm->find_by_name ("accordion-accDot");
	  d.translate_axis (staff_space * 1.5 PT, Y_AXIS);
	  d.translate_axis (0.8 * staff_space PT, X_AXIS);
	  m.add_molecule (d);
	}
      if (eflag & 0x01)
	{
	  Molecule d = fm->find_by_name ("accordion-accDot");
	  d.translate_axis (staff_space * 1.5 PT, Y_AXIS);
	  d.translate_axis (-0.8 * staff_space PT, X_AXIS);
	  m.add_molecule (d);
	}
      if (reg.left_string (2) == "SS")
	{
	  Molecule d = fm->find_by_name ("accordion-accDot");
	  d.translate_axis (0.5 * staff_space PT, Y_AXIS);
	  d.translate_axis (0.4 * staff_space PT, X_AXIS);
	  m.add_molecule (d);
	  d.translate_axis (-0.8 * staff_space PT, X_AXIS);
	  m.add_molecule (d);
	  reg = reg.right_string (reg.length ()-2);
	}
      if (reg.left_string (1) == "S")
	{
	  Molecule d = fm->find_by_name ("accordion-accDot");
	  d.translate_axis (0.5 * staff_space PT, Y_AXIS);
	  m.add_molecule (d);
	  reg = reg.right_string (reg.length ()-1);
	}
    }
  else if (sym == "Freebase")
    {
      Molecule r = fm->find_by_name ("accordion-accFreebase");
      m.add_molecule (r);
      if (reg.left_string (1) == "F")
	{
	  Molecule d = fm->find_by_name ("accordion-accDot");
	  d.translate_axis (staff_space * 1.5 PT, Y_AXIS);
	  m.add_molecule (d);
	  reg = reg.right_string (reg.length ()-1);
	}
      if (reg == "E")
	{
	  Molecule d = fm->find_by_name ("accordion-accDot");
	  d.translate_axis (staff_space * 0.5 PT, Y_AXIS);
	  m.add_molecule (d);
	}
    }
  else if (sym == "Bayanbase")
    {
      Molecule r = fm->find_by_name ("accordion-accBayanbase");
      m.add_molecule (r);
      if (reg.left_string (1) == "T")
	{
	  Molecule d = fm->find_by_name ("accordion-accDot");
	  d.translate_axis (staff_space * 2.5 PT, Y_AXIS);
	  m.add_molecule (d);
	  reg = reg.right_string (reg.length ()-1);
	}
      /* include 4' reed just for completeness. You don't want to use this. */
      if (reg.left_string (1) == "F")
	{
	  Molecule d = fm->find_by_name ("accordion-accDot");
	  d.translate_axis (staff_space * 1.5 PT, Y_AXIS);
	  m.add_molecule (d);
	  reg = reg.right_string (reg.length ()-1);
	}
      if (reg.left_string (2) == "EE")
	{
	  Molecule d = fm->find_by_name ("accordion-accDot");
	  d.translate_axis (staff_space * 0.5 PT, Y_AXIS);
	  d.translate_axis (0.4 * staff_space PT, X_AXIS);
	  m.add_molecule (d);
	  d.translate_axis (-0.8 * staff_space PT, X_AXIS);
	  m.add_molecule (d);
	  reg = reg.right_string (reg.length ()-2);
	}
      if (reg.left_string (1) == "E")
	{
	  Molecule d = fm->find_by_name ("accordion-accDot");
	  d.translate_axis (staff_space * 0.5 PT, Y_AXIS);
	  m.add_molecule (d);
	  reg = reg.right_string (reg.length ()-1);
	}
    }
  else if (sym == "Stdbase")
    {
      Molecule r = fm->find_by_name ("accordion-accStdbase");
      m.add_molecule (r);
      if (reg.left_string (1) == "T")
	{
	  Molecule d = fm->find_by_name ("accordion-accDot");
	  d.translate_axis (staff_space * 3.5 PT, Y_AXIS);
	  m.add_molecule (d);
	  reg = reg.right_string (reg.length ()-1);
	}
      if (reg.left_string (1) == "F")
	{
	  Molecule d = fm->find_by_name ("accordion-accDot");
	  d.translate_axis (staff_space * 2.5 PT, Y_AXIS);
	  m.add_molecule (d);
	  reg = reg.right_string (reg.length ()-1);
	}
      if (reg.left_string (1) == "M")
	{
	  Molecule d = fm->find_by_name ("accordion-accDot");
	  d.translate_axis (staff_space * 2 PT, Y_AXIS);
	  d.translate_axis (staff_space PT, X_AXIS);
	  m.add_molecule (d);
	  reg = reg.right_string (reg.length ()-1);
	}
      if (reg.left_string (1) == "E")
	{
	  Molecule d = fm->find_by_name ("accordion-accDot");
	  d.translate_axis (staff_space * 1.5 PT, Y_AXIS);
	  m.add_molecule (d);
	  reg = reg.right_string (reg.length ()-1);
	}
      if (reg.left_string (1) == "S")
	{
	  Molecule d = fm->find_by_name ("accordion-accDot");
	  d.translate_axis (staff_space * 0.5 PT, Y_AXIS);
	  m.add_molecule (d);
	  reg = reg.right_string (reg.length ()-1);
	}
    }
  /* ugh maybe try to use regular font for S.B. and B.B and only use one font
     for the rectangle */
  else if (sym == "SB")
    {
      Molecule r = fm->find_by_name ("accordion-accSB");
      m.add_molecule (r);
    }
  else if (sym == "BB")
    {
      Molecule r = fm->find_by_name ("accordion-accBB");
      m.add_molecule (r);
    }
  else if (sym == "OldEE")
    {
      Molecule r = fm->find_by_name ("accordion-accOldEE");
      m.add_molecule (r);
    }
  else if (sym == "OldEES")
    {
      Molecule r = fm->find_by_name ("accordion-accOldEES");
      m.add_molecule (r);
    }
  return m;  
}

Molecule
Lookup::repeat_slash (Real w, Real s, Real t)
{
  SCM wid = gh_double2scm (w);
  SCM sl = gh_double2scm (s);
  SCM thick = gh_double2scm (t);
  SCM slashnodot = scm_list_n (ly_symbol2scm ("repeat-slash"),
			    wid, sl, thick, SCM_UNDEFINED);

  Box b (Interval (0, w + sqrt (sqr(t/s) + sqr (t))),
	 Interval (0, w * s));

  return Molecule (b, slashnodot); //  http://slashnodot.org
}


Molecule
Lookup::bracket (Axis a, Interval iv, Real thick, Real protude, Real blot)
{
  Box b;
  Axis other = Axis((a+1)%2);
  b[a] = iv;
  b[other] = Interval(-1, 1) * thick * 0.5;
  
  Molecule m =  round_filled_box (b, blot);

  b[a] = Interval (iv[UP] - thick, iv[UP]);
  Interval oi = Interval (-thick/2, thick/2 + fabs (protude)) ;
  oi *=  sign (protude);
  b[other] = oi;
  m.add_molecule (round_filled_box (b, blot));
  b[a] = Interval (iv[DOWN], iv[DOWN]  +thick);
  m.add_molecule (round_filled_box (b,blot));

  return m;
}

Molecule
Lookup::triangle (Interval iv, Real thick, Real protude)
{
  Box b ;
  b[X_AXIS] = iv;
  b[Y_AXIS] = Interval (0 <? protude , 0 >? protude);

  SCM s = scm_list_n (ly_symbol2scm ("symmetric-x-triangle"),
		      gh_double2scm (thick),
		      gh_double2scm (iv.length()), 
		      gh_double2scm (protude), SCM_UNDEFINED);

  return Molecule (b, s);
}


/*
  TODO: use rounded boxes.
 */
LY_DEFINE(ly_bracket ,"ly:bracket",
	  4, 0, 0,
	  (SCM a, SCM iv, SCM t, SCM p),
	  "Make a bracket in direction @var{a}. The extent of the bracket is " 
	  "given by @var{iv}. The wings protude by an amount of @var{p}, which "
	  "may be negative. The thickness is given by @var{t}.")
{
  SCM_ASSERT_TYPE(is_axis (a), a, SCM_ARG1, __FUNCTION__, "axis") ;
  SCM_ASSERT_TYPE(is_number_pair (iv), iv, SCM_ARG2, __FUNCTION__, "number pair") ;
  SCM_ASSERT_TYPE(gh_number_p (t), a, SCM_ARG3, __FUNCTION__, "number") ;
  SCM_ASSERT_TYPE(gh_number_p (p), a, SCM_ARG4, __FUNCTION__, "number") ;


  return Lookup::bracket ((Axis)gh_scm2int (a), ly_scm2interval (iv),
			  gh_scm2double (t),
			  gh_scm2double (p),
			  gh_scm2double (t)).smobbed_copy ();
}



LY_DEFINE(ly_filled_box ,"ly:round-filled-box",
	  3, 0, 0,
	  (SCM xext, SCM yext, SCM blot),
	  "Make a filled-box of dimensions @var{xext}, @var{yext} and roundness @var{blot}.")
{
  SCM_ASSERT_TYPE(is_number_pair (xext), xext, SCM_ARG1, __FUNCTION__, "number pair") ;
  SCM_ASSERT_TYPE(is_number_pair (yext), yext, SCM_ARG2, __FUNCTION__, "number pair") ;
  SCM_ASSERT_TYPE(gh_number_p (blot), blot, SCM_ARG3, __FUNCTION__, "number") ;

  return Lookup::round_filled_box (Box (ly_scm2interval (xext), ly_scm2interval (yext)),
				   gh_scm2double (blot)).smobbed_copy ();
}

