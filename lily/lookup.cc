/*
  lookup.cc -- implement simple Lookup methods.

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>

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
Lookup::line (Real th, Offset f, Offset t)
{
  SCM at = (scm_list_n (ly_symbol2scm ("draw-line"),
			gh_double2scm (th), 
			gh_double2scm (f[X_AXIS]),
			gh_double2scm (f[Y_AXIS]),
			gh_double2scm (t[X_AXIS]),
			gh_double2scm (t[Y_AXIS]),
			SCM_UNDEFINED));

  Box box;
  box.add_point (f);
  box.add_point (t);

  box[X_AXIS].widen (th/2);
  box[Y_AXIS].widen (th/2);  

  return Molecule (box, at);
}


Molecule
Lookup::blank (Box b) 
{
  return Molecule (b, SCM_EOL);
}

Molecule
Lookup::filledbox (Box b) 
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
Lookup::roundfilledbox (Box b, Real blotdiameter)
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

  SCM at = (scm_list_n (ly_symbol2scm ("roundfilledbox"),
			gh_double2scm (-b[X_AXIS][LEFT]),
			gh_double2scm (b[X_AXIS][RIGHT]),
			gh_double2scm (-b[Y_AXIS][DOWN]),
			gh_double2scm (b[Y_AXIS][UP]),
			gh_double2scm (blotdiameter),
			SCM_UNDEFINED));

  return Molecule (b,at);
}

Molecule
Lookup::frame (Box b, Real thick)
{
  Molecule m;
  Direction d = LEFT;
  Axis a = X_AXIS;
  while (a < NO_AXES)
    {
      do
	{
	  Axis o = Axis ((a+1)%NO_AXES);

	  Box edges;
	  edges[a] = b[a][d] + 0.5 * thick * Interval (-1, 1);
	  edges[o][DOWN] = b[o][DOWN] - thick/2;
	  edges[o][UP] = b[o][UP] + thick/2;	  
	  
	  m.add_molecule (filledbox (edges));
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
  
  
  SCM at = (scm_list_n (ly_symbol2scm ("bezier-bow"),
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
Lookup::bracket (Axis a, Interval iv, Real thick, Real protude)
{
  Box b;
  Axis other = Axis((a+1)%2);
  b[a] = iv;
  b[other] = Interval(-1, 1) * thick * 0.5;
  
  Molecule m =  filledbox (b);

  b[a] = Interval (iv[UP] - thick, iv[UP]);
  Interval oi = Interval (-thick/2, thick/2 + fabs (protude)) ;
  oi *=  sign (protude);
  b[other] = oi;
  m.add_molecule (filledbox (b));
  b[a] = Interval (iv[DOWN], iv[DOWN]  +thick);
  m.add_molecule (filledbox(b));

  return m;
}

/*
  TODO: use rounded boxes.
 */
LY_DEFINE(ly_bracket ,"ly-bracket",
	  4, 0, 0,
	  (SCM a, SCM iv, SCM t, SCM p),
	  "Make a bracket in direction @var{a}. The extent of the bracket is
given by @var{iv}. The wings protude by an amount of @var{p}, which
may be negative. The thickness is given by @var{t}.")
{
  SCM_ASSERT_TYPE(ly_axis_p (a), a, SCM_ARG1, __FUNCTION__, "axis") ;
  SCM_ASSERT_TYPE(ly_number_pair_p (iv), iv, SCM_ARG2, __FUNCTION__, "number pair") ;
  SCM_ASSERT_TYPE(gh_number_p (t), a, SCM_ARG3, __FUNCTION__, "number") ;
  SCM_ASSERT_TYPE(gh_number_p (p), a, SCM_ARG4, __FUNCTION__, "number") ;


  return Lookup::bracket ((Axis)gh_scm2int (a), ly_scm2interval (iv),
			  gh_scm2double (t),
			  gh_scm2double (p)).smobbed_copy ();
}
  
