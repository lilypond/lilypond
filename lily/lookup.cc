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
      if (reg.left_str (1) == "F")
	{
	  Molecule d = fm->find_by_name ("accordion-accDot");
	  d.translate_axis (staff_space * 2.5 PT, Y_AXIS);
	  m.add_molecule (d);
	  reg = reg.right_str (reg.length_i ()-1);
	}
      int eflag = 0x00;
      if (reg.left_str (3) == "EEE")
	{
	  eflag = 0x07;
	  reg = reg.right_str (reg.length_i ()-3);
	}
      else if (reg.left_str (2) == "EE")
	{
	  eflag = 0x05;
	  reg = reg.right_str (reg.length_i ()-2);
	}
      else if (reg.left_str (2) == "Eh")
	{
	  eflag = 0x04;
	  reg = reg.right_str (reg.length_i ()-2);
	}
      else if (reg.left_str (1) == "E")
	{
	  eflag = 0x02;
	  reg = reg.right_str (reg.length_i ()-1);
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
      if (reg.left_str (2) == "SS")
	{
	  Molecule d = fm->find_by_name ("accordion-accDot");
	  d.translate_axis (0.5 * staff_space PT, Y_AXIS);
	  d.translate_axis (0.4 * staff_space PT, X_AXIS);
	  m.add_molecule (d);
	  d.translate_axis (-0.8 * staff_space PT, X_AXIS);
	  m.add_molecule (d);
	  reg = reg.right_str (reg.length_i ()-2);
	}
      if (reg.left_str (1) == "S")
	{
	  Molecule d = fm->find_by_name ("accordion-accDot");
	  d.translate_axis (0.5 * staff_space PT, Y_AXIS);
	  m.add_molecule (d);
	  reg = reg.right_str (reg.length_i ()-1);
	}
    }
  else if (sym == "Freebase")
    {
      Molecule r = fm->find_by_name ("accordion-accFreebase");
      m.add_molecule (r);
      if (reg.left_str (1) == "F")
	{
	  Molecule d = fm->find_by_name ("accordion-accDot");
	  d.translate_axis (staff_space * 1.5 PT, Y_AXIS);
	  m.add_molecule (d);
	  reg = reg.right_str (reg.length_i ()-1);
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
      if (reg.left_str (1) == "T")
	{
	  Molecule d = fm->find_by_name ("accordion-accDot");
	  d.translate_axis (staff_space * 2.5 PT, Y_AXIS);
	  m.add_molecule (d);
	  reg = reg.right_str (reg.length_i ()-1);
	}
      /* include 4' reed just for completeness. You don't want to use this. */
      if (reg.left_str (1) == "F")
	{
	  Molecule d = fm->find_by_name ("accordion-accDot");
	  d.translate_axis (staff_space * 1.5 PT, Y_AXIS);
	  m.add_molecule (d);
	  reg = reg.right_str (reg.length_i ()-1);
	}
      if (reg.left_str (2) == "EE")
	{
	  Molecule d = fm->find_by_name ("accordion-accDot");
	  d.translate_axis (staff_space * 0.5 PT, Y_AXIS);
	  d.translate_axis (0.4 * staff_space PT, X_AXIS);
	  m.add_molecule (d);
	  d.translate_axis (-0.8 * staff_space PT, X_AXIS);
	  m.add_molecule (d);
	  reg = reg.right_str (reg.length_i ()-2);
	}
      if (reg.left_str (1) == "E")
	{
	  Molecule d = fm->find_by_name ("accordion-accDot");
	  d.translate_axis (staff_space * 0.5 PT, Y_AXIS);
	  m.add_molecule (d);
	  reg = reg.right_str (reg.length_i ()-1);
	}
    }
  else if (sym == "Stdbase")
    {
      Molecule r = fm->find_by_name ("accordion-accStdbase");
      m.add_molecule (r);
      if (reg.left_str (1) == "T")
	{
	  Molecule d = fm->find_by_name ("accordion-accDot");
	  d.translate_axis (staff_space * 3.5 PT, Y_AXIS);
	  m.add_molecule (d);
	  reg = reg.right_str (reg.length_i ()-1);
	}
      if (reg.left_str (1) == "F")
	{
	  Molecule d = fm->find_by_name ("accordion-accDot");
	  d.translate_axis (staff_space * 2.5 PT, Y_AXIS);
	  m.add_molecule (d);
	  reg = reg.right_str (reg.length_i ()-1);
	}
      if (reg.left_str (1) == "M")
	{
	  Molecule d = fm->find_by_name ("accordion-accDot");
	  d.translate_axis (staff_space * 2 PT, Y_AXIS);
	  d.translate_axis (staff_space PT, X_AXIS);
	  m.add_molecule (d);
	  reg = reg.right_str (reg.length_i ()-1);
	}
      if (reg.left_str (1) == "E")
	{
	  Molecule d = fm->find_by_name ("accordion-accDot");
	  d.translate_axis (staff_space * 1.5 PT, Y_AXIS);
	  m.add_molecule (d);
	  reg = reg.right_str (reg.length_i ()-1);
	}
      if (reg.left_str (1) == "S")
	{
	  Molecule d = fm->find_by_name ("accordion-accDot");
	  d.translate_axis (staff_space * 0.5 PT, Y_AXIS);
	  m.add_molecule (d);
	  reg = reg.right_str (reg.length_i ()-1);
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

/*
  TODO: should use slope instead?  Angle gives nasty rad <-> degree
  conversions.
*/
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
Lookup::bracket (Axis a, Interval iv, Direction d, Real thick, Real protude)
{
  Box b;
  Axis other = Axis((a+1)%2);
  b[a] = iv;
  b[other] = Interval(-1, 1) * thick * 0.5;
  
  Molecule m =  filledbox (b);

  b[a] = Interval (iv[UP] - thick, iv[UP]);
  Interval oi = Interval (-thick/2, thick/2 + protude) ;
  oi *=  d;
  b[other] = oi;
  m.add_molecule (filledbox (b));
  b[a] = Interval (iv[DOWN], iv[DOWN]  +thick);
  m.add_molecule (filledbox(b));

  return m;
}

SCM
ly_bracket (SCM a, SCM iv, SCM d, SCM t, SCM p)
{
  SCM_ASSERT_TYPE(ly_axis_p (a), a, SCM_ARG1, __FUNCTION__, "axis") ;
  SCM_ASSERT_TYPE(ly_number_pair_p (iv), iv, SCM_ARG1, __FUNCTION__, "number pair") ;
  SCM_ASSERT_TYPE(isdir_b (d), a, SCM_ARG1, __FUNCTION__, "direction") ;
  SCM_ASSERT_TYPE(gh_number_p (t), a, SCM_ARG1, __FUNCTION__, "number") ;
  SCM_ASSERT_TYPE(gh_number_p(p), a, SCM_ARG1, __FUNCTION__, "number") ;


  return Lookup::bracket ((Axis)gh_scm2int (a), ly_scm2interval (iv),
		  (Direction)gh_scm2int (d), gh_scm2double (t), gh_scm2double (p)).smobbed_copy ();
}
  
static void
lookup_init ()
{
  scm_c_define_gsubr ("ly-bracket", 5, 0, 0, (Scheme_function_unknown) ly_bracket);
}

ADD_SCM_INIT_FUNC (lookup,lookup_init);

