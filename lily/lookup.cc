/*
  lookup.cc -- implement simple Lookup methods.

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>

  Jan Nieuwenhuizen <janneke@gnu.org>

  TODO
      Glissando
*/
#include <math.h>
#include <ctype.h>

#include "lookup.hh"
#include "debug.hh"
#include "dimensions.hh"
#include "bezier.hh"
#include "paper-def.hh"
#include "string-convert.hh"
#include "file-path.hh"
#include "main.hh"
#include "lily-guile.hh"
#include "all-font-metrics.hh"
#include "afm.hh"
#include "scope.hh"
#include "molecule.hh"

#include "lily-guile.hh"


Lookup::Lookup ()
{
  afm_l_ = 0;  
}

Lookup::Lookup (Lookup const& s)
{
  font_name_ = s.font_name_;
  afm_l_ = 0;  
}




Molecule
Lookup::afm_find (String s, bool warn) const
{
  if (!afm_l_)      
    {
      Lookup * me = (Lookup*)(this);
      me->afm_l_ = all_fonts_global_p->find_afm (font_name_);
      if (!me->afm_l_)
	{
	  warning (_f ("can't find font: `%s'", font_name_));
	  warning (_f ("(search path: `%s')", global_path.str ().ch_C()));
	  error (_ ("Aborting"));
	}
    }
  AFM_CharMetricInfo const *cm = afm_l_->find_char_metric (s, warn);

  if (!cm)
    {
      Molecule m;
      m.set_empty (false);
      return m;
    }
  
  SCM at =  (gh_list (ly_symbol2scm ("char"),
		    gh_int2scm (cm->code),
		    SCM_UNDEFINED));

  at= fontify_atom (afm_l_,at);
  return Molecule ( afm_bbox_to_box (cm->charBBox), at);
}

  


Molecule 
Lookup::beam (Real slope, Real width, Real thick) 
{
  Real height = slope * width; 
  Real min_y = (0 <? height) - thick/2;
  Real max_y = (0 >? height) + thick/2;

  

  Box b( Interval (0, width),
	 Interval (min_y, max_y));

  
  SCM at = gh_list (ly_symbol2scm ("beam"),
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

  SCM at = (gh_list (ly_symbol2scm ("dashed-slur"),
			       gh_double2scm (thick), 
			       gh_double2scm (dash),
			       ly_quote_scm (l),
			       SCM_UNDEFINED));

  Box box (Interval(0,0),Interval( 0,0));
  return   Molecule (box, at);
}




Molecule
Lookup::blank (Box b) 
{
  return Molecule (b, SCM_EOL);
}


Molecule
Lookup::filledbox (Box b ) 
{
  SCM  at  = (gh_list (ly_symbol2scm ("filledbox"),
		     gh_double2scm (-b[X_AXIS][LEFT]),
		     gh_double2scm (b[X_AXIS][RIGHT]),		       
		     gh_double2scm (-b[Y_AXIS][DOWN]),
		     gh_double2scm (b[Y_AXIS][UP]),		       
		     SCM_UNDEFINED));

  return Molecule ( b,at);
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
   TODO: THIS IS UGLY.
   Since the user has direct access to TeX marcos,
   that currently provide the  only way to do
   font selection, accents etc,
   we try some halfbaked attempt to detect this TeX trickery.
 */
String
sanitise_TeX_string (String text)
{
  int brace_count =0;
  for (int i= 0; i < text.length_i (); i++)
    {
      if (text[i] == '\\')
	continue;
      
      if (text[i] == '{')
	brace_count ++;
      else if (text[i] == '}')
	brace_count --;
    }
  
  if(brace_count)
    {
      warning (_f ("Non-matching braces in text `%s', adding braces", text.ch_C()));

      if (brace_count < 0)
	{
	  text = to_str ('{', -brace_count) + text;
	}
      else 
	{
	  text = text + to_str ('}', brace_count);
	}
    }
    
  return text;
}

/**
   TODO!
 */
String
sanitise_PS_string (String t)
{
  return t;
}

/**

*/
Molecule
Lookup::text (String style, String text, Paper_def *paper_l) 
{
  if (style.empty_b ())
    style = "roman";
  
  int font_mag = 0;
  Real font_h = paper_l->get_var ("font_normal");
  if (paper_l->scope_p_->elem_b ("font_" + style))
    {
      font_h = paper_l->get_var ("font_" + style);
    }


  if (paper_l->scope_p_->elem_b ("magnification_" + style))
    {
      font_mag = (int)paper_l->get_var ("magnification_" + style);
    }

  /*
    UGH.
  */
  SCM l = scm_eval (gh_list (ly_symbol2scm ("style-to-cmr"),
			     ly_str02scm (style.ch_C()),
			     SCM_UNDEFINED));
  
  if (l != SCM_BOOL_F)
    {
      style = ly_scm2string (gh_cdr(l)) +to_str  ((int)font_h);
    }

  

  Font_metric* metric_l = 0;

  if (font_mag)
    metric_l = all_fonts_global_p->find_scaled (style, font_mag);
  else
    metric_l = all_fonts_global_p->find_font (style);
  
  
    

  int i = text.index_i ("\\n");
  while (i >=0 )
    {
      text = text.left_str (i) + "\n" + text.right_str (text.length_i () - i - 2);
      i = text.index_i ("\\n");
    }
  
  Array<String> lines = String_convert::split_arr (text, '\n');
  
  Real kern = paper_l->get_var ("line_kern");
  
  for (int i=0; i < lines.size (); i++)
    {
      String str (lines[i]);
      if (output_global_ch == "tex")
	str = sanitise_TeX_string  (str);
      else if (output_global_ch == "ps")
	str = sanitise_PS_string (str);
      lines[i] = str;
    }

  if (!lines.size())
	return Molecule();

  SCM first = gh_list (ly_symbol2scm ("text"),
			 ly_str02scm (lines[0].ch_C()),
			 SCM_UNDEFINED);
  first = fontify_atom (metric_l, first);

  

  Molecule mol (metric_l->text_dimension (lines[0]), first);

  for (i = 1; i < lines.size (); i++)
    {
      SCM line = (gh_list (ly_symbol2scm ("text"),
			   ly_str02scm (lines[i].ch_C ()),
			   SCM_UNDEFINED));
      line = fontify_atom (metric_l, line);
      mol.add_at_edge (Y_AXIS, DOWN,
		       Molecule (metric_l->text_dimension (lines[i]), line),
		       kern);
    }

  return mol;
}
  


/*
  Make a smooth curve along the points 
 */
Molecule
Lookup::slur (Bezier curve, Real curvethick, Real linethick) 
{
  Real alpha = (curve.control_[3] - curve.control_[0]).arg ();
  Bezier back = curve;

  back.reverse ();
  back.control_[1] += curvethick * complex_exp (Offset (0, alpha + M_PI/2));
  back.control_[2] += curvethick * complex_exp (Offset (0, alpha + M_PI/2));  

  SCM scontrols[8];

  for (int i=4; i--;)
    scontrols[ i ] = ly_offset2scm(back.control_[i]);
  for (int i=4 ; i--;)
    scontrols[i+4] = ly_offset2scm (curve.control_[i]);

  /*
    Need the weird order b.o. the way PS want its arguments  
   */
  int indices[]= {5, 6, 7, 4, 1, 2, 3, 0};
  SCM list = SCM_EOL;
  for (int i= 8; i--;  )
    {
      list = gh_cons (scontrols[indices[i]], list);
    }
  
  
  SCM at = (gh_list (ly_symbol2scm ("bezier-sandwich"),
		     ly_quote_scm (list),
		     gh_double2scm (linethick),
		     SCM_UNDEFINED));

  Box b ( curve.extent (X_AXIS), curve.extent (Y_AXIS));
  return Molecule (b, at);
}

Molecule
Lookup::accordion (SCM s, Real staff_space) const
{
  Molecule m;
  String sym = ly_scm2string(gh_car (s));
  String reg = ly_scm2string(gh_car (gh_cdr(s)));

  if (sym == "Discant")
    {
      Molecule r = afm_find("accordion-accDiscant");
      m.add_molecule(r);
      if (reg.left_str(1) == "F")
	{
	  Molecule d = afm_find("accordion-accDot");
	  d.translate_axis(staff_space * 2.5 PT, Y_AXIS);
	  m.add_molecule(d);
	  reg = reg.right_str(reg.length_i()-1);
	}
      int eflag = 0x00;
      if (reg.left_str(3) == "EEE")
	{
	  eflag = 0x07;
	  reg = reg.right_str(reg.length_i()-3);
	}
      else if (reg.left_str(2) == "EE")
	{
	  eflag = 0x05;
	  reg = reg.right_str(reg.length_i()-2);
	}
      else if (reg.left_str(2) == "Eh")
	{
	  eflag = 0x04;
	  reg = reg.right_str(reg.length_i()-2);
	}
      else if (reg.left_str(1) == "E")
	{
	  eflag = 0x02;
	  reg = reg.right_str(reg.length_i()-1);
	}
      if (eflag & 0x02)
	{
	  Molecule d = afm_find("accordion-accDot");
	  d.translate_axis(staff_space * 1.5 PT, Y_AXIS);
	  m.add_molecule(d);
	}
      if (eflag & 0x04)
	{
	  Molecule d = afm_find("accordion-accDot");
	  d.translate_axis(staff_space * 1.5 PT, Y_AXIS);
	  d.translate_axis(0.8 * staff_space PT, X_AXIS);
	  m.add_molecule(d);
	}
      if (eflag & 0x01)
	{
	  Molecule d = afm_find("accordion-accDot");
	  d.translate_axis(staff_space * 1.5 PT, Y_AXIS);
	  d.translate_axis(-0.8 * staff_space PT, X_AXIS);
	  m.add_molecule(d);
	}
      if (reg.left_str(2) == "SS")
	{
	  Molecule d = afm_find("accordion-accDot");
	  d.translate_axis(0.5 * staff_space PT, Y_AXIS);
	  d.translate_axis(0.4 * staff_space PT, X_AXIS);
	  m.add_molecule(d);
	  d.translate_axis(-0.8 * staff_space PT, X_AXIS);
	  m.add_molecule(d);
	  reg = reg.right_str(reg.length_i()-2);
	}
      if (reg.left_str(1) == "S")
	{
	  Molecule d = afm_find("accordion-accDot");
	  d.translate_axis(0.5 * staff_space PT, Y_AXIS);
	  m.add_molecule(d);
	  reg = reg.right_str(reg.length_i()-1);
	}
    }
  else if (sym == "Freebase")
    {
      Molecule r = afm_find("accordion-accFreebase");
      m.add_molecule(r);
      if (reg.left_str(1) == "F")
	{
	  Molecule d = afm_find("accordion-accDot");
	  d.translate_axis(staff_space * 1.5 PT, Y_AXIS);
	  m.add_molecule(d);
	  reg = reg.right_str(reg.length_i()-1);
	}
      if (reg == "E")
	{
	  Molecule d = afm_find("accordion-accDot");
	  d.translate_axis(staff_space * 0.5 PT, Y_AXIS);
	  m.add_molecule(d);
	}
    }
  else if (sym == "Bayanbase")
    {
      Molecule r = afm_find("accordion-accBayanbase");
      m.add_molecule(r);
      if (reg.left_str(1) == "T")
	{
	  Molecule d = afm_find("accordion-accDot");
	  d.translate_axis(staff_space * 2.5 PT, Y_AXIS);
	  m.add_molecule(d);
	  reg = reg.right_str(reg.length_i()-1);
	}
      /* include 4' reed just for completeness. You don't want to use this. */
      if (reg.left_str(1) == "F")
	{
	  Molecule d = afm_find("accordion-accDot");
	  d.translate_axis(staff_space * 1.5 PT, Y_AXIS);
	  m.add_molecule(d);
	  reg = reg.right_str(reg.length_i()-1);
	}
      if (reg.left_str(2) == "EE")
	{
	  Molecule d = afm_find("accordion-accDot");
	  d.translate_axis(staff_space * 0.5 PT, Y_AXIS);
	  d.translate_axis(0.4 * staff_space PT, X_AXIS);
	  m.add_molecule(d);
	  d.translate_axis(-0.8 * staff_space PT, X_AXIS);
	  m.add_molecule(d);
	  reg = reg.right_str(reg.length_i()-2);
	}
      if (reg.left_str(1) == "E")
	{
	  Molecule d = afm_find("accordion-accDot");
	  d.translate_axis(staff_space * 0.5 PT, Y_AXIS);
	  m.add_molecule(d);
	  reg = reg.right_str(reg.length_i()-1);
	}
    }
  else if (sym == "Stdbase")
    {
      Molecule r = afm_find("accordion-accStdbase");
      m.add_molecule(r);
      if (reg.left_str(1) == "T")
	{
	  Molecule d = afm_find("accordion-accDot");
	  d.translate_axis(staff_space * 3.5 PT, Y_AXIS);
	  m.add_molecule(d);
	  reg = reg.right_str(reg.length_i()-1);
	}
      if (reg.left_str(1) == "F")
	{
	  Molecule d = afm_find("accordion-accDot");
	  d.translate_axis(staff_space * 2.5 PT, Y_AXIS);
	  m.add_molecule(d);
	  reg = reg.right_str(reg.length_i()-1);
	}
      if (reg.left_str(1) == "M")
	{
	  Molecule d = afm_find("accordion-accDot");
	  d.translate_axis(staff_space * 2 PT, Y_AXIS);
	  d.translate_axis(staff_space PT, X_AXIS);
	  m.add_molecule(d);
	  reg = reg.right_str(reg.length_i()-1);
	}
      if (reg.left_str(1) == "E")
	{
	  Molecule d = afm_find("accordion-accDot");
	  d.translate_axis(staff_space * 1.5 PT, Y_AXIS);
	  m.add_molecule(d);
	  reg = reg.right_str(reg.length_i()-1);
	}
      if (reg.left_str(1) == "S")
	{
	  Molecule d = afm_find("accordion-accDot");
	  d.translate_axis(staff_space * 0.5 PT, Y_AXIS);
	  m.add_molecule(d);
	  reg = reg.right_str(reg.length_i()-1);
	}
    }
  /* ugh maybe try to use regular font for S.B. and B.B and only use one font
     for the rectangle */
  else if (sym == "SB")
    {
      Molecule r = afm_find("accordion-accSB");
      m.add_molecule(r);
    }
  else if (sym == "BB")
    {
      Molecule r = afm_find("accordion-accBB");
      m.add_molecule(r);
    }
  else if (sym == "OldEE")
    {
      Molecule r = afm_find("accordion-accOldEE");
      m.add_molecule(r);
    }
  else if (sym == "OldEES")
    {
      Molecule r = afm_find("accordion-accOldEES");
      m.add_molecule(r);
    }
  return m;  
}

