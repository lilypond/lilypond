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
#include "atom.hh"
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


/*
  build a ledger line for small pieces.
 */
Molecule
Lookup::ledger_line (Interval xwid) const
{
  Drul_array<Molecule> endings;
  endings[LEFT] = afm_find ("noteheads-ledgerending");
  Molecule * e = &endings[LEFT];
  endings[RIGHT] = *e;
  
  Real thick = e->dim_[Y_AXIS].length();
  Real len = e->dim_[X_AXIS].length () - thick;

  Molecule total;
  Direction d = LEFT;
  do {
    endings[d].translate_axis (xwid[d] - endings[d].dim_[X_AXIS][d], X_AXIS);
    total.add_molecule (endings[d]);    
  } while ((flip(&d)) != LEFT);

  Real xpos = xwid [LEFT] + len;

  while (xpos + len + thick /2 <= xwid[RIGHT])
    {
      e->translate_axis (len, X_AXIS);
      total.add_molecule (*e);
      xpos += len;
    }

  return total;
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
	  warning (_f ("Can't find font: `%s'", font_name_));
	  warning (_f ("(search path `%s')", global_path.str ().ch_C()));
	  error (_ ("Aborting"));
	}
    }
  AFM_CharMetricInfo const *cm = afm_l_->find_char_metric (s, warn);
  Molecule m;
  if (!cm)
    {
      m.set_empty (false);
      return m;
    }
  
  Atom* at = new Atom (gh_list (ly_symbol2scm ("char"),
		    gh_int2scm (cm->code),
		    SCM_UNDEFINED));

  at->fontify (afm_l_);
  m.dim_ = afm_bbox_to_box (cm->charBBox);
  m.add_atom (at->self_scm_);
  return m;
}

Molecule
Lookup::simple_bar (String type, Real h, Paper_def* paper_l) const
{
  SCM thick = ly_symbol2scm (("barthick_" + type).ch_C());
  Real w = 0.0;
  
  if (paper_l->scope_p_->elem_b (thick))
    {
      w = paper_l->get_realvar (thick);
    }
  else
    {
      programming_error ("No bar thickness set ! ");
      w = 1 PT;
    }
  return filledbox (Box (Interval(0,w), Interval(-h/2, h/2)));
}

  
Molecule
Lookup::bar (String str, Real h, Paper_def *paper_l) const
{
  if (str == "bracket")
    return staff_bracket (h, paper_l);
  else if (str == "brace")
    {
      Real staffht  = paper_l->get_var ("staffheight");
      return staff_brace (h,staffht);
    }
  Real kern = paper_l->get_var ("bar_kern");
  Real thinkern = paper_l->get_var ("bar_thinkern");

  Molecule thin = simple_bar ("thin", h, paper_l);
  Molecule thick = simple_bar ("thick", h, paper_l);
  Molecule colon = afm_find ("dots-repeatcolon", paper_l);  

  Molecule m;
  
  if (str == "")
    {
      return fill (Box (Interval(0, 0), Interval (-h/2, h/2)));
    }
  if (str == "scorepostbreak")
    {
      return simple_bar ("score", h, paper_l);
    }
  else if (str == "|")
    {
      return thin;
    }
  else if (str == "|.")
    {
      m.add_at_edge (X_AXIS, LEFT, thick, 0);      
      m.add_at_edge (X_AXIS, LEFT, thin, kern);
    }
  else if (str == ".|")
    {
      m.add_at_edge (X_AXIS, RIGHT, thick, 0);
      m.add_at_edge (X_AXIS, RIGHT, thin, kern);
    }
  else if (str == ":|")
    {
      m.add_at_edge (X_AXIS, LEFT, thick, 0);
      m.add_at_edge (X_AXIS, LEFT, thin, kern);
      m.add_at_edge (X_AXIS, LEFT, colon, kern);      
    }
  else if (str == "|:")
    {
      m.add_at_edge (X_AXIS, RIGHT, thick, 0);
      m.add_at_edge (X_AXIS, RIGHT, thin, kern);
      m.add_at_edge (X_AXIS, RIGHT, colon, kern);      
    }
  else if (str == ":|:")
    {
      m.add_at_edge (X_AXIS, LEFT, thick, thinkern);
      m.add_at_edge (X_AXIS, LEFT, colon, kern);      
      m.add_at_edge (X_AXIS, RIGHT, thick, kern);
      m.add_at_edge (X_AXIS, RIGHT, colon, kern);      
    }
  else if (str == ".|.")
    {
      m.add_at_edge (X_AXIS, LEFT, thick, thinkern);
      m.add_at_edge (X_AXIS, RIGHT, thick, kern);      
    }
  else if (str == "||")
    {
      m.add_at_edge (X_AXIS, RIGHT, thin, 0);
      m.add_at_edge (X_AXIS, RIGHT, thin, thinkern);      
    }

  return m;
}

Molecule 
Lookup::beam (Real slope, Real width, Real thick) const
{
  Real height = slope * width; 
  Real min_y = (0 <? height) - thick/2;
  Real max_y = (0 >? height) + thick/2;

  
  Molecule m;
  m.dim_[X_AXIS] = Interval (0, width);
  m.dim_[Y_AXIS] = Interval (min_y, max_y);

  
  Atom *at = new Atom
    (gh_list (ly_symbol2scm ("beam"),
	      gh_double2scm (width),
	      gh_double2scm (slope),
	      gh_double2scm (thick),
	      SCM_UNDEFINED));

  m.add_atom (at->self_scm_);
  return m;
}



/*
  FIXME.
 */
Molecule
Lookup::dashed_slur (Bezier b, Real thick, Real dash) const
{
  SCM l = SCM_EOL;
  for (int i= 4; i -- ;)
    {
      l = gh_cons (ly_offset2scm (b.control_[i]), l);
    }

  Atom *at = new Atom(gh_list (ly_symbol2scm ("dashed-slur"),
			       gh_double2scm (thick), 
			       gh_double2scm (dash),
			       ly_quote_scm (l),
			       SCM_UNDEFINED));
  Molecule m;
  m.add_atom (at->self_scm_);
  return m;
}




Molecule
Lookup::fill (Box b) const
{
  Molecule m;
  m.dim_ = b;
  return m;
}


Molecule
Lookup::filledbox (Box b ) const
{
  Molecule m;
  
  Atom* at  = new Atom(gh_list (ly_symbol2scm ("filledbox"),
		     gh_double2scm (-b[X_AXIS][LEFT]),
		     gh_double2scm (b[X_AXIS][RIGHT]),		       
		     gh_double2scm (-b[Y_AXIS][DOWN]),
		     gh_double2scm (b[Y_AXIS][UP]),		       
		     SCM_UNDEFINED));

  m.dim_ = b;
  m.add_atom (at->self_scm_);
  return m;
}

/*
   TODO: THIS IS UGLY.  Since the user has direct access to TeX
   strings, we try some halfbaked attempt to detect TeX trickery.
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
Lookup::text (String style, String text, Paper_def *paper_l) const
{
  Molecule m;
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
  SCM l = ly_eval_str (("(style-to-cmr \"" + style + "\")").ch_C());
  if (l != SCM_BOOL_F)
    {
      style = ly_scm2string (gh_cdr(l)) +to_str  ((int)font_h);
    }

  

  Font_metric* metric_l = 0;

  if (font_mag)
    metric_l = all_fonts_global_p->find_scaled (style, font_mag);
  else
    metric_l = all_fonts_global_p->find_font (style);
  
  
  if (output_global_ch == "tex")
    text = sanitise_TeX_string  (text);
  else if (output_global_ch == "ps")
    text = sanitise_PS_string (text);
    
  m.dim_ = metric_l->text_dimension (text);
  
  Atom *at = new Atom (gh_list (ly_symbol2scm ("text"),
		     ly_str02scm (text.ch_C()),
		     SCM_UNDEFINED));
  at->fontify (metric_l);
  
  m.add_atom (at->self_scm_);
  return m;
}
  


Molecule
Lookup::staff_brace (Real y, int staff_size) const
{
  Molecule m;

  Real step  = 1.0;
  int minht  = 2 * staff_size;
  int maxht = 7 *  minht;
  int idx = int (((maxht - step) <? y - minht) / step);
  idx = idx >? 0;


  String nm = String ("feta-braces" + to_str (staff_size));
  SCM e =gh_list (ly_symbol2scm ("char"), gh_int2scm (idx), SCM_UNDEFINED);
  Atom *at = new Atom (e);

  at->fontify (all_fonts_global_p->find_font (nm));
  
  m.dim_[Y_AXIS] = Interval (-y/2,y/2);
  m.dim_[X_AXIS] = Interval (0,0);
  m.add_atom (at->self_scm_);
  return m;
}


/*
  Make a smooth curve along the points 
 */
Molecule
Lookup::slur (Bezier curve, Real curvethick, Real linethick) const
{
  Real alpha = (curve.control_[3] - curve.control_[0]).arg ();
  Bezier back = curve;

  back.reverse ();
  back.control_[1] += curvethick * complex_exp (Offset (0, alpha + M_PI/2));
  back.control_[2] += curvethick * complex_exp (Offset (0, alpha + M_PI/2));  

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
  for (int i= 8; i--;  )
    {
      list = gh_cons (scontrols[indices[i]], list);
    }
  
  
  Atom *at = new Atom (gh_list (ly_symbol2scm ("bezier-sandwich"),
		     ly_quote_scm (list),
		     gh_double2scm (linethick),
		     SCM_UNDEFINED));

  Molecule m; 
  m.dim_[X_AXIS] = curve.extent (X_AXIS);
  m.dim_[Y_AXIS] = curve.extent (Y_AXIS);
  m.add_atom (at->self_scm_);
  return m;
}

Molecule
Lookup::staff_bracket (Real height, Paper_def* paper_l) const
{
  Molecule m;
  Atom *at = new Atom  ( gh_list (ly_symbol2scm ("bracket"),
		      gh_double2scm (paper_l->get_var("bracket_arch_angle")),
		      gh_double2scm (paper_l->get_var("bracket_arch_width")),
		      gh_double2scm (paper_l->get_var("bracket_arch_height")),
		      gh_double2scm (paper_l->get_var("bracket_width")),
		      gh_double2scm (height),
		      gh_double2scm (paper_l->get_var("bracket_arch_thick")),
		      gh_double2scm (paper_l->get_var("bracket_thick")),
  		      SCM_UNDEFINED));
  
  m.add_atom (at->self_scm_);				 
  m.dim_[Y_AXIS] = Interval (-height/2,height/2);
  m.dim_[X_AXIS] = Interval (0,4 PT);

  m.translate_axis (- 4. / 3. * m.dim_[X_AXIS].length (), X_AXIS);
  return m;
}

Molecule
Lookup::volta (Real h, Real w, Real thick, bool vert_start, bool vert_end) const
{
  Molecule m; 

  Atom *at = new Atom(gh_list (ly_symbol2scm ("volta"),
		     gh_double2scm (h),
		     gh_double2scm (w),
		     gh_double2scm (thick),
		     gh_int2scm (vert_start),
		     gh_int2scm (vert_end),
		     SCM_UNDEFINED));

  m.dim_[Y_AXIS] = Interval (- h/2, h/2);
  m.dim_[X_AXIS] = Interval (0, w);

  m.add_atom (at->self_scm_);
  return m;
}

Molecule
Lookup::accordion (SCM s, Real staff_space) const
{
  Molecule m;
  String sym = ly_scm2string(gh_car (s));
  String reg = ly_scm2string(gh_car (gh_cdr(s)));

  if (sym == "Discant")
    {
      Molecule r = afm_find("scripts-accDiscant");
      m.add_molecule(r);
      if (reg.left_str(1) == "F")
	{
	  Molecule d = afm_find("scripts-accDot");
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
	  Molecule d = afm_find("scripts-accDot");
	  d.translate_axis(staff_space * 1.5 PT, Y_AXIS);
	  m.add_molecule(d);
	}
      if (eflag & 0x04)
	{
	  Molecule d = afm_find("scripts-accDot");
	  d.translate_axis(staff_space * 1.5 PT, Y_AXIS);
	  d.translate_axis(0.8 * staff_space PT, X_AXIS);
	  m.add_molecule(d);
	}
      if (eflag & 0x01)
	{
	  Molecule d = afm_find("scripts-accDot");
	  d.translate_axis(staff_space * 1.5 PT, Y_AXIS);
	  d.translate_axis(-0.8 * staff_space PT, X_AXIS);
	  m.add_molecule(d);
	}
      if (reg.left_str(2) == "SS")
	{
	  Molecule d = afm_find("scripts-accDot");
	  d.translate_axis(0.5 * staff_space PT, Y_AXIS);
	  d.translate_axis(0.4 * staff_space PT, X_AXIS);
	  m.add_molecule(d);
	  d.translate_axis(-0.8 * staff_space PT, X_AXIS);
	  m.add_molecule(d);
	  reg = reg.right_str(reg.length_i()-2);
	}
      if (reg.left_str(1) == "S")
	{
	  Molecule d = afm_find("scripts-accDot");
	  d.translate_axis(0.5 * staff_space PT, Y_AXIS);
	  m.add_molecule(d);
	  reg = reg.right_str(reg.length_i()-1);
	}
    }
  else if (sym == "Freebase")
    {
      Molecule r = afm_find("scripts-accFreebase");
      m.add_molecule(r);
      if (reg.left_str(1) == "F")
	{
	  Molecule d = afm_find("scripts-accDot");
	  d.translate_axis(staff_space * 1.5 PT, Y_AXIS);
	  m.add_molecule(d);
	  reg = reg.right_str(reg.length_i()-1);
	}
      if (reg == "E")
	{
	  Molecule d = afm_find("scripts-accDot");
	  d.translate_axis(staff_space * 0.5 PT, Y_AXIS);
	  m.add_molecule(d);
	}
    }
  else if (sym == "Bayanbase")
    {
      Molecule r = afm_find("scripts-accBayanbase");
      m.add_molecule(r);
      if (reg.left_str(1) == "T")
	{
	  Molecule d = afm_find("scripts-accDot");
	  d.translate_axis(staff_space * 2.5 PT, Y_AXIS);
	  m.add_molecule(d);
	  reg = reg.right_str(reg.length_i()-1);
	}
      /* include 4' reed just for completeness. You don't want to use this. */
      if (reg.left_str(1) == "F")
	{
	  Molecule d = afm_find("scripts-accDot");
	  d.translate_axis(staff_space * 1.5 PT, Y_AXIS);
	  m.add_molecule(d);
	  reg = reg.right_str(reg.length_i()-1);
	}
      if (reg.left_str(2) == "EE")
	{
	  Molecule d = afm_find("scripts-accDot");
	  d.translate_axis(staff_space * 0.5 PT, Y_AXIS);
	  d.translate_axis(0.4 * staff_space PT, X_AXIS);
	  m.add_molecule(d);
	  d.translate_axis(-0.8 * staff_space PT, X_AXIS);
	  m.add_molecule(d);
	  reg = reg.right_str(reg.length_i()-2);
	}
      if (reg.left_str(1) == "E")
	{
	  Molecule d = afm_find("scripts-accDot");
	  d.translate_axis(staff_space * 0.5 PT, Y_AXIS);
	  m.add_molecule(d);
	  reg = reg.right_str(reg.length_i()-1);
	}
    }
  else if (sym == "Stdbase")
    {
      Molecule r = afm_find("scripts-accStdbase");
      m.add_molecule(r);
      if (reg.left_str(1) == "T")
	{
	  Molecule d = afm_find("scripts-accDot");
	  d.translate_axis(staff_space * 3.5 PT, Y_AXIS);
	  m.add_molecule(d);
	  reg = reg.right_str(reg.length_i()-1);
	}
      if (reg.left_str(1) == "F")
	{
	  Molecule d = afm_find("scripts-accDot");
	  d.translate_axis(staff_space * 2.5 PT, Y_AXIS);
	  m.add_molecule(d);
	  reg = reg.right_str(reg.length_i()-1);
	}
      if (reg.left_str(1) == "M")
	{
	  Molecule d = afm_find("scripts-accDot");
	  d.translate_axis(staff_space * 2 PT, Y_AXIS);
	  d.translate_axis(staff_space PT, X_AXIS);
	  m.add_molecule(d);
	  reg = reg.right_str(reg.length_i()-1);
	}
      if (reg.left_str(1) == "E")
	{
	  Molecule d = afm_find("scripts-accDot");
	  d.translate_axis(staff_space * 1.5 PT, Y_AXIS);
	  m.add_molecule(d);
	  reg = reg.right_str(reg.length_i()-1);
	}
      if (reg.left_str(1) == "S")
	{
	  Molecule d = afm_find("scripts-accDot");
	  d.translate_axis(staff_space * 0.5 PT, Y_AXIS);
	  m.add_molecule(d);
	  reg = reg.right_str(reg.length_i()-1);
	}
    }
  /* ugh maybe try to use regular font for S.B. and B.B and only use one font
     for the rectangle */
  else if (sym == "SB")
    {
      Molecule r = afm_find("scripts-accSB");
      m.add_molecule(r);
    }
  else if (sym == "BB")
    {
      Molecule r = afm_find("scripts-accBB");
      m.add_molecule(r);
    }
  else if (sym == "OldEE")
    {
      Molecule r = afm_find("scripts-accOldEE");
      m.add_molecule(r);
    }
  else if (sym == "OldEES")
    {
      Molecule r = afm_find("scripts-accOldEES");
      m.add_molecule(r);
    }
  return m;  
}

