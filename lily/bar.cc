/*
  bar.cc -- implement Bar

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include <math.h>

#include "main.hh"
#include "dimensions.hh"
#include "dimension-cache.hh"
#include "bar.hh"
#include "string.hh"
#include "molecule.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "debug.hh"
#include "all-font-metrics.hh"

Bar::Bar ()
{
  set_elt_property ("breakable", SCM_BOOL_T);
}


Real
Bar::get_bar_size () const
{
  // Never called!
  return 0;
}


Molecule 
Bar::do_brew_molecule () const
{
  SCM s = get_elt_property ("glyph");
  if (gh_string_p (s))
    {
      String str  =ly_scm2string (s);
      if (str == "bracket")
	return staff_bracket (get_bar_size ());
      else if  (str == "brace")
	return staff_brace (get_bar_size ());
      else
	return compound_barline (str, get_bar_size ());
    }
  return Molecule ();
}


Molecule
Bar::staff_bracket (Real height) const 
{
  Paper_def* p= paper_l ();
  SCM at = gh_list (ly_symbol2scm ("bracket"),
		    gh_double2scm (p->get_var("bracket_arch_angle")),
		    gh_double2scm (p->get_var("bracket_arch_width")),
		    gh_double2scm (p->get_var("bracket_arch_height")),
		    gh_double2scm (p->get_var("bracket_width")),
		    gh_double2scm (height),
		    gh_double2scm (p->get_var("bracket_arch_thick")),
		    gh_double2scm (p->get_var("bracket_thick")),
		    SCM_UNDEFINED);

  Real staff_space = p->get_var ("interline");
  Box b (Interval (0, 1.5 * staff_space), Interval (-height/2,height/2));
  Molecule mol (b, at);
  
  mol.translate_axis (- mol.dim_[X_AXIS].length () / 2, X_AXIS);
  return mol;
}

Molecule
Bar::compound_barline (String str, Real h) const
{
  Real kern = paper_l()->get_var ("bar_kern");
  Real thinkern = paper_l()->get_var ("bar_thinkern");

  Molecule thin = simple_barline (paper_l()->get_var ("barthick_thin"), h);
  Molecule thick = simple_barline (paper_l()->get_var ("barthick_thick"), h);
  Molecule colon = lookup_l ()->afm_find ("dots-repeatcolon");  

  Molecule m;
  
  if (str == "")
    {
      return lookup_l ()->blank (Box (Interval(0, 0), Interval (-h/2, h/2)));
    }
  if (str == "scorepostbreak")
    {
      return simple_barline (paper_l ()->get_var ("barthick_score"), h);
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

/*
  ugh. Suck me plenty.
 */
Molecule
Bar::staff_brace (Real y)  const
{
  Real staffht  = paper_l ()->get_var ("staffheight");
  int staff_size  = int (rint (staffht ));

  // URG
  Real step  = 1.0;
  int minht  = 2 * staff_size;
  int maxht = 7 *  minht;
  int idx = int (((maxht - step) <? y - minht) / step);
  idx = idx >? 0;

  SCM l = ly_eval_str ("(style-to-cmr \"brace\")");
  String nm = "feta-braces";
  if (l != SCM_BOOL_F)
    nm = ly_scm2string (gh_cdr (l));
  nm += to_str (staff_size);
  SCM e =gh_list (ly_symbol2scm ("char"), gh_int2scm (idx), SCM_UNDEFINED);
  SCM at = (e);

  at = fontify_atom (all_fonts_global_p->find_font (nm), at);
  
  Box b ( Interval (-y/2,y/2),
	   Interval (0,0));
  return Molecule(b, at);
}
  

Molecule
Bar::simple_barline (Real w, Real h) const
{
  return lookup_l ()->filledbox (Box (Interval(0,w), Interval(-h/2, h/2)));
}


void
Bar::before_line_breaking ()
{
  SCM g = get_elt_property ("glyph");
  Direction bsd = break_status_dir ();
  if (gh_string_p (g))
    {
      if (bsd)
	{
	  SCM breakdir = gh_int2scm (bsd);
	  g = scm_eval (gh_list (ly_symbol2scm ("break-barline"),
				 g,
				 breakdir,
				 SCM_UNDEFINED));
	}
    }
  else
    {
      g = SCM_UNDEFINED;
    }
  
  if (!gh_string_p (g))
    {
      set_elt_property ("transparent", SCM_BOOL_T);
      set_empty (X_AXIS);      
    }
  else
    set_elt_property ("glyph", g);
}
  

