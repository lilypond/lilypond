/*   
  text-item.cc -- implement Text_item

  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
 */
#include <math.h>

#include "debug.hh"
#include "text-item.hh"
#include "paper-def.hh"
#include "font-interface.hh"
#include "staff-symbol-referencer.hh"
#include "staff-symbol-referencer.hh"
#include "main.hh"
#include "all-font-metrics.hh"
#include "afm.hh"
#include "lookup.hh"


/*

  TEXT: STRING
        | (MARKUP? TEXT+)
        ;

  HEAD: MARKUP-ITEM | (MARKUP-ITEM+)

  MARKUP-ITEM: PROPERTY | ABBREV | FONT-STYLE
  PROPERTY: (key . value)
  ABBREV: rows lines roman music bold italic named super sub text
   
*/

Molecule
Text_item::text2molecule (Grob *me, SCM text, SCM alist_chain) 
{
  if (gh_string_p (text))
    return string2molecule (me, text, alist_chain);
  else if (gh_pair_p (text))
    {
      /* urg, why not just do  this in markup_text2molecule ? */
      if (gh_string_p (ly_car (text)))
	return markup_text2molecule (me,
				     gh_append2 (scm_list_n (SCM_EOL,
							 SCM_UNDEFINED),
						 text),
				     alist_chain);
      /*
	Allow (faulty) texts that are in an extra list:
	#'(("foo"))
       */
      else if (scm_ilength (text) <= 1)
	return text2molecule (me, ly_car (text), alist_chain);
      else
	return markup_text2molecule (me, text, alist_chain);
    }
  return Molecule ();
}
	     
Molecule
Text_item::string2molecule (Grob *me, SCM text, SCM alist_chain)
{
  SCM style = ly_assoc_chain (ly_symbol2scm ("font-style"),
			      alist_chain);
  if (gh_pair_p (style) && gh_symbol_p (ly_cdr (style)))
    alist_chain = Font_interface::add_style (me, ly_cdr (style), alist_chain);

  Font_metric *fm = Font_interface::get_font (me, alist_chain);
  
  SCM lookup = ly_assoc_chain (ly_symbol2scm ("lookup"), alist_chain);
    
  Molecule mol;
  if (gh_pair_p (lookup) && ly_cdr (lookup) ==ly_symbol2scm ("name"))
    mol = lookup_character (me, fm, text);
  else
    mol = lookup_text (me, fm, text);
  
  return mol;
}

Molecule
Text_item::lookup_character (Grob *, Font_metric*fm, SCM char_name)
{
  return fm->find_by_name (ly_scm2string (char_name));
}


Molecule
Text_item::lookup_text (Grob *me, Font_metric*fm, SCM text)
{
  SCM list = scm_list_n (ly_symbol2scm ("text"), text, SCM_UNDEFINED);
  list = fontify_atom (fm, list);
  
  return Molecule (fm->text_dimension (ly_scm2string (text)), list);
}


/*
  TODO:

  DOCME.


  MARKUP_TEXT must be compound (may not be simple string.)
  
 */
Molecule
Text_item::markup_text2molecule (Grob *me, SCM markup_text,
				 SCM alist_chain)
{
  SCM sheet = me->paper_l ()->style_sheet_;
  SCM f = ly_cdr (scm_assoc (ly_symbol2scm ("markup-to-properties"), sheet));
  
  SCM markup = ly_car (markup_text);
  SCM text = ly_cdr (markup_text);

  SCM p = gh_cons (gh_call2 (f, sheet, markup), alist_chain);

  Real staff_space = Staff_symbol_referencer::staff_space (me);

  /*
    Line mode is default.
   */
  Axis axis = X_AXIS;

  SCM a = ly_assoc_chain (ly_symbol2scm ("axis"), p);
  if (gh_pair_p (a) && ly_axis_p (ly_cdr (a)))
    axis = (Axis)gh_scm2int (ly_cdr (a));

  Real baseline_skip = 0;
  SCM b = ly_assoc_chain (ly_symbol2scm ("baseline-skip"), p);
  if (gh_pair_p (b) && gh_number_p (ly_cdr (b)))
    baseline_skip = gh_scm2double (ly_cdr (b)) * staff_space;
  
  Real kern[2] = {0,0};

  SCM k = ly_assoc_chain (ly_symbol2scm ("kern"), p);
  if (gh_pair_p (k) && gh_number_p (ly_cdr (k)))
    kern[axis] = gh_scm2double (ly_cdr (k)) * staff_space;
			     
  Real raise = 0;
  SCM r = ly_assoc_chain (ly_symbol2scm ("raise"), p);
  if (gh_pair_p (r) && gh_number_p (ly_cdr (r)))
    raise = gh_scm2double (ly_cdr (r)) * staff_space;
  

  Interval extent;
  bool extent_b = false;
  SCM e = ly_assoc_chain (ly_symbol2scm ("extent"), p);
  if (gh_pair_p (e) && ly_number_pair_p (ly_cdr (e)))
    {
      extent = Interval (gh_scm2double (ly_cadr (e)) * staff_space,
		       gh_scm2double (ly_cddr (e)) * staff_space);
      extent_b = true;
    }

  Offset o (kern[X_AXIS], raise - kern[Y_AXIS]);
  
  Molecule mol = Lookup::filledbox (Box (Interval (0,0), Interval (0,0)));

  SCM cp = ly_deep_copy (p);
  if (raise)
    {
      SCM cr = ly_assoc_chain (ly_symbol2scm ("raise"), cp);
      scm_set_cdr_x (cr, gh_int2scm (0));
    }
  
  while (gh_pair_p (text))
    {
      Molecule m = text2molecule (me, ly_car (text), cp);

      if (!m.empty_b ())
	{
	  m.translate_axis (mol.extent (axis)[axis == X_AXIS ? RIGHT : DOWN]
			    - (axis == Y_AXIS ? baseline_skip : 0),
			    axis);
	  mol.add_molecule (m);
	}
      text = ly_cdr (text);
    }
  
  
  /* Set extend to markup requested value. */
  if (extent_b)
    {
      Box b = mol.extent_box ();
      SCM expr = mol.get_expr ();

      b[axis] = extent;
      mol = Molecule (b, expr);
    }
  
  mol.translate (o);
  
  return mol;
}

MAKE_SCHEME_CALLBACK (Text_item, brew_molecule, 1);
SCM 
Text_item::brew_molecule (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  
  SCM text = me->get_grob_property ("text");

  SCM properties = Font_interface::font_alist_chain (me);
  Molecule mol = Text_item::text2molecule (me, text, properties);

  SCM space = me->get_grob_property ("word-space");
  if (gh_number_p (space))
    {
      Molecule m;
      m.set_empty (false);
      mol.add_at_edge (X_AXIS, RIGHT, m, gh_scm2double (space)
		       * Staff_symbol_referencer::staff_space (me));
    }
  return mol.smobbed_copy (); 
}



ADD_INTERFACE (Text_item,"text-interface",
  "A scheme markup text",
  "text align baseline-skip lookup raise kern word-space magnify");
