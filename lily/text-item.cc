/*   
  text-item.cc -- implement Text_item

  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
#if 0
  /*
    Fixme; should be done differently, move to font-interface?

    differently -- how/why?
   */

  SCM magnification = me->get_grob_property ("font-magnification");

  Font_metric* metric = 0;
  if (gh_number_p (magnification))
    {

      Real realmag = pow (1.2, gh_scm2int (magnification));
      metric = all_fonts_global_p->find_scaled (ly_scm2string (font_name), realmag);

      assert (false);
    }
#else
  SCM magnification = me->get_grob_property ("font-magnification");

  if (gh_number_p (magnification) && gh_scm2double (magnification) > 1)
    programming_error ("font-magnification disabled");
#endif
  

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
  if (gh_pair_p (a) && isaxis_b (ly_cdr (a)))
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

  Offset o (0, (axis == Y_AXIS ? - kern[axis] : 0));

  Molecule mol;
  while (gh_pair_p (text))
    {
   
      Molecule m = text2molecule (me, ly_car (text), p);

      /*
	TODO: look at padding?
	
	Look ahead here for kern and raise.

	(cols "foo" ((raise . 1) "bar"))
	(cols "foo" ((bold (raise . 1)) "bar"))

	When constructing the molecule for bar, all normal extra
	properties found, such as bold, are used for the construction
	of bar's molecule.  But for kern or raise, it seems that we're
	too late then, translating bar's molecule has no effect (or
	maybe the effect of translating gets nullified when bar's
	molecule is `added_to_edge' of the molecule for foo?)

	So, while constructing foo's molecule, we look ahead for the
	raise of bar.  The HEAD of the description of bar may be a
	single property, or a list, so we must check that too.
      */
	
      SCM next_p = SCM_EOL;
      if (gh_pair_p (ly_car (text)))
	next_p = scm_list_n (gh_call2 (f, sheet, ly_caar (text)), SCM_UNDEFINED);
      SCM next_k = ly_assoc_chain (ly_symbol2scm ("kern"), next_p);
      Real next_kern = kern[axis];
      if (gh_pair_p (next_k) && gh_number_p (ly_cdr (next_k)))
	next_kern = gh_scm2double (ly_cdr (next_k)) * staff_space;

      SCM next_r = ly_assoc_chain (ly_symbol2scm ("raise"), next_p);
      Real next_raise = 0;
      if (gh_pair_p (next_r) && gh_number_p (ly_cdr (next_r)))
	next_raise = gh_scm2double (ly_cdr (next_r)) * staff_space;

      o[Y_AXIS] = next_raise;

      if (!m.empty_b ())
	{
	  m.translate (o);
	  if (mol.empty_b ())
	    mol = m;
	  else
	    {
	      if (axis == Y_AXIS && baseline_skip)
		next_kern += baseline_skip - m.extent (Y_AXIS)[UP];
	      mol.add_at_edge (axis, axis == X_AXIS ? RIGHT : DOWN, m, next_kern);
	    }
	}
      text = ly_cdr (text);
    }
  
  if (extent_b)
    {
#if 0
      /* Hmm, we're not allowed to change a Molecule's extent? */
      mol.dim_[axis] = extent;
      Molecule::ly_set_molecule_extent_x (mol.self_scm (), gh_int2scm (axis),
					  ly_cdr (e));
#else
      // burp: unpredictable names, these...
      Box b = mol.extent_box ();
      SCM expr = mol.get_expr ();

      b[axis] = extent;
      mol = Molecule (b, expr);
#endif
	}  
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

