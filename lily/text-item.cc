/*   
  text-item.cc -- implement Text_item

  source file of the GNU LilyPond music typesetter
  
 (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
    TEXT : STRING | (MARKUP SENTENCE)
    MARKUP: PROPERTY | ABBREV
    SENTENCE: TEXT | SENTENCE TEXT
    PROPERTY: (key . value)
    ABBREV: rows lines roman music bold italic named super sub text, or any font-style
 */

/*
  TODO:

  rewrite routines and syntax to be like

  TEXT: STRING
      | (head-expression* TEXT*)
      ;

  head-expression is a list, containing a tag and a variable number of
  arguments. If necessary, the number of arguments can be stored in a alist,

  '(
   (tag1 . argcount1)
   (tag2 . argcount2)

   ... etc
   
   )

   or even entries like

   (tag . (argcount function-to-handle-the-tag  ))
  
 */

Molecule
Text_item::text2molecule (Grob *me, SCM text, SCM alist_chain) 
{
  if (gh_string_p (text))
    return string2molecule (me, text, alist_chain);
  else if (gh_list_p (text))
    {
      if (!gh_pair_p (gh_car (text)) && gh_string_p (gh_car (text)))
	return string2molecule (me, gh_car (text), alist_chain);
      else
	return markup_sentence2molecule (me, text, alist_chain);
    }
  return Molecule ();
}
	     
Molecule
Text_item::string2molecule (Grob *me, SCM text, SCM alist_chain)
{
  SCM style = ly_assoc_chain (ly_symbol2scm ("font-style"),
			      alist_chain);
  if  (gh_pair_p (style) && gh_symbol_p (gh_cdr (style)))
    alist_chain = Font_interface::add_style (me, gh_cdr(style), alist_chain);

  Font_metric *fm = Font_interface::get_font (me, alist_chain);
  
  SCM lookup = ly_assoc_chain (ly_symbol2scm ("lookup"), alist_chain);
    
  Molecule mol;
  if (gh_pair_p (lookup) && gh_cdr (lookup) ==ly_symbol2scm ("name"))
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
   */

  SCM magnification = me->get_grob_property ("font-magnification");

  Font_metric* metric = 0;
  if (gh_number_p (magnification))
    {

      Real realmag = pow (1.2, gh_scm2int (magnification));
      metric = all_fonts_global_p->find_scaled (ly_scm2string (font_name), realmag);

      assert (false);
    }

#endif

  SCM list = gh_list (ly_symbol2scm ("text"), text, SCM_UNDEFINED);
  list = fontify_atom (fm, list);
  
  return Molecule (fm->text_dimension (ly_scm2string (text)), list);
}

Molecule
Text_item::markup_sentence2molecule (Grob *me, SCM markup_sentence,
				     SCM alist_chain)
{
  /*
    FIXME

    huh?
   */
  // return Molecule ();
  
  SCM sheet = me->paper_l ()->style_sheet_;
  SCM f = gh_cdr (scm_assoc (ly_symbol2scm ("markup-to-properties"), sheet));
  
  SCM markup = gh_car (markup_sentence);
  SCM sentence = gh_cdr (markup_sentence);
  
  SCM p = gh_cons  (gh_call2 (f, sheet, markup), alist_chain);

  Axis align = X_AXIS;
  SCM a = ly_assoc_chain (ly_symbol2scm ("align"), p);
  if (gh_pair_p (a) && gh_number_p (gh_cdr (a)))
    align = (Axis)gh_scm2int (gh_cdr (a));

  Real staff_space = Staff_symbol_referencer::staff_space (me);
  Real kern = 0;
  SCM k = ly_assoc_chain (ly_symbol2scm ("kern"), p);
  if (gh_pair_p (k) && gh_number_p (gh_cdr (k)))
    kern = gh_scm2double (gh_cdr (k)) * staff_space;
			     
  Real raise = 0;
  SCM r = ly_assoc_chain (ly_symbol2scm ("raise"), p);
  if (gh_pair_p (r) && gh_number_p (gh_cdr (r)))
    raise = gh_scm2double (gh_cdr (r)) * staff_space;

  Offset o (align == X_AXIS ? kern : 0,
	    (align == Y_AXIS ? - kern : 0) + raise);

  Molecule mol;
  while (gh_pair_p (sentence))
    {
      Molecule m = text2molecule (me, gh_car (sentence), p);
      if (!m.empty_b ())
	{
	  m.translate (o);
	  mol.add_at_edge (align, align == X_AXIS ? RIGHT : DOWN, m, 0);
	}
      sentence = gh_cdr (sentence);
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

