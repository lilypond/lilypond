/*   
  text-item.cc -- implement Text_item

  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "debug.hh"
#include "molecule.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "staff-symbol-referencer.hh"

struct Text_item
{
  static SCM brew_molecule (SCM);
};


MAKE_SCHEME_SCORE_ELEMENT_CALLBACK(Text_item,brew_molecule)

SCM 
Text_item::brew_molecule (SCM sm) 
{
  Score_element * s = unsmob_element (sm);
  
  SCM style = s->get_elt_property ("style");
  String st = gh_string_p (style) ?  ly_scm2string (style) : "";
  SCM txt = s-> get_elt_property ("text");
  String t = gh_string_p (txt) ? ly_scm2string (txt) : "";

  Molecule mol =  s->paper_l ()->lookup_l(0)->text (st, t, s->paper_l ());

  SCM space =  s->get_elt_property ("word-space");
  if (gh_number_p (space))
    {
      Molecule m;
      m.set_empty (false);
      mol.add_at_edge (X_AXIS, RIGHT, m, gh_scm2double (space)*
		       Staff_symbol_referencer_interface (s).staff_space ());
    }
  return mol.create_scheme (); 
}


