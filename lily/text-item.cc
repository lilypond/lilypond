/*   
  text-item.cc -- implement Text_item

  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "text-item.hh"
#include "debug.hh"
#include "molecule.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "staff-symbol-referencer.hh"

Molecule 
Text_item::do_brew_molecule () const
{
  SCM style = get_elt_property ("style");
  String st = gh_string_p (style) ?  ly_scm2string (style) : "";
  SCM txt = get_elt_property ("text");
  String t = gh_string_p (txt) ? ly_scm2string (txt) : "";

  Molecule mol = paper_l ()->lookup_l(0)->text (st, t, paper_l ());

  SCM s = get_elt_property ("word-space");
  if (gh_number_p (s))
    mol.dim_.interval_a_[X_AXIS][RIGHT] += gh_scm2double (s)
      * staff_symbol_referencer (this).staff_space ();
 
  return mol; 
}


