/*   
  text-item.cc -- implement Text_item

  source file of the GNU LilyPond music typesetter
  
  (c) 1998--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "text-item.hh"
#include "debug.hh"
#include "molecule.hh"
#include "paper-def.hh"
#include "lookup.hh"

Molecule*
Text_item::do_brew_molecule_p () const
{
  SCM style = get_elt_property ("style");
  String st = gh_string_p (style) ?  ly_scm2string (style) : "";
  SCM txt = get_elt_property ("text");
  String t = gh_string_p (txt) ? ly_scm2string (txt) : "";
  
  Molecule a= paper_l ()->lookup_l(0)->text (st, t, paper_l ());

  return new Molecule (a);
}


