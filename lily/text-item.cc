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
  String st = (style == SCM_UNDEFINED) ? "" : ly_scm2string (style);
  
  Molecule a= paper_l ()->lookup_l(0)->text (st, text_str_, paper_l ());

  return new Molecule (a);
}

void
Text_item::do_print () const
{
  DEBUG_OUT <<  "text= " << text_str_;
}

