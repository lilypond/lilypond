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
  Molecule a= paper_l ()->lookup_l(0)->text (style_str_,text_str_, paper_l ()); 

  return new Molecule (a);
}

Text_item::Text_item ()
{
  style_str_ = "roman";
}

void
Text_item::do_print () const
{
  DEBUG_OUT <<  "text= " << text_str_;
}
