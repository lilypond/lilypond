/*   
  g-text-item.cc -- implement G_text_item

  source file of the GNU LilyPond music typesetter
  
  (c) 1998--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "g-text-item.hh"
#include "debug.hh"
#include "molecule.hh"
#include "paper-def.hh"
#include "lookup.hh"

Molecule*
G_text_item::do_brew_molecule_p () const
{
  Molecule a= paper_l ()->lookup_l(0)->text (style_str_,text_str_); 

  return new Molecule (a);
}

G_text_item::G_text_item ()
{
  style_str_ = "roman";
}

void
G_text_item::do_print () const
{
  DOUT <<  "text= " << text_str_;
}
