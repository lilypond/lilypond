/*   
  g-text-item.cc -- implement G_text_item

  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "g-text-item.hh"

#include "molecule.hh"
#include "paper-def.hh"
#include "lookup.hh"

Molecule*
G_text_item::do_brew_molecule_p () const
{
  Molecule a= paper ()->lookup_l(0)->text (style_str_,text_str_); 
  return new Molecule (a);
}

G_text_item::G_text_item ()
{
  style_str_ = "roman";
}
