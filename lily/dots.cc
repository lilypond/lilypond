/*
  dots.cc -- implement Dots

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "dots.hh"
#include "molecule.hh"
#include "paper-def.hh"
#include "lookup.hh"

Dots::Dots ()
{
  no_dots_i_ =0;
  position_i_ =0;
}

void
Dots::do_post_processing ()
{
  if (!(position_i_ % 2))
    position_i_ ++;
}

Molecule* 
Dots::brew_molecule_p () const
{
  Symbol d = paper ()->lookup_l ()->dots (no_dots_i_);
  Molecule *out = new Molecule (Atom (d));
  Real inter_f = paper ()->internote_f ();
  out->translate (inter_f * position_i_, Y_AXIS);
  return out;
}

IMPLEMENT_IS_TYPE_B1(Dots, Item);
