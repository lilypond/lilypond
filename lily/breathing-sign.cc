/*
  breathing_sign.cc -- implement Breathing_sign

  Copyright (C) 1999 Michael Krause

  written for the GNU LilyPond music typesetter

TODO: --> see breathing-sign-engraver.cc

*/

#include "breathing-sign.hh"
#include "string.hh"
#include "molecule.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "debug.hh"
#include "dimensions.hh"
#include "direction.hh"


Breathing_sign::Breathing_sign ()
{
  set_direction (UP);
  set_elt_property ("breakable", SCM_BOOL_T);
}



Molecule*
Breathing_sign::do_brew_molecule_p () const
{
  Real dl = staff_line_leading_f();
  Interval i1(0, dl / 6), i2(-dl / 2, dl / 2);
  Box b(i1, i2);

  Molecule *output = new Molecule (lookup_l()->filledbox(b));

  return output;
}

void
Breathing_sign::do_post_processing()
{
  Real dl = staff_line_leading_f();

  translate_axis(2.0 * dl * get_direction (), Y_AXIS);
}

