/*
  breathing_sign.cc -- implement Breathing_sign

  Copyright (C) 1999 Michael Krause

  written for the GNU LilyPond music typesetter

  TODO: --> see breathing-sign-engraver.cc

*/
#include "staff-symbol-referencer.hh"
#include "directional-element-interface.hh"

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
  set_elt_property ("breakable", SCM_BOOL_T);
}

Molecule 
Breathing_sign::do_brew_molecule () const
{
  Staff_symbol_referencer_interface si (this);
  
  Real space = si.staff_space();
  Interval i1(0, space / 6), i2(-space / 2, space / 2);
  Box b(i1, i2);

  return lookup_l()->filledbox(b);
}

void
Breathing_sign::after_line_breaking ()
{
  Real space = staff_symbol_referencer (this).staff_space();
  Direction d = directional_element (this). get ();
  if (!d)
    {
      d = UP;
      directional_element(this).set (d);
    }

  translate_axis(2.0 * space * d, Y_AXIS);
}

