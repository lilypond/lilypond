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



MAKE_SCHEME_SCORE_ELEMENT_CALLBACK(Breathing_sign,brew_molecule);

SCM 
Breathing_sign::brew_molecule (SCM smob)
{
  Score_element * sc = unsmob_element (smob);
  Staff_symbol_referencer_interface si (sc);
  
  Real space = si.staff_space();

  // todo: cfg'able.
  Interval i1(0, space / 6), i2(-space / 2, space / 2);
  Box b(i1, i2);

  return sc->lookup_l()->filledbox(b).create_scheme ();
}

Real
Breathing_sign::offset_callback (Score_element * b, Axis a)
{
  Score_element * me = (Score_element*)b;
  
  Real space = Staff_symbol_referencer_interface (b).staff_space();
  Direction d = Directional_element_interface (b). get ();
  if (!d)
    {
      d = UP;
      Directional_element_interface (me).set (d);
    }

  return 2.0 * space * d;
}

void
Breathing_sign::set_interface (Score_element *b)
{
  Staff_symbol_referencer_interface::set_interface  (b);
  b->add_offset_callback (Breathing_sign::offset_callback,Y_AXIS); 
}
