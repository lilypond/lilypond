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

MAKE_SCHEME_CALLBACK(Breathing_sign,brew_molecule,1);
SCM 
Breathing_sign::brew_molecule (SCM smob)
{
  Score_element * me = unsmob_element (smob);
  Real space = Staff_symbol_referencer::staff_space (me);

  // todo: cfg'able.
  Interval i1(0, space / 6), i2(-space / 2, space / 2);
  Box b(i1, i2);

  return Lookup::filledbox(b).smobbed_copy ();
}

MAKE_SCHEME_CALLBACK(Breathing_sign,offset_callback,2);
SCM
Breathing_sign::offset_callback (SCM element_smob, SCM )
{
  Score_element *me = unsmob_element (element_smob);
  
  Direction d = Directional_element_interface::get (me);
  if (!d)
    {
      d = UP;
      Directional_element_interface::set (me, d);
    }

  Real inter_f = Staff_symbol_referencer::staff_space (me)/2;
  int sz = Staff_symbol_referencer::line_count (me)-1;
  return gh_double2scm ( inter_f * sz * d);
}

void
Breathing_sign::set_interface (Score_element *b)
{
  Staff_symbol_referencer::set_interface  (b);

}
