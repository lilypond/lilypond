/*
  breathing-sign.hh

  Copyright (C) 1999 Michael Krause

  written for the GNU LilyPond music typesetter

*/

#ifndef BREATHING_SIGN_HH
#define BREATHING_SIGN_HH

#include "lily-guile.hh"

/*
  breathing sign (apostrophe within staff, not the comma above staff
  type)
*/
class Breathing_sign
{
public:
  static SCM brew_molecule (SCM);
  static Real offset_callback (Score_element *, Axis);
  static void set_interface (Score_element*);
  static bool has_interface (Score_element*);
};

#endif // BREATHING_SIGN_HH
