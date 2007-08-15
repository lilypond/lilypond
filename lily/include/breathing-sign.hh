/*
  breathing-sign.hh

  Copyright (c) 1999--2007 Michael Krause

  written for the GNU LilyPond music typesetter
*/

#ifndef BREATHING_SIGN_HH
#define BREATHING_SIGN_HH

#include "lily-guile.hh"

class Grob;

/*
  breathing sign (apostrophe within staff, not the comma above staff
  type)
*/
class Breathing_sign
{
public:
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (railtracks, (SCM));
  DECLARE_SCHEME_CALLBACK (divisio_minima, (SCM));
  DECLARE_SCHEME_CALLBACK (divisio_maior, (SCM));
  DECLARE_SCHEME_CALLBACK (divisio_maxima, (SCM));
  DECLARE_SCHEME_CALLBACK (finalis, (SCM));
  DECLARE_SCHEME_CALLBACK (offset_callback, (SCM element));

  static bool has_interface (Grob *);
};

#endif // BREATHING_SIGN_HH
