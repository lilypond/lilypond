/*
  breathing-sign.hh

  Copyright (c) 1999--2008 Michael Krause

  written for the GNU LilyPond music typesetter
*/

#ifndef BREATHING_SIGN_HH
#define BREATHING_SIGN_HH

#include "grob-interface.hh"
#include "lily-proto.hh"

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

  DECLARE_GROB_INTERFACE();
};

#endif // BREATHING_SIGN_HH
