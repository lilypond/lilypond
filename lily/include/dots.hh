/*
  dots.hh -- declare Dots

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef DOTS_HH
#define DOTS_HH

#include "lily-proto.hh"
#include "lily-guile.hh"

extern SCM Dots_quantised_position_callback_proc;

/**
  The dots to go with a notehead/rest.  A separate class, since they
  are a party in collision resolution.

  properties:

  dot-count -- number of dots.

  
  */
class Dots			// interface
{
public:
  static SCM quantised_position_callback (SCM element, SCM axis);
  static SCM brew_molecule (SCM);
};

#endif // DOTS_HH
