/*
  dots.hh -- declare Dots

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef DOTS_HH
#define DOTS_HH

#include "lily-proto.hh"
#include "lily-guile.hh"



class Dots
{
public:
  DECLARE_SCHEME_CALLBACK (quantised_position_callback, (SCM element, SCM axis));
  DECLARE_SCHEME_CALLBACK (brew_molecule, (SCM ));
};

#endif // DOTS_HH
