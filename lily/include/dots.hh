/*
  dots.hh -- declare Dots

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef DOTS_HH
#define DOTS_HH

#include "item.hh"


/**
  The dots to go with a notehead/rest.  A separate class, since they
  are a party in collision resolution.

  properties:

  dot-count -- number of dots.

  
  */
class Dots :  public Item
{
public:
  SCM member_brew_molecule () const;
  SCM member_after_line_breaking ();
  static SCM after_line_breaking (SCM);
  static SCM brew_molecule (SCM);

  Dots (SCM);
};

#endif // DOTS_HH
