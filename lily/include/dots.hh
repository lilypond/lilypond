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
  */
class Dots :
  public Item
{
protected:
  virtual Molecule do_brew_molecule () const;
  virtual void do_post_processing ();
public:
  
  Dots ();
};

#endif // DOTS_HH
