/*
  dots.hh -- declare Dots

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef DOTS_HH
#define DOTS_HH

#include "item.hh"
#include "staff-symbol-referencer.hh"
#include "directional-element.hh"

/**
  The dots to go with a notehead/rest.  A separate class, since they
  are a party in collision resolution.
  */
class Dots : public Item, public Staff_symbol_referencer,
  public Directional_element
{
protected:
  virtual Molecule * do_brew_molecule_p () const;
  virtual void do_post_processing ();
public:
  int dots_i_;
  
  Dots ();
};

#endif // DOTS_HH
