/*
  dots.hh -- declare Dots

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef DOTS_HH
#define DOTS_HH

#include "item.hh"

/**
  The dots to go with a notehead/rest.  A separate class, since they
  are a party in collision resolution.
  */
class Dots : public Item
{
protected:
  virtual Molecule * brew_molecule_p () const;
  virtual void do_post_processing ();
public:
  int no_dots_i_;
  int position_i_;

  DECLARE_MY_RUNTIME_TYPEINFO;
  Dots ();
};

#endif // DOTS_HH
