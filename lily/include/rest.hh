/*
  rest.hh -- declare Rest

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef REST_HH
#define REST_HH

#include "rhythmic-head.hh"

class  Rest : public Rhythmic_head
{
public:
  
  
  int position_i_;
  Rest ();
  void add_dots (Dots*);
protected:
  virtual void do_add_processing ();
  virtual Molecule * do_brew_molecule_p () const;
};
#endif // REST_HH
