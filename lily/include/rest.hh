/*
  rest.hh -- declare Rest

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef REST_HH
#define REST_HH

#include "rhythmic-head.hh"

class  Rest : public Rhythmic_head
{
public:
  DECLARE_MY_RUNTIME_TYPEINFO;
  
  int position_i_;
  Rest ();
  void add (Dots*);
protected:
  virtual void do_add_processing ();
  virtual Molecule * brew_molecule_p () const;
};
#endif // REST_HH
