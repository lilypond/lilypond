/*
  breathing-sign.hh

  Copyright (C) 1999 Michael Krause

  written for the GNU LilyPond music typesetter

*/

#ifndef BREATHING_SIGN_HH
#define BREATHING_SIGN_HH

#include "item.hh"
#include "parray.hh"

class Breathing_sign : public Item
{
public:
  VIRTUAL_COPY_CONS(Score_element);
  Breathing_sign ();
protected:
  virtual void do_post_processing ();
  virtual Molecule* do_brew_molecule_p () const;
};



#endif // BREATHING_SIGN_HH
