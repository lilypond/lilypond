/*
  bow.hh -- declare Bow

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef BOW_HH
#define BOW_HH

#include "spanner.hh"

/**
  Base class for anything that looks like a slur.
  Anybody with a better name?

  UGH. Fixme.  Should junk

    dy_f_drul_ , dx_f_drul_
  
  */
class Bow : public Spanner
{
protected:
};
#error

#endif // BOW_HH
