/*
  staff-symbol.hh -- declare Staff_symbol

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef STAFF_SYMBOL_HH
#define STAFF_SYMBOL_HH

#include "lily-guile.hh"

/**
  TODO: add stafflinethickness as parameter.
  */
class Staff_symbol 
{
public:
  static Real staff_space (Grob*) ;
  static int steps_i (Grob*) ;
  static int line_count (Grob*);
  DECLARE_SCHEME_CALLBACK (brew_molecule, (SCM ));
  static bool has_interface (Grob*);
  static void set_interface (Grob*);
};
#endif // STAFF_SYMBOL_HH
