/*
  staffsym.hh -- declare Staff_symbol

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef STAFFSYM_HH
#define STAFFSYM_HH

#include "lily-guile.hh"

/**
  TODO: add stafflinethickness as parameter.
  */
class Staff_symbol 
{
public:
  static Real staff_space (Score_element*) ;
  static int steps_i(Score_element*) ;
  static int line_count (Score_element*);
  DECLARE_SCHEME_CALLBACK(brew_molecule, (SCM ));
  static bool has_interface (Score_element*);
  static void set_interface (Score_element*);
};
#endif // STAFFSYM_HH
