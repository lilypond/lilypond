/*
  crescendo.hh -- declare Hairpin

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef CRESCENDO_HH
#define CRESCENDO_HH

#include "lily-guile.hh"
/**
  The hairpin symbol. 
 */
struct Hairpin
{
public:
  DECLARE_SCHEME_CALLBACK (brew_molecule, (SCM));
  static bool has_interface (Score_element*);
};

#endif // CRESCENDO_HH
