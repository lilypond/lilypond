/*
  crescendo.hh -- declare Crescendo

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef CRESCENDO_HH
#define CRESCENDO_HH

#include "lily-guile.hh"
/**
  The hairpin symbol. (cresc)

  (normal spanner?)
 */
struct Crescendo
{
public:
  static SCM brew_molecule (SCM);
  static void set_interface(Score_element*);
  static bool has_interface (Score_element*);
};

#endif // CRESCENDO_HH
