/*
  ligature-bracket.hh -- part of GNU LilyPond

  source file of the GNU LilyPond music typesetter
  
  (c) 2002 Juergen Reuter <reuter@ipd.uka.de>
*/

#ifndef LIGATURE_BRACKET_HH
#define LIGATURE_BRACKET_HH

#include "lily-proto.hh"
#include "lily-guile.hh"

struct Ligature_bracket
{
  DECLARE_SCHEME_CALLBACK (brew_molecule, (SCM ));
  static bool has_interface (Grob*);
};

#endif /* LIGATURE_BRACKET_HH */

