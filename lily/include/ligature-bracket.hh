/*
  ligature-bracket.hh -- part of GNU LilyPond

  source file of the GNU LilyPond music typesetter
  
  (c) 2002 Juergen Reuter <reuter@ipd.uka.de>
*/

#ifndef LIGATURE_BRACKET_HH
#define LIGATURE_BRACKET_HH

#include "lily-proto.hh"
#include "lily-guile.hh"

class Ligature_bracket
{
public:
  Ligature_bracket (SCM);
  DECLARE_SCHEME_CALLBACK (brew_molecule, (SCM ));

private:
};

#endif /* LIGATURE_BRACKET_HH */

