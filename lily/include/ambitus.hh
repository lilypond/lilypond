/*
  ambitus.hh

  source file of the GNU LilyPond music typesetter

 (C) 2000 Juergen Reuter <reuter@ipd.uka.de>
*/

#ifndef AMBITUS_HH
#define AMBITUS_HH

#include "lily-guile.hh"

struct Ambitus
{
  DECLARE_SCHEME_CALLBACK (brew_molecule, (SCM smob));
  static bool has_interface (Grob*);
};

#endif // AMBITUS_HH

