/*
  custos.hh

  source file of the GNU LilyPond music typesetter

  (C) 2000 Juergen Reuter <reuterj@ira.uka.de>
*/

#ifndef CUSTOS_HH
#define CUSTOS_HH

#include "lily-guile.hh"

struct Custos
{
  DECLARE_SCHEME_CALLBACK(brew_molecule, (SCM ));
  static bool has_interface (Grob*);
};

#endif // CUSTOS_HH

