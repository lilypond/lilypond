/*
  vaticana-ligature.hh

  source file of the GNU LilyPond music typesetter

 (C) 2003 Juergen Reuter <reuter@ipd.uka.de>
*/

#ifndef VATICANA_LIGATURE_HH
#define VATICANA_LIGATURE_HH

#include "lily-guile.hh"
#include "molecule.hh"

struct Vaticana_ligature
{
  DECLARE_SCHEME_CALLBACK (brew_ligature_primitive, (SCM ));
  DECLARE_SCHEME_CALLBACK (brew_molecule, (SCM ));
  static bool has_interface (Grob *);
};

#endif // VATICANA_LIGATURE_HH
