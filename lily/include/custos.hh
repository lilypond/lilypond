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

private:
  static void add_streepjes(Grob* me, int pos, int interspaces, Molecule* custos_p_);
  static Molecule create_ledger_line (Interval x_extent, Grob *me) ;

};

#endif // CUSTOS_HH

