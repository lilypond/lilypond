/*
  ligature-head.hh -- part of GNU LilyPond

  (c) 2002 Juergen Reuter <reuter@ipd.uka.de>
*/

#ifndef LIGATURE_HEAD_HH
#define LIGATURE_HEAD_HH

#include "lily-guile.hh"
#include "molecule.hh"

/** ball within a ligature.  Also takes care of ledger lines.

    LigatureHead is a kind of RhythmicHead, see there.

  Read-only:
*/

class Ligature_head 
{
public:
  DECLARE_SCHEME_CALLBACK (brew_molecule, (SCM ));
  static bool has_interface (Grob*);
  
};
#endif // LIGATURE_HEAD_HH

