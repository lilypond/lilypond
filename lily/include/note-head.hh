/*
  note-head.hh -- part of GNU LilyPond

  (c) 1996--2001 Han-Wen Nienhuys
*/

#ifndef NOTEHEAD_HH
#define NOTEHEAD_HH

#include "lily-guile.hh"
#include "molecule.hh"

/** ball at the end of the stem. Also takes care of ledger lines.

    NoteHead is a kind of RhythmicHead, see there.

  Read-only:
*/

class Note_head 
{
public:
  DECLARE_SCHEME_CALLBACK(brew_molecule, (SCM ));
  static Molecule ledger_line (Interval, Grob*) ;
  DECLARE_SCHEME_CALLBACK(brew_ez_molecule, (SCM ));
  static bool has_interface (Grob*);
  
};
#endif // NOTEHEAD_HH

