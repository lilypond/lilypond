/*
  note-head.hh -- part of GNU LilyPond

  (c) 1996--2002 Han-Wen Nienhuys
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
  DECLARE_SCHEME_CALLBACK (brew_molecule, (SCM ));
  static Interval head_extent (Grob*, Axis);
  static Molecule brew_ledger_lines (Grob *me, int pos, int interspaces,
				     Interval x_extent, bool take_space);
  DECLARE_SCHEME_CALLBACK (brew_ez_molecule, (SCM ));
  static bool has_interface (Grob*);
  static Real stem_attachment_coordinate (Grob *, Axis a);
  static int balltype_i (Grob*) ;  
};
#endif // NOTEHEAD_HH

