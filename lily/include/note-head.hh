/*
  note-head.hh -- part of GNU LilyPond

  (c) 1996--2005 Han-Wen Nienhuys
*/

#ifndef NOTEHEAD_HH
#define NOTEHEAD_HH

#include "stencil.hh"

/** ball at the end of the stem. Also takes care of ledger lines.

NoteHead is a kind of RhythmicHead, see there.

Read-only:
*/

class Note_head
{
public:
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (brew_ez_stencil, (SCM));
  DECLARE_SCHEME_CALLBACK (extent, (SCM, SCM));
  static bool has_interface (Grob *);
  static Real stem_attachment_coordinate (Grob *, Axis a);
  static int get_balltype (Grob *);
};
#endif // NOTEHEAD_HH

