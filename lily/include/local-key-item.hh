/*
  local-key-item.hh -- part of GNU LilyPond

  (c) 1996--2002 Han-Wen Nienhuys
*/

#ifndef LOCALKEYITEM_HH
#define LOCALKEYITEM_HH


#include "array.hh"
#include "pitch.hh"

class Local_key_item
{
  static Molecule parenthesize (Grob*me, Molecule) ;
public:
  DECLARE_SCHEME_CALLBACK (after_line_breaking, (SCM));
  DECLARE_SCHEME_CALLBACK (brew_molecule, (SCM));
  static void add_pitch (Grob*me, Pitch, bool cautionary, bool natural, Grob *tie_break_cautionary);
  static bool has_interface (Grob*);
  static void set_interface (Grob*);  
};


#endif // LOCALKEYITEM_HH

