/*
  local-key-item.hh -- part of GNU LilyPond

  (c) 1996--2000 Han-Wen Nienhuys
*/

#ifndef LOCALKEYITEM_HH
#define LOCALKEYITEM_HH


#include "array.hh"
#include "musical-pitch.hh"

class Local_key_item
{
  static Molecule parenthesize (Score_element*me, Molecule) ;
public:
  DECLARE_SCHEME_CALLBACK(brew_molecule, (SCM ));
  static void add_pitch (Score_element*me, Musical_pitch, bool cautionary, bool natural);
  static bool has_interface (Score_element*);
  static void set_interface (Score_element*);  
};


#endif // LOCALKEYITEM_HH

