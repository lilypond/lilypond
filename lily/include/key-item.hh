/*
  key-item.hh -- part of GNU LilyPond

  (c) 1996--2001 Han-Wen Nienhuys
*/

#ifndef KEYITEM_HH
#define KEYITEM_HH

#include "lily-guile.hh"
#include "lily-proto.hh"


struct Key_item
{
  static int calculate_position (Grob*,SCM pair) ;
  static void set_interface (Grob*);
  static bool has_interface (Grob*);
  DECLARE_SCHEME_CALLBACK (brew_molecule, (SCM ));
};

#endif // KEYITEM_HH
