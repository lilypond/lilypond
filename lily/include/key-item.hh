/*
  key-item.hh -- part of GNU LilyPond

  (c) 1996--2000 Han-Wen Nienhuys
*/

#ifndef KEYITEM_HH
#define KEYITEM_HH

#include "lily-guile.hh"
#include "lily-proto.hh"


struct Key_item
{
  static int calculate_position(Score_element*,SCM pair) ;
  static void set_interface (Score_element*);
  static bool has_interface (Score_element*);
  DECLARE_SCHEME_CALLBACK(brew_molecule, (SCM ));
};

#endif // KEYITEM_HH
