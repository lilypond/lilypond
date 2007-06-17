/*
  spacing-interface.hh -- declare Spacing_interface

  source file of the GNU LilyPond music typesetter

  (c) 2002--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/
#include "grob-interface.hh"
#include "lily-proto.hh"

#ifndef SPACING_INTERFACE_HH
#define SPACING_INTERFACE_HH

struct Spacing_interface
{
  static Real minimum_distance (Grob *me);
  static vector<Item*> right_note_columns (Grob *me);
  static vector<Item*> left_note_columns (Grob *me);
  static Item* right_column (Grob *me);
  static Item* left_column (Grob *me);

  DECLARE_GROB_INTERFACE();
};

#endif /* SPACING_INTERFACE_HH */
