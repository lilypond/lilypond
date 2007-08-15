/*
  rod.hh -- declare Rod, Column_rod

  source file of the GNU LilyPond music typesetter

  (c) 1998--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef ROD_HH
#define ROD_HH

#include "lily-proto.hh"
#include "drul-array.hh"

struct Rod
{
  Drul_array<Item *> item_drul_;
  Real distance_;

  /**
     translate the rod so as to refer to Paper_columns
  */
  void columnize ();

  void add_to_cols ();

  Rod ();
};

#endif /* ROD_HH */

