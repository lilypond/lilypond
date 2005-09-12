/*
  laissez-vibrer-tie-column.hh -- declare Laissez_vibrer_tie_column

  source file of the GNU LilyPond music typesetter

  (c) 2005 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#ifndef TIE_LAISSEZ_VIBRER_COLUMN_HH
#define TIE_LAISSEZ_VIBRER_COLUMN_HH

#include "grob-interface.hh"
#include "lily-proto.hh"

struct Laissez_vibrer_tie_column
{
  static bool has_interface (Grob *);
  static void set_directions (Grob *me);
};


#endif /* TIE_LAISSEZ_VIBRER_COLUMN_HH */

