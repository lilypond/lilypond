/*
  laissez-vibrer-tie-column.hh -- declare Laissez_vibrer_tie_column

  source file of the GNU LilyPond music typesetter

  (c) 2005--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#ifndef TIE_LAISSEZ_VIBRER_COLUMN_HH
#define TIE_LAISSEZ_VIBRER_COLUMN_HH

#include "grob-interface.hh"
#include "lily-proto.hh"
#include "lily-guile.hh"

struct Laissez_vibrer_tie_column
{
  static bool has_interface (Grob *);
  
  DECLARE_SCHEME_CALLBACK(calc_positioning_done, (SCM));
};


#endif /* TIE_LAISSEZ_VIBRER_COLUMN_HH */

