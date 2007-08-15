/*
  semi-tie-column.hh -- declare Laissez_vibrer_tie_column

  source file of the GNU LilyPond music typesetter

  (c) 2005--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#ifndef SEMI_TIE_COLUMN_HH
#define SEMI_TIE_COLUMN_HH

#include "grob-interface.hh"
#include "lily-proto.hh"
#include "lily-guile.hh"

struct Semi_tie_column
{
  static bool has_interface (Grob *);
  
  DECLARE_SCHEME_CALLBACK (calc_positioning_done, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_head_direction, (SCM));
};


#endif /* SEMI_TIE_COLUMN_HH */

