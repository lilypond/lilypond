/*
  semi-tie-column.hh -- declare Laissez_vibrer_tie_column

  source file of the GNU LilyPond music typesetter

  (c) 2005--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#ifndef SEMI_TIE_COLUMN_HH
#define SEMI_TIE_COLUMN_HH

#include "grob-interface.hh"
#include "lily-proto.hh"

struct Semi_tie_column
{
  DECLARE_GROB_INTERFACE();
  
  DECLARE_SCHEME_CALLBACK (calc_positioning_done, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_head_direction, (SCM));
};


#endif /* SEMI_TIE_COLUMN_HH */

