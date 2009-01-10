/*
  semi-tie.hh -- declare Laissez_vibrer_tie

  source file of the GNU LilyPond music typesetter

  (c) 2005--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#ifndef SEMI_TIE_HH
#define SEMI_TIE_HH


#include "grob-interface.hh"
#include "lily-proto.hh"

struct Semi_tie
{
  DECLARE_GROB_INTERFACE();
  
  DECLARE_SCHEME_CALLBACK (calc_direction, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_control_points, (SCM));
  static bool less (Grob *const &s1,
		    Grob *const &s2);
  static int get_position (Grob *);
};

#endif /* SEMI_TIE_HH */
