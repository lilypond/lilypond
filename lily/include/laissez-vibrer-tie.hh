/*
  laissez-vibrer-tie.hh -- declare Laissez_vibrer_tie

  source file of the GNU LilyPond music typesetter

  (c) 2005--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#ifndef TIE_LAISSEZ_VIBRER_HH
#define TIE_LAISSEZ_VIBRER_HH


#include "grob-interface.hh"
#include "lily-guile.hh"

struct Laissez_vibrer_tie
{
  static bool has_interface (Grob *);
  
  DECLARE_SCHEME_CALLBACK (calc_direction, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_control_points, (SCM));
  static int compare (Grob *const &s1,
		      Grob *const &s2);
  static int get_position (Grob *);
};

#endif /* TIE_LAISSEZ_VIBRER_HH */
