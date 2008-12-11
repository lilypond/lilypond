/*
  melody-spanner.hh -- declare Melody_spanner

  source file of the GNU LilyPond music typesetter

  (c) 2005--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#ifndef MELODY_SPANNER_HH
#define MELODY_SPANNER_HH

#include "lily-proto.hh"
#include "grob-interface.hh"

class Melody_spanner
{
public:
  DECLARE_GROB_INTERFACE();
  static void add_stem (Grob*, Grob*);
  DECLARE_SCHEME_CALLBACK(calc_neutral_stem_direction, (SCM));
};

#endif /* MELODY_SPANNER_HH */

