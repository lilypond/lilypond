/*
  separating-group-spanner.hh -- declare Separating_group_spanner

  source file of the GNU LilyPond music typesetter

  (c) 1998--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef SEPARATING_GROUP_SPANNER_HH
#define SEPARATING_GROUP_SPANNER_HH

#include "spanner.hh"

class Separating_group_spanner
{
  static void find_rods (Item *, SCM, Real);
public:
  static void add_spacing_unit (Grob *me, Item *);

  static bool has_interface (Grob *);
  DECLARE_SCHEME_CALLBACK (set_spacing_rods, (SCM));
};

#endif /* SEPARATING_GROUP_SPANNER_HH */

