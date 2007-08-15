/*
  profile.hh -- declare property profiling utils.

  source file of the GNU LilyPond music typesetter

  (c) 2005--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef PROFILE_HH
#define PROFILE_HH

#include "lily-guile.hh"

void note_property_access (SCM *table, SCM sym);
extern SCM context_property_lookup_table;
extern SCM grob_property_lookup_table;

#endif /* PROFILE_HH */
