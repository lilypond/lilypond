/*
  hara-kiri-vertical-group-spanner.hh -- declare Har_kiri_vertical_group_spanner

  source file of the GNU LilyPond music typesetter

  (c) 1998--2002 Jan Nieuwenhuizen <janneke@gnu.org>
*/


#ifndef HARA_KIRI_VERTICAL_GROUP_SPANNER_HH
#define HARA_KIRI_VERTICAL_GROUP_SPANNER_HH

#include "lily-guile.hh"
#include "lily-proto.hh"

class Hara_kiri_group_spanner 
{
public:
  DECLARE_SCHEME_CALLBACK (force_hara_kiri_callback, (SCM ,SCM));
  DECLARE_SCHEME_CALLBACK (y_extent, (SCM smob, SCM axis));
  DECLARE_SCHEME_CALLBACK (force_hara_kiri_in_parent_callback, (SCM ,SCM));
  static void add_element (Grob *me, Grob *e);
  static void set_interface (Grob*me);
  static bool has_interface (Grob*);
  static void consider_suicide (Grob*me);
  static void add_interesting_item (Grob * me , Grob* n);
};


#endif // HARA_KIRI_VERTICAL_GROUP_SPANNER_HH
