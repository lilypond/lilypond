/*
  hara-kiri-vertical-group-spanner.hh -- declare Har_kiri_vertical_group_spanner

  source file of the GNU LilyPond music typesetter

  (c) 1998--2009 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef HARA_KIRI_VERTICAL_GROUP_SPANNER_HH
#define HARA_KIRI_VERTICAL_GROUP_SPANNER_HH

#include "lily-proto.hh"
#include "grob-interface.hh"

class Hara_kiri_group_spanner
{
public:
  DECLARE_SCHEME_CALLBACK (force_hara_kiri_callback, (SCM));
  DECLARE_SCHEME_CALLBACK (y_extent, (SCM smob));
  DECLARE_SCHEME_CALLBACK (calc_skylines, (SCM smob));
  DECLARE_SCHEME_CALLBACK (pure_height, (SCM smob, SCM start, SCM end));
  DECLARE_SCHEME_CALLBACK (force_hara_kiri_in_y_parent_callback, (SCM));
  DECLARE_SCHEME_CALLBACK (after_line_breaking, (SCM));
  DECLARE_GROB_INTERFACE();
  static bool request_suicide (Grob *me, int start, int end);
  static void consider_suicide (Grob *me);
  static void add_interesting_item (Grob *me, Grob *n);
};

#endif // HARA_KIRI_VERTICAL_GROUP_SPANNER_HH
