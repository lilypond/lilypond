/*
  hara-kiri-vertical-group-spanner.hh -- declare Har_kiri_vertical_group_spanner

  source file of the GNU LilyPond music typesetter

  (c) 1998--2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/


#ifndef HARA_KIRI_VERTICAL_GROUP_SPANNER_HH
#define HARA_KIRI_VERTICAL_GROUP_SPANNER_HH

#include "lily-guile.hh"
#include "lily-proto.hh"

/** 
  As Vertical_group_spanner, but keep track of interesting items.  If
  we don't contain any interesting items after linebreaking, then
  gracefully commit suicide.  Objective: don't disgrace Lily by
  typesetting empty lines in orchestral scores.

  properties:

  items-worth-living -- list of interesting items. If empty in a particular system,
    clear this line


    todo: naming
*/
class Hara_kiri_group_spanner 
{
public:
  static Real force_hara_kiri_callback (Score_element * , Axis);
  static Interval y_extent (Score_element * , Axis);
  static Real force_hara_kiri_in_parent_callback (Score_element * , Axis);
  static void add_element (Score_element *me, Score_element *e);
  static void set_interface (Score_element*me);
  static bool has_interface (Score_element*);
  static void consider_suicide (Score_element*me);
  static void add_interesting_item (Score_element * me , Score_element* n);
};


#endif // HARA_KIRI_VERTICAL_GROUP_SPANNER_HH
