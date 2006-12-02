/*
  single-malt-grouping-item.hh -- declare Separation_item

  source file of the GNU LilyPond music typesetter

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef SINGLE_MALT_GROUPING_ITEM_HH
#define SINGLE_MALT_GROUPING_ITEM_HH

#include "lily-proto.hh"
#include "direction.hh"
#include "grob-interface.hh"

struct Separation_item
{
  DECLARE_GROB_INTERFACE();
  DECLARE_SCHEME_CALLBACK(calc_skylines, (SCM));

  static vector<Box> boxes (Grob *me);
  static Interval conditional_width (Grob *, Grob *);
  static Interval width (Grob *);
  static Interval relative_width (Grob *, Grob *);
  static Grob *extremal_break_aligned_grob (Grob *, Direction, Interval *);
  static bool set_distance (Drul_array<Item *>, Real);
  static void set_skyline_distance (Drul_array<Item*>, Real);
  static void add_item (Grob *, Item *);
  static void add_conditional_item (Grob *, Grob *);
};

#endif /* SINGLE_MALT_GROUPING_ITEM_HH */

