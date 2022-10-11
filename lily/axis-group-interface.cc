/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "axis-group-interface.hh"

#include "align-interface.hh"
#include "directional-element-interface.hh"
#include "grob-array.hh"
#include "hara-kiri-group-spanner.hh"
#include "international.hh"
#include "interval-set.hh"
#include "ly-scm-list.hh"
#include "paper-column.hh"
#include "paper-score.hh"
#include "pointer-group-interface.hh"
#include "separation-item.hh"
#include "skyline-pair.hh"
#include "staff-grouper-interface.hh"
#include "stem.hh"
#include "stencil.hh"
#include "system.hh"
#include "warn.hh"
#include "unpure-pure-container.hh"

#include <algorithm>
#include <map>
#include <vector>

using std::multimap;
using std::pair;
using std::string;
using std::vector;

Real Axis_group_interface::default_outside_staff_padding_ = 0.46;

Real
Axis_group_interface::get_default_outside_staff_padding ()
{
  return default_outside_staff_padding_;
}

void
Axis_group_interface::add_element (Grob *me, Grob *e)
{
  ly_scm_list_t<Axis> axes (get_property (me, "axes"));
  if (axes.empty ())
    programming_error ("axes should be nonempty");

  for (auto a : axes)
    {
      if (!e->get_parent (a))
        e->set_parent (me, a);

      set_object (e,
                  (a == X_AXIS) ? ly_symbol2scm ("axis-group-parent-X")
                                : ly_symbol2scm ("axis-group-parent-Y"),
                  me->self_scm ());
    }

  /* must be ordered, because Align_interface also uses
     Axis_group_interface  */
  Pointer_group_interface::add_grob (me, ly_symbol2scm ("elements"), e);
}

bool
Axis_group_interface::has_axis (Grob *me, Axis a)
{
  SCM axes = get_property (me, "axes");

  return scm_is_true (scm_memq (to_scm (a), axes));
}

Interval
Axis_group_interface::relative_group_extent (vector<Grob *> const &elts,
                                             Grob *common, Axis a)
{
  return relative_maybe_bound_group_extent (elts, common, a, false);
}

Interval
Axis_group_interface::relative_maybe_bound_group_extent (
  vector<Grob *> const &elts, Grob *common, Axis a, bool bound)
{
  Interval r;
  for (vsize i = 0; i < elts.size (); i++)
    {
      Grob *se = elts[i];
      if (!from_scm<bool> (get_property (se, "cross-staff")))
        {
          Interval dims = (bound && has_interface<Axis_group_interface> (se)
                             ? generic_bound_extent (se, common, a)
                             : se->extent (common, a));
          if (!dims.is_empty ())
            r.unite (dims);
        }
    }
  return r;
}

Interval
Axis_group_interface::generic_bound_extent (Grob *me, Grob *common, Axis a)
{
  /* trigger the callback to do skyline-spacing on the children */
  if (a == Y_AXIS)
    (void) get_property (me, "vertical-skylines");

  extract_grob_set (me, "elements", elts);
  vector<Grob *> new_elts;

  SCM interfaces = get_property (me, "bound-alignment-interfaces");

  for (vsize i = 0; i < elts.size (); i++)
    for (SCM alignment_interface : as_ly_scm_list (interfaces))
      if (elts[i]->internal_has_interface (alignment_interface))
        new_elts.push_back (elts[i]);

  if (!new_elts.size ())
    return robust_relative_extent (me, common, a);

  if (!common)
    common = common_refpoint_of_array (new_elts, me, a);

  return relative_maybe_bound_group_extent (new_elts, common, a, true);
}

Interval
Axis_group_interface::relative_pure_height (Grob *me, int start, int end)
{
  /* It saves a _lot_ of time if we assume a VerticalAxisGroup is additive
     (ie. height (i, k) = std::max (height (i, j) height (j, k)) for all i <= j <= k).
     Unfortunately, it isn't always true, particularly if there is a
     VerticalAlignment somewhere in the descendants.

     Usually, the only VerticalAlignment comes from Score. This makes it
     reasonably safe to assume that if our parent is a VerticalAlignment,
     we can assume additivity and cache things nicely. */
  Grob *p = me->get_y_parent ();
  if (has_interface<Align_interface> (p))
    return Axis_group_interface::sum_partial_pure_heights (me, start, end);

  Grob *common = unsmob<Grob> (get_object (me, "pure-Y-common"));
  extract_grob_set (me, "pure-relevant-grobs", elts);

  Interval r;
  for (auto *g : elts)
    {
      g = g->pure_find_visible_prebroken_piece (start, end);
      if (!g)
        continue;

      Interval_t<int> rank_span = g->spanned_column_rank_interval ();
      if (rank_span[LEFT] <= end && rank_span[RIGHT] >= start
          && !(from_scm<bool> (get_property (g, "cross-staff"))
               && has_interface<Stem> (g)))
        {
          Interval dims = g->pure_y_extent (common, start, end);
          if (!dims.is_empty ())
            r.unite (dims);
        }
    }
  return r;
}

MAKE_SCHEME_CALLBACK (Axis_group_interface, width,
                      "ly:axis-group-interface::width", 1);
SCM
Axis_group_interface::width (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  return generic_group_extent (me, X_AXIS);
}

MAKE_SCHEME_CALLBACK (Axis_group_interface, height,
                      "ly:axis-group-interface::height", 1);
SCM
Axis_group_interface::height (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  return generic_group_extent (me, Y_AXIS);
}

MAKE_SCHEME_CALLBACK (Axis_group_interface, pure_height,
                      "ly:axis-group-interface::pure-height", 3);
SCM
Axis_group_interface::pure_height (SCM smob, SCM start_scm, SCM end_scm)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  int start = from_scm (start_scm, 0);
  int end = from_scm (end_scm, INT_MAX);
  return to_scm (pure_group_height (me, start, end));
}

Interval
Axis_group_interface::pure_group_height (Grob *me, int start, int end)
{
  Grob *common = unsmob<Grob> (get_object (me, "pure-Y-common"));

  if (!common)
    {
      programming_error ("no pure Y common refpoint");
      return Interval ();
    }
  Real my_coord = me->pure_relative_y_coordinate (common, start, end);
  Interval r (relative_pure_height (me, start, end));

  return r - my_coord;
}

SCM
Axis_group_interface::generic_group_extent (Grob *me, Axis a)
{
  extract_grob_set (me, "elements", elts);

  /* trigger the callback to do skyline-spacing on the children */
  if (a == Y_AXIS)
    for (vsize i = 0; i < elts.size (); i++)
      if (!(has_interface<Stem> (elts[i])
            && from_scm<bool> (get_property (elts[i], "cross-staff"))))
        (void) get_property (elts[i], "vertical-skylines");

  Grob *common = common_refpoint_of_array (elts, me, a);

  Real my_coord = me->relative_coordinate (common, a);
  Interval r (relative_group_extent (elts, common, a));

  return to_scm (r - my_coord);
}

/* This is like generic_group_extent, but it only counts the grobs that
   are children of some other axis-group. This is uncached; if it becomes
   commonly used, it may be necessary to cache it somehow. */
Interval
Axis_group_interface::staff_extent (Grob *me, Grob *refp, Axis ext_a,
                                    Grob *staff, Axis parent_a)
{
  extract_grob_set (me, "elements", elts);
  vector<Grob *> new_elts;

  for (vsize i = 0; i < elts.size (); i++)
    if (elts[i]->has_in_ancestry (staff, parent_a))
      new_elts.push_back (elts[i]);

  return relative_group_extent (new_elts, refp, ext_a);
}

MAKE_SCHEME_CALLBACK (Axis_group_interface, calc_pure_relevant_grobs,
                      "ly:axis-group-interface::calc-pure-relevant-grobs", 1);
SCM
Axis_group_interface::calc_pure_relevant_grobs (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  /* TODO: Filter out elements that belong to a different Axis_group,
     such as the tie in
     << \new Staff=A { c'1~ \change Staff=B c'}
        \new Staff=B { \clef bass R1 R } >>
    because thier location relative to this Axis_group is not known before
    page layout.  For now, we need to trap this case in calc_pure_y_common.
  */
  return internal_calc_pure_relevant_grobs (me, "elements");
}

Interval
Axis_group_interface::sum_partial_pure_heights (Grob *me, int start, int end)
{
  Interval iv = begin_of_line_pure_height (me, start);
  iv.unite (rest_of_line_pure_height (me, start, end));

  return iv;
}

Interval
Axis_group_interface::part_of_line_pure_height (Grob *me, bool begin,
                                                vsize start, vsize end)
{
  Spanner *sp = dynamic_cast<Spanner *> (me);
  if (!sp)
    return Interval (0, 0);
  SCM cache_symbol = begin ? ly_symbol2scm ("begin-of-line-pure-height")
                           : ly_symbol2scm ("rest-of-line-pure-height");
  SCM cached = sp->get_cached_pure_property (cache_symbol, start, end);
  if (scm_is_pair (cached))
    return from_scm (cached, Interval (0, 0));

  SCM adjacent_pure_heights = get_property (me, "adjacent-pure-heights");
  Interval ret;

  if (!scm_is_pair (adjacent_pure_heights))
    ret = Interval (0, 0);
  else
    {
      SCM these_pure_heights = begin ? scm_car (adjacent_pure_heights)
                                     : scm_cdr (adjacent_pure_heights);

      if (scm_is_vector (these_pure_heights))
        ret = combine_pure_heights (me, these_pure_heights, start, end);
      else
        ret = Interval (0, 0);
    }

  sp->cache_pure_property (cache_symbol, start, end, to_scm (ret));
  return ret;
}

Interval
Axis_group_interface::begin_of_line_pure_height (Grob *me, vsize start)
{
  return part_of_line_pure_height (me, true, start, start + 1);
}

Interval
Axis_group_interface::rest_of_line_pure_height (Grob *me, vsize start,
                                                vsize end)
{
  return part_of_line_pure_height (me, false, start, end);
}

Interval
Axis_group_interface::combine_pure_heights (Grob *me, SCM measure_extents,
                                            vsize start, vsize end)
{
  Paper_score *ps = get_root_system (me)->paper_score ();
  vector<vsize> const &break_ranks = ps->get_break_ranks ();
  auto it = std::lower_bound (break_ranks.begin (), break_ranks.end (), start);
  vsize break_idx = it - break_ranks.begin ();
  vector<vsize> const &breaks = ps->get_break_indices ();
  vector<Paper_column *> const &cols = ps->get_columns ();

  Interval ext;
  for (vsize i = break_idx; i + 1 < breaks.size (); i++)
    {
      vsize r = cols[breaks[i]]->get_rank ();
      if (r >= end)
        break;

      ext.unite (from_scm<Interval> (scm_c_vector_ref (measure_extents, i)));
    }

  return ext;
}

// adjacent-pure-heights is a pair of vectors, each of which has one element
// for every measure in the score. The first vector stores, for each measure,
// the combined height of the elements that are present only when the bar
// is at the beginning of a line. The second vector stores, for each measure,
// the combined height of the elements that are present only when the bar
// is not at the beginning of a line.
MAKE_SCHEME_CALLBACK (Axis_group_interface, adjacent_pure_heights,
                      "ly:axis-group-interface::adjacent-pure-heights", 1)
SCM
Axis_group_interface::adjacent_pure_heights (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);

  Grob *common = unsmob<Grob> (get_object (me, "pure-Y-common"));
  extract_grob_set (me, "pure-relevant-grobs", elts);

  Paper_score *ps = get_root_system (me)->paper_score ();
  vector<vsize> const &ranks = ps->get_break_ranks ();

  vector<Interval> begin_line_heights;
  vector<Interval> mid_line_heights;
  vector<Interval> begin_line_staff_heights;
  vector<Interval> mid_line_staff_heights;
  begin_line_heights.resize (ranks.size () - 1);
  mid_line_heights.resize (ranks.size () - 1);

  for (vsize i = 0; i < elts.size (); ++i)
    {
      Grob *g = elts[i];

      if (from_scm<bool> (get_property (g, "cross-staff")))
        continue;

      if (!g->is_live ())
        {
          Item *it = dynamic_cast<Item *> (g);
          if (!it)
            continue;
          if (!it->get_column ())
            continue;
        }

      bool outside_staff
        = scm_is_number (get_property (g, "outside-staff-priority"));
      Real padding
        = from_scm<double> (get_property (g, "outside-staff-padding"),
                            get_default_outside_staff_padding ());

      // When we encounter the first outside-staff grob, make a copy
      // of the current heights to use as an estimate for the staff heights.
      // Note that the outside-staff approximation that we use here doesn't
      // consider any collisions that might occur between outside-staff grobs,
      // but only the fact that outside-staff grobs may need to be raised above
      // the staff.
      if (outside_staff && begin_line_staff_heights.empty ())
        {
          begin_line_staff_heights = begin_line_heights;
          mid_line_staff_heights = mid_line_heights;
        }

      // TODO: consider a pure version of get_grob_direction?
      Direction d = from_scm<Direction> (get_property_data (g, "direction"));
      d = (d == CENTER) ? UP : d;

      Interval_t<vsize> rank_span (g->spanned_column_rank_interval ());
      auto first_break
        = std::lower_bound (ranks.begin (), ranks.end (), rank_span[LEFT]);
      if (first_break != ranks.begin ())
        first_break--;

      for (vsize j = first_break - ranks.begin ();
           j + 1 < ranks.size () && ranks[j] <= rank_span[RIGHT]; ++j)
        {
          vsize start = ranks[j];
          vsize end = ranks[j + 1];

          // Take grobs that are visible with respect to a slightly longer line.
          // Otherwise, we will never include grobs at breakpoints which aren't
          // end-of-line-visible.
          vsize visibility_end = j + 2 < ranks.size () ? ranks[j + 2] : end;

          Grob *maybe_subst
            = g->pure_find_visible_prebroken_piece (start, visibility_end);
          if (maybe_subst)
            {
              Interval dims = maybe_subst->pure_y_extent (common, start, end);
              if (!dims.is_empty ())
                {
                  if (rank_span[LEFT] <= start)
                    {
                      if (outside_staff)
                        begin_line_heights[j].unite (
                          begin_line_staff_heights[j].union_disjoint (
                            dims, padding, d));
                      else
                        begin_line_heights[j].unite (dims);
                    }
                  if (rank_span[RIGHT] > start)
                    {
                      if (outside_staff)
                        mid_line_heights[j].unite (
                          mid_line_staff_heights[j].union_disjoint (
                            dims, padding, d));
                      else
                        mid_line_heights[j].unite (dims);
                    }
                }
            }
        }
    }

  // Convert begin_line_heights and min_line_heights to SCM.
  SCM begin_scm = scm_c_make_vector (ranks.size () - 1, SCM_EOL);
  SCM mid_scm = scm_c_make_vector (ranks.size () - 1, SCM_EOL);
  for (vsize i = 0; i < begin_line_heights.size (); ++i)
    {
      scm_c_vector_set_x (begin_scm, i, to_scm (begin_line_heights[i]));
      scm_c_vector_set_x (mid_scm, i, to_scm (mid_line_heights[i]));
    }

  return scm_cons (begin_scm, mid_scm);
}

MAKE_SCHEME_CALLBACK (Axis_group_interface, calc_skylines,
                      "ly:axis-group-interface::calc-skylines", 1);
SCM
Axis_group_interface::calc_skylines (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  Skyline_pair skylines = skyline_spacing (me);
  return to_scm (skylines);
}

/* whereas calc_skylines calculates skylines for axis-groups with a lot of
   visible children, combine_skylines is designed for axis-groups whose only
   children are other axis-groups (ie. VerticalAlignment). Rather than
   calculating all the skylines from scratch, we just merge the skylines
   of the children.
*/
MAKE_SCHEME_CALLBACK (Axis_group_interface, combine_skylines,
                      "ly:axis-group-interface::combine-skylines", 1);
SCM
Axis_group_interface::combine_skylines (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  extract_grob_set (me, "elements", elements);
  Grob *y_common = common_refpoint_of_array (elements, me, Y_AXIS);
  Grob *x_common = common_refpoint_of_array (elements, me, X_AXIS);

  if (y_common != me)
    programming_error ("combining skylines that don't belong to me");

  Skyline_pair ret;
  for (vsize i = 0; i < elements.size (); i++)
    {
      SCM skyp_scm = get_property (elements[i], "vertical-skylines");
      if (is_scm<Skyline_pair> (skyp_scm))
        {
          Real offset = elements[i]->relative_coordinate (y_common, Y_AXIS);
          Skyline_pair skyp = from_scm<Skyline_pair> (skyp_scm);
          skyp.raise (offset);
          skyp.shift (elements[i]->relative_coordinate (x_common, X_AXIS));
          ret.merge (skyp);
        }
    }
  return to_scm (ret);
}

struct Grob_with_priority
{
  Grob_with_priority (Grob *grob, double priority)
    : grob_ (grob),
      priority_ (priority)
  {
  }

  Grob *grob_;
  double priority_;
};

bool
operator<(const Grob_with_priority &g1, const Grob_with_priority &g2)
{
  return g1.priority_ < g2.priority_;
}

SCM
Axis_group_interface::internal_calc_pure_relevant_grobs (
  Grob *me, const string &grob_set_name)
{
  extract_grob_set (me, grob_set_name.c_str (), elts);

  // It is cheaper to cache the outside-staff-priority than saving the one copy
  // to assemble the final Grob_array.
  vector<Grob_with_priority> relevant_grobs;

  for (Grob *g : elts)
    {
      if (Item *it = dynamic_cast<Item *> (g))
        {
          if (it->original ())
            continue;
        }
      Real priority = from_scm<double> (
        get_property (g, "outside-staff-priority"), -infinity_f);
      /* This might include potentially suicided items. Callers should
         look at the relevant prebroken clone where necesary */
      relevant_grobs.emplace_back (g, priority);
    }

  std::sort (relevant_grobs.begin (), relevant_grobs.end ());

  SCM grobs_scm = Grob_array::make_array ();
  Grob_array *grobs = unsmob<Grob_array> (grobs_scm);
  for (auto const &g : relevant_grobs)
    grobs->add (g.grob_);

  return grobs_scm;
}

MAKE_SCHEME_CALLBACK (Axis_group_interface, calc_pure_y_common,
                      "ly:axis-group-interface::calc-pure-y-common", 1);
SCM
Axis_group_interface::calc_pure_y_common (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);

  extract_grob_set (me, "pure-relevant-grobs", elts);
  Grob *common = common_refpoint_of_array (elts, me, Y_AXIS);
  if (common != me && has_interface<Align_interface> (common))
    {
      me->programming_error ("My pure_y_common is a VerticalAlignment,"
                             " which might contain several staves.");
      common = me;
    }
  if (!common)
    {
      me->programming_error ("No common parent found in calc_pure_y_common.");
      return SCM_EOL;
    }

  return common->self_scm ();
}

SCM
Axis_group_interface::calc_common (Grob *me, Axis axis)
{
  extract_grob_set (me, "elements", elts);
  Grob *common = common_refpoint_of_array (elts, me, axis);
  if (!common)
    {
      me->programming_error ("No common parent found in calc_common axis.");
      return SCM_EOL;
    }

  return common->self_scm ();
}

MAKE_SCHEME_CALLBACK (Axis_group_interface, calc_x_common,
                      "ly:axis-group-interface::calc-x-common", 1);
SCM
Axis_group_interface::calc_x_common (SCM grob)
{
  return calc_common (unsmob<Grob> (grob), X_AXIS);
}

MAKE_SCHEME_CALLBACK (Axis_group_interface, calc_y_common,
                      "ly:axis-group-interface::calc-y-common", 1);
SCM
Axis_group_interface::calc_y_common (SCM grob)
{
  return calc_common (unsmob<Grob> (grob), Y_AXIS);
}

void
Axis_group_interface::get_children (Grob *me, vector<Grob *> *found)
{
  found->push_back (me);

  if (!has_interface<Axis_group_interface> (me))
    return;

  extract_grob_set (me, "elements", elements);
  for (vsize i = 0; i < elements.size (); i++)
    {
      Grob *e = elements[i];
      Axis_group_interface::get_children (e, found);
    }
}

// Raises the grob elt (whose skylines are given by h_skyline
// and v_skyline) so that it doesn't intersect with staff_skyline,
// or with anything in other_h_skylines and other_v_skylines.
void
avoid_outside_staff_collisions (Grob *elt, Skyline_pair *v_skyline,
                                Real padding, Real horizon_padding,
                                vector<Skyline_pair> const &other_v_skylines,
                                vector<Real> const &other_padding,
                                vector<Real> const &other_horizon_padding,
                                Direction const dir)
{
  assert (other_v_skylines.size () == other_padding.size ());
  assert (other_v_skylines.size () == other_horizon_padding.size ());
  vector<Interval> forbidden_intervals;
  for (vsize j = 0; j < other_v_skylines.size (); j++)
    {
      Skyline_pair const &v_other = other_v_skylines[j];
      Real pad = std::max (padding, other_padding[j]);
      Real horizon_pad = std::max (horizon_padding, other_horizon_padding[j]);

      // We need to push elt up by at least this much to be above v_other.
      Real up = (*v_skyline)[DOWN].distance (v_other[UP], horizon_pad) + pad;
      // We need to push elt down by at least this much to be below v_other.
      Real down = (*v_skyline)[UP].distance (v_other[DOWN], horizon_pad) + pad;

      forbidden_intervals.push_back (Interval (-down, up));
    }

  Interval_set allowed_shifts
    = Interval_set::interval_union (forbidden_intervals).complement ();
  Real move = allowed_shifts.nearest_point (0, dir);
  v_skyline->raise (move);
  elt->translate_axis (move, Y_AXIS);
}

SCM
valid_outside_staff_placement_directive (Grob *me)
{
  SCM directive = get_property (me, "outside-staff-placement-directive");

  if (scm_is_eq (directive, ly_symbol2scm ("left-to-right-greedy"))
      || scm_is_eq (directive, ly_symbol2scm ("left-to-right-polite"))
      || scm_is_eq (directive, ly_symbol2scm ("right-to-left-greedy"))
      || scm_is_eq (directive, ly_symbol2scm ("right-to-left-polite")))
    return directive;

  me->warning (_f ("\"%s\" is not a valid outside-staff-placement-directive",
                   robust_symbol2string (directive, "").c_str ()));

  return ly_symbol2scm ("left-to-right-polite");
}

// Shifts the grobs in elements to ensure that they (and any
// connected riders) don't collide with the staff skylines
// or anything in all_X_skylines.  Afterwards, the skylines
// of the grobs in elements will be added to all_v_skylines.
static void
add_grobs_of_one_priority (Grob *me,
                           Drul_array<vector<Skyline_pair>> *all_v_skylines,
                           Drul_array<vector<Real>> *all_paddings,
                           Drul_array<vector<Real>> *all_horizon_paddings,
                           vector<Grob *> elements, Grob *x_common,
                           Grob *y_common,
                           multimap<Grob *, Grob *> const &riders)
{

  SCM directive = valid_outside_staff_placement_directive (me);

  bool l2r = (scm_is_eq (directive, ly_symbol2scm ("left-to-right-greedy"))
              || scm_is_eq (directive, ly_symbol2scm ("left-to-right-polite")));

  bool polite
    = (scm_is_eq (directive, ly_symbol2scm ("left-to-right-polite"))
       || scm_is_eq (directive, ly_symbol2scm ("right-to-left-polite")));

  vector<Box> boxes;
  vector<Skyline_pair> skylines_to_merge;

  // We want to avoid situations like this:
  //           still more text
  //      more text
  //   text
  //   -------------------
  //   staff
  //   -------------------

  // The point is that "still more text" should be positioned under
  // "more text".  In order to achieve this, we place the grobs in several
  // passes.  We keep track of the right-most horizontal position that has been
  // affected by the current pass so far (actually we keep track of 2
  // positions, one for above the staff, one for below).

  // In each pass, we loop through the unplaced grobs from left to right.
  // If the grob doesn't overlap the right-most affected position, we place it
  // (and then update the right-most affected position to point to the right
  // edge of the just-placed grob).  Otherwise, we skip it until the next pass.
  while (!elements.empty ())
    {
      Drul_array<Real> last_end (-infinity_f, -infinity_f);
      vector<Grob *> skipped_elements;
      for (vsize i = l2r ? 0 : elements.size ();
           l2r ? i < elements.size () : i--; l2r ? i++ : 0)
        {
          Grob *elt = elements[i];
          Real padding = from_scm<double> (
            get_property (elt, "outside-staff-padding"),
            Axis_group_interface ::get_default_outside_staff_padding ());
          Real horizon_padding = from_scm<double> (
            get_property (elt, "outside-staff-horizontal-padding"), 0.0);
          Interval x_extent = elt->extent (x_common, X_AXIS);
          x_extent.widen (horizon_padding);

          Direction dir = get_grob_direction (elt);
          if (dir == CENTER)
            {
              warning (_ ("an outside-staff object should have a direction, "
                          "defaulting to up"));
              dir = UP;
            }

          if (x_extent[LEFT] <= last_end[dir] && polite)
            {
              skipped_elements.push_back (elt);
              continue;
            }
          last_end[dir] = x_extent[RIGHT];

          SCM orig_skyp = get_property (elt, "vertical-skylines");
          if (!is_scm<Skyline_pair> (orig_skyp))
            continue;
          Skyline_pair v_skylines = from_scm<Skyline_pair> (orig_skyp);
          if (v_skylines.is_empty ())
            continue;
          // Find the riders associated with this grob, and merge their
          // skylines with elt's skyline.
          typedef multimap<Grob *, Grob *>::const_iterator GrobMapIterator;
          pair<GrobMapIterator, GrobMapIterator> range
            = riders.equal_range (elt);
          vector<Skyline_pair> rider_v_skylines;
          for (GrobMapIterator j = range.first; j != range.second; j++)
            {
              Grob *rider = j->second;
              SCM v_rider_scm = get_property (rider, "vertical-skylines");
              if (is_scm<Skyline_pair> (v_rider_scm))
                {
                  Skyline_pair v_rider = from_scm<Skyline_pair> (v_rider_scm);
                  v_rider.shift (rider->relative_coordinate (x_common, X_AXIS));
                  v_rider.raise (rider->relative_coordinate (y_common, Y_AXIS));
                  rider_v_skylines.push_back (v_rider);
                }
            }
          v_skylines.shift (elt->relative_coordinate (x_common, X_AXIS));
          v_skylines.raise (elt->relative_coordinate (y_common, Y_AXIS));
          v_skylines.merge (Skyline_pair (rider_v_skylines));

          avoid_outside_staff_collisions (
            elt, &v_skylines, padding, horizon_padding, (*all_v_skylines)[dir],
            (*all_paddings)[dir], (*all_horizon_paddings)[dir], dir);

          set_property (elt, "outside-staff-priority", SCM_BOOL_F);
          (*all_v_skylines)[dir].push_back (v_skylines);
          (*all_paddings)[dir].push_back (padding);
          (*all_horizon_paddings)[dir].push_back (horizon_padding);
        }
      std::swap (elements, skipped_elements);
      skipped_elements.clear ();
    }
}

// If the Grob has a Y-ancestor with outside-staff-priority, return it.
// Otherwise, return 0.
Grob *
Axis_group_interface::outside_staff_ancestor (Grob *me)
{
  Grob *parent = me->get_y_parent ();
  if (!parent)
    return 0;

  if (scm_is_number (get_property (parent, "outside-staff-priority")))
    return parent;

  return outside_staff_ancestor (parent);
}

struct Skyline_key
{
  Skyline_key (Grob *grob, double priority, double left_extent)
    : grob_ (grob),
      priority_ (priority),
      left_extent_ (left_extent)
  {
  }

  Grob *grob_;
  double priority_;
  double left_extent_;
};

bool
operator<(const Skyline_key &k1, const Skyline_key &k2)
{
  if (k1.priority_ != k2.priority_)
    return k1.priority_ < k2.priority_;

  return k1.left_extent_ < k2.left_extent_;
}

// It is tricky to correctly handle skyline placement of cross-staff grobs.
// For example, cross-staff beams cannot be formatted until the distance between
// staves is known and therefore any grobs that depend on the beam cannot be placed
// until the skylines are known. On the other hand, the distance between staves should
// really depend on position of the cross-staff grobs that lie between them.
// Currently, we just leave cross-staff grobs out of the
// skyline altogether, but this could mean that staves are placed so close together
// that there is no room for the cross-staff grob. It also means, of course, that
// we don't get the benefits of skyline placement for cross-staff grobs.
Skyline_pair
Axis_group_interface::skyline_spacing (Grob *me)
{
  extract_grob_set (
    me,
    unsmob<Grob_array> (get_object (me, "vertical-skyline-elements"))
      ? "vertical-skyline-elements"
      : "elements",
    orig_elements);
  Grob *x_common = common_refpoint_of_array (orig_elements, me, X_AXIS);
  Grob *y_common = common_refpoint_of_array (orig_elements, me, Y_AXIS);

  if (y_common != me)
    {
      me->programming_error ("Some of my vertical-skyline-elements"
                             " are outside my VerticalAxisGroup.");
      y_common = me;
    }

  vector<Skyline_key> elements;
  elements.reserve (orig_elements.size ());
  for (Grob *g : orig_elements)
    {
      /*
        As a sanity check, we make sure that no grob with an outside staff priority
        has a Y-parent that also has an outside staff priority, which would result
        in two movings.
      */
      double priority = from_scm<double> (
        get_property (g, "outside-staff-priority"), -infinity_f);
      double left_extent = 0;
      if (!std::isinf (priority))
        {
          if (outside_staff_ancestor (g))
            {
              g->warning (_ ("Cannot set outside-staff-priority for element "
                             "and elements' Y parent."));
              set_property (g, "outside-staff-priority", SCM_BOOL_F);
              priority = -infinity_f;
            }
          else
            {
              left_extent = g->extent (x_common, X_AXIS)[LEFT];
            }
        }
      elements.emplace_back (g, priority, left_extent);
    }

  std::stable_sort (elements.begin (), elements.end ());

  // A rider is a grob that is not outside-staff, but has an outside-staff
  // ancestor.  In that case, the rider gets moved along with its ancestor.
  multimap<Grob *, Grob *> riders;

  vsize i = 0;
  vector<Skyline_pair> inside_staff_skylines;

  for (i = 0; i < elements.size () && std::isinf (elements[i].priority_); i++)
    {
      Grob *elt = elements[i].grob_;
      if (Grob *ancestor = outside_staff_ancestor (elt))
        riders.insert (pair<Grob *, Grob *> (ancestor, elt));
      else if (!from_scm<bool> (get_property (elt, "cross-staff")))
        {
          SCM maybe_pair = get_property (elt, "vertical-skylines");
          if (!is_scm<Skyline_pair> (maybe_pair))
            continue;
          const Skyline_pair &skyp = from_scm<Skyline_pair> (maybe_pair);
          if (skyp.is_empty ())
            continue;
          inside_staff_skylines.push_back (skyp);
          inside_staff_skylines.back ().shift (
            elt->relative_coordinate (x_common, X_AXIS));
          inside_staff_skylines.back ().raise (
            elt->relative_coordinate (y_common, Y_AXIS));
        }
    }

  Skyline_pair skylines (inside_staff_skylines);

  // These are the skylines of all outside-staff grobs
  // that have already been processed.  We keep them around in order to
  // check them for collisions with the currently active outside-staff grob.
  Drul_array<vector<Skyline_pair>> all_v_skylines;
  Drul_array<vector<Real>> all_paddings;
  Drul_array<vector<Real>> all_horizon_paddings;
  for (const auto d : {UP, DOWN})
    {
      all_v_skylines[d].push_back (skylines);
      all_paddings[d].push_back (0);
      all_horizon_paddings[d].push_back (0);
    }

  for (; i < elements.size (); i++)
    {
      if (from_scm<bool> (get_property (elements[i].grob_, "cross-staff")))
        continue;

      // Collect all the outside-staff grobs that have a particular priority.
      vector<Grob *> current_elts;
      current_elts.push_back (elements[i].grob_);
      while (i + 1 < elements.size ()
             && elements[i].priority_ == elements[i + 1].priority_)
        {
          if (!from_scm<bool> (
                get_property (elements[i + 1].grob_, "cross-staff")))
            current_elts.push_back (elements[i + 1].grob_);
          ++i;
        }

      add_grobs_of_one_priority (me, &all_v_skylines, &all_paddings,
                                 &all_horizon_paddings, current_elts, x_common,
                                 y_common, riders);
    }

  // Now everything in all_v_skylines has been shifted appropriately; merge
  // them all into skylines to get the complete outline.
  Skyline_pair other_skylines (all_v_skylines[UP]);
  other_skylines.merge (Skyline_pair (all_v_skylines[DOWN]));
  skylines.merge (other_skylines);

  // We began by shifting my skyline to be relative to the common refpoint; now
  // shift it back.
  skylines.shift (-me->relative_coordinate (x_common, X_AXIS));

  return skylines;
}

MAKE_SCHEME_CALLBACK (Axis_group_interface, calc_pure_staff_staff_spacing,
                      "ly:axis-group-interface::calc-pure-staff-staff-spacing",
                      3)
SCM
Axis_group_interface::calc_pure_staff_staff_spacing (SCM smob, SCM start,
                                                     SCM end)
{
  return calc_maybe_pure_staff_staff_spacing (
    unsmob<Grob> (smob), true, from_scm<int> (start), from_scm<int> (end));
}

MAKE_SCHEME_CALLBACK (Axis_group_interface, calc_staff_staff_spacing,
                      "ly:axis-group-interface::calc-staff-staff-spacing", 1)
SCM
Axis_group_interface::calc_staff_staff_spacing (SCM smob)
{
  return calc_maybe_pure_staff_staff_spacing (unsmob<Grob> (smob), false, 0,
                                              INT_MAX);
}

SCM
Axis_group_interface::calc_maybe_pure_staff_staff_spacing (Grob *me, bool pure,
                                                           int start, int end)
{
  Grob *grouper = unsmob<Grob> (get_object (me, "staff-grouper"));

  if (grouper)
    {
      bool within_group = Staff_grouper_interface::maybe_pure_within_group (
        grouper, me, pure, start, end);
      if (within_group)
        return get_maybe_pure_property (grouper, "staff-staff-spacing", pure,
                                        start, end);
      else
        return get_maybe_pure_property (grouper, "staffgroup-staff-spacing",
                                        pure, start, end);
    }
  return get_maybe_pure_property (me, "default-staff-staff-spacing", pure,
                                  start, end);
}

ADD_INTERFACE (Axis_group_interface,
               R"(
An object that groups other layout objects.
               )",

               // TODO: some of these properties are specific to
               // VerticalAxisGroup. We should split off a
               // vertical-axis-group-interface.
               /* properties */
               R"(
adjacent-pure-heights
axes
bound-alignment-interfaces
default-staff-staff-spacing
elements
nonstaff-nonstaff-spacing
nonstaff-relatedstaff-spacing
nonstaff-unrelatedstaff-spacing
pure-relevant-grobs
pure-relevant-items
pure-relevant-spanners
pure-Y-common
staff-affinity
staff-grouper
staff-staff-spacing
system-Y-offset
X-common
Y-common
               )");
