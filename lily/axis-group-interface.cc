/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2021 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "lookup.hh"
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

static bool
pure_staff_priority_less (Grob *const &g1, Grob *const &g2);

/* gets the relevant version of `g` in the context of a line running
   from `start` to `end` */
Grob *
pure_subst_prebroken_piece (Grob *g, vsize start, vsize end)
{
  Item *it = dynamic_cast<Item *> (g);
  if (!it)
    return g;

  if (it->get_column ()->get_rank () == int (start))
    {
      return it->find_prebroken_piece (RIGHT);
    }
  if (it->get_column ()->get_rank () == int (end))
    {
      return it->find_prebroken_piece (LEFT);
    }

  return it;
}

Real Axis_group_interface::default_outside_staff_padding_ = 0.46;

Real
Axis_group_interface::get_default_outside_staff_padding ()
{
  return default_outside_staff_padding_;
}

void
Axis_group_interface::add_element (Grob *me, Grob *e)
{
  SCM axes = get_property (me, "axes");
  if (!scm_is_pair (axes))
    programming_error ("axes should be nonempty");

  for (SCM ax = axes; scm_is_pair (ax); ax = scm_cdr (ax))
    {
      Axis a = (Axis) scm_to_int (scm_car (ax));

      if (!e->get_parent (a))
        e->set_parent (me, a);

      set_object (e, (a == X_AXIS)
                  ? ly_symbol2scm ("axis-group-parent-X")
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
Axis_group_interface::relative_maybe_bound_group_extent (vector<Grob *> const &elts,
                                                         Grob *common, Axis a, bool bound)
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
    for (SCM l = interfaces; scm_is_pair (l); l = scm_cdr (l))
      if (elts[i]->internal_has_interface (scm_car (l)))
        new_elts.push_back (elts[i]);

  if (!new_elts.size ())
    return robust_relative_extent (me, common, a);

  if (!common)
    common = common_refpoint_of_array (new_elts, me, a);

  return relative_maybe_bound_group_extent (new_elts, common, a, true);
}

Interval
Axis_group_interface::sum_partial_pure_heights (Grob *me, int start, int end)
{
  Interval iv = begin_of_line_pure_height (me, start);
  iv.unite (rest_of_line_pure_height (me, start, end));

  return iv;
}

Interval
Axis_group_interface::part_of_line_pure_height (Grob *me, bool begin, vsize start, vsize end)
{
  Spanner *sp = dynamic_cast<Spanner *> (me);
  if (!sp)
    return Interval (0, 0);
  SCM cache_symbol = begin
                     ? ly_symbol2scm ("begin-of-line-pure-height")
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
      SCM these_pure_heights = begin
                               ? scm_car (adjacent_pure_heights)
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
Axis_group_interface::rest_of_line_pure_height (Grob *me, vsize start, vsize end)
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
MAKE_SCHEME_CALLBACK (Axis_group_interface, adjacent_pure_heights, 1)
SCM
Axis_group_interface::adjacent_pure_heights (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);

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

      bool outside_staff = scm_is_number (get_property (g, "outside-staff-priority"));
      Real padding = from_scm<double> (get_property (g, "outside-staff-padding"), get_default_outside_staff_padding ());

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

      Interval_t<vsize> rank_span (g->spanned_rank_interval ());
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
            = pure_subst_prebroken_piece (g, start, visibility_end);
          if (Item::break_visible (maybe_subst))
            {
              Interval dims = maybe_subst->pure_y_extent (common, start, end);
              if (!dims.is_empty ())
                {
                  if (rank_span[LEFT] <= start)
                    {
                      if (outside_staff)
                        begin_line_heights[j].unite (begin_line_staff_heights[j].union_disjoint (dims, padding, d));
                      else
                        begin_line_heights[j].unite (dims);
                    }
                  if (rank_span[RIGHT] > start)
                    {
                      if (outside_staff)
                        mid_line_heights[j].unite (mid_line_staff_heights[j].union_disjoint (dims, padding, d));
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
      scm_c_vector_set_x (begin_scm, i,
                          to_scm (begin_line_heights[i]));
      scm_c_vector_set_x (mid_scm, i,
                          to_scm (mid_line_heights[i]));
    }

  return scm_cons (begin_scm, mid_scm);
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
  for (vsize i = 0; i < elts.size (); i++)
    {
      Grob *g = pure_subst_prebroken_piece (elts[i], start, end);

      Interval_t<int> rank_span = g->spanned_rank_interval ();
      if (rank_span[LEFT] <= end && rank_span[RIGHT] >= start
          && Item::break_visible (g)
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

MAKE_SCHEME_CALLBACK (Axis_group_interface, width, 1);
SCM
Axis_group_interface::width (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);
  return generic_group_extent (me, X_AXIS);
}

MAKE_SCHEME_CALLBACK (Axis_group_interface, height, 1);
SCM
Axis_group_interface::height (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);
  return generic_group_extent (me, Y_AXIS);
}

MAKE_SCHEME_CALLBACK (Axis_group_interface, pure_height, 3);
SCM
Axis_group_interface::pure_height (SCM smob, SCM start_scm, SCM end_scm)
{
  int start = from_scm (start_scm, 0);
  int end = from_scm (end_scm, INT_MAX);
  Grob *me = unsmob<Grob> (smob);

  /* Maybe we are in the second pass of a two-pass spacing run. In that
     case, the Y-extent of a system is already given to us */
  System *system = dynamic_cast<System *> (me);
  if (system)
    {
      SCM line_break_details = get_property (system->column (start), "line-break-system-details");
      SCM system_y_extent = scm_assq (ly_symbol2scm ("system-Y-extent"), line_break_details);
      if (scm_is_pair (system_y_extent))
        return scm_cdr (system_y_extent);
    }

  return to_scm (pure_group_height (me, start, end));
}

MAKE_SCHEME_CALLBACK (Axis_group_interface, calc_skylines, 1);
SCM
Axis_group_interface::calc_skylines (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);
  Skyline_pair skylines = skyline_spacing (me);
  return skylines.smobbed_copy ();
}

/* whereas calc_skylines calculates skylines for axis-groups with a lot of
   visible children, combine_skylines is designed for axis-groups whose only
   children are other axis-groups (ie. VerticalAlignment). Rather than
   calculating all the skylines from scratch, we just merge the skylines
   of the children.
*/
MAKE_SCHEME_CALLBACK (Axis_group_interface, combine_skylines, 1);
SCM
Axis_group_interface::combine_skylines (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);
  extract_grob_set (me, "elements", elements);
  Grob *y_common = common_refpoint_of_array (elements, me, Y_AXIS);
  Grob *x_common = common_refpoint_of_array (elements, me, X_AXIS);

  if (y_common != me)
    programming_error ("combining skylines that don't belong to me");

  Skyline_pair ret;
  for (vsize i = 0; i < elements.size (); i++)
    {
      SCM skyline_scm = get_property (elements[i], "vertical-skylines");
      if (unsmob<Skyline_pair> (skyline_scm))
        {
          Real offset = elements[i]->relative_coordinate (y_common, Y_AXIS);
          Skyline_pair other = *unsmob<Skyline_pair> (skyline_scm);
          other.raise (offset);
          other.shift (elements[i]->relative_coordinate (x_common, X_AXIS));
          ret.merge (other);
        }
    }
  return ret.smobbed_copy ();
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
Axis_group_interface::staff_extent (Grob *me, Grob *refp, Axis ext_a, Grob *staff, Axis parent_a)
{
  extract_grob_set (me, "elements", elts);
  vector<Grob *> new_elts;

  for (vsize i = 0; i < elts.size (); i++)
    if (elts[i]->common_refpoint (staff, parent_a) == staff)
      new_elts.push_back (elts[i]);

  return relative_group_extent (new_elts, refp, ext_a);
}

MAKE_SCHEME_CALLBACK (Axis_group_interface, calc_pure_relevant_grobs, 1);
SCM
Axis_group_interface::calc_pure_relevant_grobs (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);
  /* TODO: Filter out elements that belong to a different Axis_group,
     such as the tie in
     << \new Staff=A { c'1~ \change Staff=B c'}
        \new Staff=B { \clef bass R1 R } >>
    because thier location relative to this Axis_group is not known before
    page layout.  For now, we need to trap this case in calc_pure_y_common.
  */
  return internal_calc_pure_relevant_grobs (me, "elements");
}

SCM
Axis_group_interface::internal_calc_pure_relevant_grobs (Grob *me, const string &grob_set_name)
{
  extract_grob_set (me, grob_set_name.c_str (), elts);

  vector<Grob *> relevant_grobs;

  for (vsize i = 0; i < elts.size (); i++)
    {
      if (Item *it = dynamic_cast<Item *> (elts[i]))
        {
          if (it->original ())
            continue;
        }
      /* This might include potentially suicided items. Callers should
         look at the relevant prebroken clone where necesary */
      relevant_grobs.push_back (elts[i]);
    }

  std::sort (relevant_grobs.begin (), relevant_grobs.end (),
             pure_staff_priority_less);
  SCM grobs_scm = Grob_array::make_array ();
  unsmob<Grob_array> (grobs_scm)->set_array (relevant_grobs);

  return grobs_scm;
}

MAKE_SCHEME_CALLBACK (Axis_group_interface, calc_pure_y_common, 1);
SCM
Axis_group_interface::calc_pure_y_common (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);

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

MAKE_SCHEME_CALLBACK (Axis_group_interface, calc_x_common, 1);
SCM
Axis_group_interface::calc_x_common (SCM grob)
{
  return calc_common (unsmob<Grob> (grob), X_AXIS);
}

MAKE_SCHEME_CALLBACK (Axis_group_interface, calc_y_common, 1);
SCM
Axis_group_interface::calc_y_common (SCM grob)
{
  return calc_common (unsmob<Grob> (grob), Y_AXIS);
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

static bool
staff_priority_less (Grob *const &g1, Grob *const &g2)
{
  Real priority_1 = from_scm<double> (get_property (g1, "outside-staff-priority"), -infinity_f);
  Real priority_2 = from_scm<double> (get_property (g2, "outside-staff-priority"), -infinity_f);

  if (priority_1 < priority_2)
    return true;
  else if (priority_1 > priority_2)
    return false;

  /* if neither grob has an outside-staff priority, the ordering will have no
     effect and we assume the two grobs to be equal (none of the two is less).
     We do this to avoid the side-effect of calculating extents. */
  if (std::isinf (priority_1) && std::isinf (priority_2))
    return false;

  /* if there is no preference in staff priority, choose the left-most one */
  Grob *common = g1->common_refpoint (g2, X_AXIS);
  Real start_1 = g1->extent (common, X_AXIS)[LEFT];
  Real start_2 = g2->extent (common, X_AXIS)[LEFT];
  return start_1 < start_2;
}

static bool
pure_staff_priority_less (Grob *const &g1, Grob *const &g2)
{
  Real priority_1 = from_scm<double> (get_property (g1, "outside-staff-priority"), -infinity_f);
  Real priority_2 = from_scm<double> (get_property (g2, "outside-staff-priority"), -infinity_f);

  return priority_1 < priority_2;
}

// Raises the grob elt (whose skylines are given by h_skyline
// and v_skyline) so that it doesn't intersect with staff_skyline,
// or with anything in other_h_skylines and other_v_skylines.
void
avoid_outside_staff_collisions (Grob *elt,
                                Skyline_pair *v_skyline,
                                Real padding,
                                Real horizon_padding,
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
                           Drul_array<vector<Skyline_pair> > *all_v_skylines,
                           Drul_array<vector<Real> > *all_paddings,
                           Drul_array<vector<Real> > *all_horizon_paddings,
                           vector<Grob *> elements,
                           Grob *x_common,
                           Grob *y_common,
                           multimap<Grob *, Grob *> const &riders)
{

  SCM directive
    = valid_outside_staff_placement_directive (me);

  bool l2r = (scm_is_eq (directive, ly_symbol2scm ("left-to-right-greedy"))
              || scm_is_eq (directive, ly_symbol2scm ("left-to-right-polite")));

  bool polite = (scm_is_eq (directive, ly_symbol2scm ("left-to-right-polite"))
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
           l2r ? i < elements.size () : i--;
           l2r ? i++ : 0)
        {
          Grob *elt = elements[i];
          Real padding
            = from_scm<double> (get_property (elt, "outside-staff-padding"),
                                Axis_group_interface
                                ::get_default_outside_staff_padding ());
          Real horizon_padding
            = from_scm<double> (get_property (elt, "outside-staff-horizontal-padding"), 0.0);
          Interval x_extent = elt->extent (x_common, X_AXIS);
          x_extent.widen (horizon_padding);

          Direction dir = get_grob_direction (elt);
          if (dir == CENTER)
            {
              warning (_ ("an outside-staff object should have a direction, defaulting to up"));
              dir = UP;
            }

          if (x_extent[LEFT] <= last_end[dir] && polite)
            {
              skipped_elements.push_back (elt);
              continue;
            }
          last_end[dir] = x_extent[RIGHT];

          Skyline_pair *v_orig = unsmob<Skyline_pair> (get_property (elt, "vertical-skylines"));
          if (!v_orig || v_orig->is_empty ())
            continue;

          // Find the riders associated with this grob, and merge their
          // skylines with elt's skyline.
          typedef multimap<Grob *, Grob *>::const_iterator GrobMapIterator;
          pair<GrobMapIterator, GrobMapIterator> range = riders.equal_range (elt);
          vector<Skyline_pair> rider_v_skylines;
          for (GrobMapIterator j = range.first; j != range.second; j++)
            {
              Grob *rider = j->second;
              Skyline_pair *v_rider = unsmob<Skyline_pair> (get_property (rider, "vertical-skylines"));
              if (v_rider)
                {
                  Skyline_pair copy (*v_rider);
                  copy.shift (rider->relative_coordinate (x_common, X_AXIS));
                  copy.raise (rider->relative_coordinate (y_common, Y_AXIS));
                  rider_v_skylines.push_back (copy);
                }
            }
          Skyline_pair v_skylines (*v_orig);
          v_skylines.shift (elt->relative_coordinate (x_common, X_AXIS));
          v_skylines.raise (elt->relative_coordinate (y_common, Y_AXIS));
          v_skylines.merge (Skyline_pair (rider_v_skylines));

          avoid_outside_staff_collisions (elt,
                                          &v_skylines,
                                          padding,
                                          horizon_padding,
                                          (*all_v_skylines)[dir],
                                          (*all_paddings)[dir],
                                          (*all_horizon_paddings)[dir],
                                          dir);

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
  extract_grob_set (me,
                    unsmob<Grob_array> (get_object (me, "vertical-skyline-elements"))
                    ? "vertical-skyline-elements"
                    : "elements",
                    fakeelements);
  vector<Grob *> elements (fakeelements);
  for (vsize i = 0; i < elements.size (); i++)
    /*
      As a sanity check, we make sure that no grob with an outside staff priority
      has a Y-parent that also has an outside staff priority, which would result
      in two movings.
    */
    if (scm_is_number (get_property (elements[i], "outside-staff-priority"))
        && outside_staff_ancestor (elements[i]))
      {
        elements[i]->warning (_ ("Cannot set outside-staff-priority for element and elements' Y parent."));
        set_property (elements[i], "outside-staff-priority", SCM_BOOL_F);
      }

  /* For grobs with an outside-staff-priority, the sorting function might
     call extent and cause suicide. This breaks the contract that is required
     for the STL sort function. To avoid this, we make sure that any suicides
     are triggered beforehand.
  */
  for (vsize i = 0; i < elements.size (); i++)
    if (scm_is_number (get_property (elements[i], "outside-staff-priority")))
      elements[i]->extent (elements[i], X_AXIS);

  std::stable_sort (elements.begin (), elements.end (), staff_priority_less);
  Grob *x_common = common_refpoint_of_array (elements, me, X_AXIS);
  Grob *y_common = common_refpoint_of_array (elements, me, Y_AXIS);

  if (y_common != me)
    {
      me->programming_error ("Some of my vertical-skyline-elements"
                             " are outside my VerticalAxisGroup.");
      y_common = me;
    }

  // A rider is a grob that is not outside-staff, but has an outside-staff
  // ancestor.  In that case, the rider gets moved along with its ancestor.
  multimap<Grob *, Grob *> riders;

  vsize i = 0;
  vector<Skyline_pair> inside_staff_skylines;

  for (i = 0;
       i < elements.size ()
       && !scm_is_number (get_property (elements[i], "outside-staff-priority"));
       i++)
    {
      Grob *elt = elements[i];
      if (Grob *ancestor = outside_staff_ancestor (elt))
        riders.insert (pair<Grob *, Grob *> (ancestor, elt));
      else if (!from_scm<bool> (get_property (elt, "cross-staff")))
        {
          Skyline_pair *maybe_pair
            = unsmob<Skyline_pair> (get_property (elt, "vertical-skylines"));
          if (!maybe_pair)
            continue;
          if (maybe_pair->is_empty ())
            continue;
          inside_staff_skylines.push_back (*maybe_pair);
          inside_staff_skylines.back ().shift (elt->relative_coordinate (x_common, X_AXIS));
          inside_staff_skylines.back ().raise (elt->relative_coordinate (y_common, Y_AXIS));
        }
    }

  Skyline_pair skylines (inside_staff_skylines);

  // These are the skylines of all outside-staff grobs
  // that have already been processed.  We keep them around in order to
  // check them for collisions with the currently active outside-staff grob.
  Drul_array<vector<Skyline_pair> > all_v_skylines;
  Drul_array<vector<Real> > all_paddings;
  Drul_array<vector<Real> > all_horizon_paddings;
  for (UP_and_DOWN (d))
    {
      all_v_skylines[d].push_back (skylines);
      all_paddings[d].push_back (0);
      all_horizon_paddings[d].push_back (0);
    }

  for (; i < elements.size (); i++)
    {
      if (from_scm<bool> (get_property (elements[i], "cross-staff")))
        continue;

      // Collect all the outside-staff grobs that have a particular priority.
      SCM priority = get_property (elements[i], "outside-staff-priority");
      vector<Grob *> current_elts;
      current_elts.push_back (elements[i]);
      while (i + 1 < elements.size ()
             && ly_is_equal (get_property (elements[i + 1], "outside-staff-priority"), priority))
        {
          if (!from_scm<bool> (get_property (elements[i + 1], "cross-staff")))
            current_elts.push_back (elements[i + 1]);
          ++i;
        }

      add_grobs_of_one_priority (me,
                                 &all_v_skylines,
                                 &all_paddings,
                                 &all_horizon_paddings,
                                 current_elts,
                                 x_common,
                                 y_common,
                                 riders);
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

MAKE_SCHEME_CALLBACK (Axis_group_interface, print, 1)
SCM
Axis_group_interface::print (SCM smob)
{
  if (!debug_skylines)
    return SCM_BOOL_F;

  Grob *me = unsmob<Grob> (smob);
  Stencil ret;
  if (Skyline_pair *s = unsmob<Skyline_pair> (get_property (me, "vertical-skylines")))
    {
      ret.add_stencil (Lookup::points_to_line_stencil (0.1, (*s)[UP].to_points (X_AXIS))
                       .in_color (1.0, 0.0, 1.0));
      ret.add_stencil (Lookup::points_to_line_stencil (0.1, (*s)[DOWN].to_points (X_AXIS))
                       .in_color (0.0, 1.0, 1.0));
    }
  return ret.smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Axis_group_interface, calc_pure_staff_staff_spacing, 3)
SCM
Axis_group_interface::calc_pure_staff_staff_spacing (SCM smob, SCM start, SCM end)
{
  return calc_maybe_pure_staff_staff_spacing (unsmob<Grob> (smob),
                                              true,
                                              scm_to_int (start),
                                              scm_to_int (end));
}

MAKE_SCHEME_CALLBACK (Axis_group_interface, calc_staff_staff_spacing, 1)
SCM
Axis_group_interface::calc_staff_staff_spacing (SCM smob)
{
  return calc_maybe_pure_staff_staff_spacing (unsmob<Grob> (smob),
                                              false,
                                              0,
                                              INT_MAX);
}

SCM
Axis_group_interface::calc_maybe_pure_staff_staff_spacing (Grob *me, bool pure, int start, int end)
{
  Grob *grouper = unsmob<Grob> (get_object (me, "staff-grouper"));

  if (grouper)
    {
      bool within_group = Staff_grouper_interface::maybe_pure_within_group (grouper, me, pure, start, end);
      if (within_group)
        return get_maybe_pure_property (grouper, "staff-staff-spacing", pure, start, end);
      else
        return get_maybe_pure_property (grouper, "staffgroup-staff-spacing", pure, start, end);
    }
  return get_maybe_pure_property (me, "default-staff-staff-spacing", pure, start, end);
}

ADD_INTERFACE (Axis_group_interface,
               "An object that groups other layout objects.",

               // TODO: some of these properties are specific to
               // VerticalAxisGroup. We should split off a
               // vertical-axis-group-interface.
               /* properties */
               "adjacent-pure-heights "
               "axes "
               "bound-alignment-interfaces "
               "default-staff-staff-spacing "
               "elements "
               "no-alignment "
               "nonstaff-nonstaff-spacing "
               "nonstaff-relatedstaff-spacing "
               "nonstaff-unrelatedstaff-spacing "
               "pure-relevant-grobs "
               "pure-relevant-items "
               "pure-relevant-spanners "
               "pure-Y-common "
               "staff-affinity "
               "staff-grouper "
               "staff-staff-spacing "
               "system-Y-offset "
               "X-common "
               "Y-common "
              );
