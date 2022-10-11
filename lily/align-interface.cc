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

#include "align-interface.hh"
#include "axis-group-interface.hh"
#include "grob-array.hh"
#include "hara-kiri-group-spanner.hh"
#include "international.hh"
#include "item.hh"
#include "page-layout-problem.hh"
#include "paper-column.hh"
#include "pointer-group-interface.hh"
#include "spanner.hh"
#include "skyline-pair.hh"
#include "system.hh"
#include "warn.hh"

using std::vector;

MAKE_SCHEME_CALLBACK (Align_interface, align_to_minimum_distances,
                      "ly:align-interface::align-to-minimum-distances", 1);
SCM
Align_interface::align_to_minimum_distances (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);

  set_property (me, "positioning-done", SCM_BOOL_T);

  Align_interface::align_elements_to_minimum_distances (me, axis (me));

  return SCM_BOOL_T;
}

MAKE_SCHEME_CALLBACK (Align_interface, align_to_ideal_distances,
                      "ly:align-interface::align-to-ideal-distances", 1);
SCM
Align_interface::align_to_ideal_distances (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);

  set_property (me, "positioning-done", SCM_BOOL_T);

  Align_interface::align_elements_to_ideal_distances (me);

  return SCM_BOOL_T;
}

/* Return upper and lower skylines for VerticalAxisGroup g. If the extent
   is non-empty but there is no skyline available (or pure is true), just
   create a flat skyline from the bounding box */
// TODO(jneem): the pure and non-pure parts seem to share very little
// code. Split them into 2 functions, perhaps?
static Skyline_pair
get_skylines (Grob *g, Axis a, Grob *other_common, bool pure, vsize start,
              vsize end)
{
  if (!pure)
    {
      SCM sym = (a == Y_AXIS) ? ly_symbol2scm ("vertical-skylines")
                              : ly_symbol2scm ("horizontal-skylines");
      SCM skyp_scm = get_property (g, sym);
      Skyline_pair skylines;
      if (is_scm<Skyline_pair> (skyp_scm))
        {
          skylines = from_scm<Skyline_pair> (skyp_scm);
          /* This skyline was calculated relative to the grob g. In order to compare it to
          skylines belonging to other grobs, we need to shift it so that it is relative
          to the common reference. */
          Real offset = g->relative_coordinate (other_common, other_axis (a));
          skylines.shift (offset);
        }

      return skylines;
    }

  if (Hara_kiri_group_spanner::request_suicide (g, start, end))
    return Skyline_pair ();

  assert (a == Y_AXIS);
  Interval extent = g->pure_y_extent (g, start, end);
  vector<Box> boxes;

  // This is a hack to get better accuracy on the pure-height of VerticalAlignment.
  // It's quite common for a treble clef to be the highest element of one system
  // and for a low note (or lyrics) to be the lowest note on another. The two will
  // never collide, but the pure-height stuff only works with bounding boxes, so it
  // doesn't know that. The result is a significant over-estimation of the pure-height,
  // especially on systems with many staves. To correct for this, we build a skyline
  // in two parts: the part we did above contains most of the grobs (note-heads, etc.)
  // while the bit we're about to do only contains the breakable grobs at the beginning
  // of the system. This way, the tall treble clefs are only compared with the treble
  // clefs of the other staff and they will be ignored if the staff above is, for example,
  // lyrics.
  if (has_interface<Axis_group_interface> (g))
    {
      extent = Axis_group_interface::rest_of_line_pure_height (g, start, end);
      Interval begin_of_line_extent
        = Axis_group_interface::begin_of_line_pure_height (g, start);
      if (!begin_of_line_extent.is_empty ())
        {
          boxes.push_back (
            Box (Interval (-infinity_f, -1), begin_of_line_extent));
        }
    }

  if (!extent.is_empty ())
    {
      boxes.push_back (Box (Interval (0, infinity_f), extent));
    }

  return Skyline_pair (boxes, X_AXIS);
}

vector<Real>
Align_interface::get_minimum_translations (Grob *me,
                                           vector<Grob *> const &all_grobs,
                                           Axis a)
{
  return internal_get_minimum_translations (me, all_grobs, a, true, false, 0,
                                            0);
}

vector<Real>
Align_interface::get_pure_minimum_translations (Grob *me,
                                                vector<Grob *> const &all_grobs,
                                                Axis a, vsize start, vsize end)
{
  return internal_get_minimum_translations (me, all_grobs, a, true, true, start,
                                            end);
}

vector<Real>
Align_interface::get_minimum_translations_without_min_dist (
  Grob *me, vector<Grob *> const &all_grobs, Axis a)
{
  return internal_get_minimum_translations (me, all_grobs, a, false, false, 0,
                                            0);
}

// If include_fixed_spacing is false, the only constraints that will be measured
// here are those that result from collisions (+ padding) and the spacing spec
// between adjacent staves.
// If include_fixed_spacing is true, constraints from line-break-system-details,
// basic-distance+stretchable=0, and staff-staff-spacing of spaceable staves
// with loose lines in between, are included as well.
// - If you want to find the minimum height of a system, include_fixed_spacing should be true.
// - If you're going to actually lay out the page, then it should be false (or
//   else centered dynamics will break when there is a fixed alignment).
vector<Real>
Align_interface::internal_get_minimum_translations (
  Grob *me, vector<Grob *> const &elems, Axis a, bool include_fixed_spacing,
  bool pure, vsize start, vsize end)
{
  if (!pure && a == Y_AXIS && dynamic_cast<Spanner *> (me)
      && !me->get_system ())
    me->programming_error ("vertical alignment called before line-breaking");

  // check the cache
  if (pure)
    {
      SCM fv = ly_assoc_get (scm_cons (to_scm (start), to_scm (end)),
                             get_property (me, "minimum-translations-alist"),
                             SCM_EOL);
      if (!scm_is_null (fv))
        return from_scm_list<std::vector<Real>> (fv);
    }

  // If include_fixed_spacing is true, we look at things like system-system-spacing
  // and alignment-distances, which only make sense for the toplevel VerticalAlignment.
  // If we aren't toplevel, we're working on something like BassFigureAlignment
  // and so we definitely don't want to include alignment-distances!
  if (!dynamic_cast<System *> (me->get_y_parent ()))
    include_fixed_spacing = false;

  Direction stacking_dir = from_scm (get_property (me, "stacking-dir"), DOWN);

  Grob *other_common = common_refpoint_of_array (elems, me, other_axis (a));

  Real where = 0;
  Real default_padding = from_scm<double> (get_property (me, "padding"), 0.0);
  vector<Real> translates;
  Skyline down_skyline (stacking_dir);
  Grob *last_nonempty_element = 0;
  Real last_spaceable_element_pos = 0;
  Grob *last_spaceable_element = 0;
  Skyline last_spaceable_skyline (stacking_dir);
  int spaceable_count = 0;
  for (vsize j = 0; j < elems.size (); j++)
    {
      Real dy = 0;
      Real padding = default_padding;

      Skyline_pair skyline
        = get_skylines (elems[j], a, other_common, pure, start, end);

      if (skyline.is_empty ())
        {
          translates.push_back (where);
          continue;
        }

      if (!last_nonempty_element)
        {
          dy = skyline[-stacking_dir].max_height () + padding;
          for (vsize k = j; k-- > 0;)
            translates[k] = stacking_dir * dy;
        }
      else
        {
          SCM spec = Page_layout_problem::get_spacing_spec (
            last_nonempty_element, elems[j], pure, start, end);
          Page_layout_problem::read_spacing_spec (spec, &padding,
                                                  ly_symbol2scm ("padding"));

          dy = down_skyline.distance (skyline[-stacking_dir]) + padding;

          Real spec_distance = 0;
          if (Page_layout_problem::read_spacing_spec (
                spec, &spec_distance, ly_symbol2scm ("minimum-distance")))
            dy = std::max (dy, spec_distance);
          // Consider the likely final spacing when estimating distance between staves of the full score
          if (INT_MAX == end && 0 == start
              && Page_layout_problem::read_spacing_spec (
                spec, &spec_distance, ly_symbol2scm ("basic-distance")))
            dy = std::max (dy, spec_distance);

          if (include_fixed_spacing
              && Page_layout_problem::is_spaceable (elems[j])
              && last_spaceable_element)
            {
              // Spaceable staves may have
              // constraints coming from the previous spaceable staff
              // as well as from the previous staff.
              spec = Page_layout_problem::get_spacing_spec (
                last_spaceable_element, elems[j], pure, start, end);
              Real spaceable_padding = 0;
              Page_layout_problem::read_spacing_spec (
                spec, &spaceable_padding, ly_symbol2scm ("padding"));
              dy = std::max (
                dy, (last_spaceable_skyline.distance (skyline[-stacking_dir])
                     + stacking_dir * (last_spaceable_element_pos - where)
                     + spaceable_padding));

              Real spaceable_min_distance = 0;
              if (Page_layout_problem::read_spacing_spec (
                    spec, &spaceable_min_distance,
                    ly_symbol2scm ("minimum-distance")))
                dy = std::max (
                  dy, spaceable_min_distance
                        + stacking_dir * (last_spaceable_element_pos - where));

              dy = std::max (dy, Page_layout_problem::get_fixed_spacing (
                                   last_spaceable_element, elems[j],
                                   spaceable_count, pure, start, end));
            }
        }

      dy = std::max (0.0, dy);
      down_skyline.raise (-stacking_dir * dy);
      down_skyline.merge (skyline[stacking_dir]);
      where += stacking_dir * dy;
      translates.push_back (where);

      if (Page_layout_problem::is_spaceable (elems[j]))
        {
          spaceable_count++;
          last_spaceable_element = elems[j];
          last_spaceable_element_pos = where;
          last_spaceable_skyline = down_skyline;
        }
      last_nonempty_element = elems[j];
    }

  if (pure)
    {
      SCM mta = get_property (me, "minimum-translations-alist");
      mta = scm_cons (scm_cons (scm_cons (to_scm (start), to_scm (end)),
                                to_scm_list (translates)),
                      mta);
      set_property (me, "minimum-translations-alist", mta);
    }
  return translates;
}

void
Align_interface::align_elements_to_ideal_distances (Grob *me)
{
  System *sys = me->get_system ();
  if (sys)
    {
      Page_layout_problem layout (NULL, SCM_EOL, ly_list (sys->self_scm ()));
      layout.solution (true);
    }
  else
    programming_error ("vertical alignment called before line breaking");
}

void
Align_interface::align_elements_to_minimum_distances (Grob *me, Axis a)
{
  extract_grob_set (me, "elements", all_grobs);

  vector<Real> translates = get_minimum_translations (me, all_grobs, a);
  if (translates.size ())
    for (vsize j = 0; j < all_grobs.size (); j++)
      all_grobs[j]->translate_axis (translates[j], a);
}

Real
Align_interface::get_pure_child_y_translation (Grob *me, Grob *ch, vsize start,
                                               vsize end)
{
  extract_grob_set (me, "elements", all_grobs);
  vector<Real> translates
    = get_pure_minimum_translations (me, all_grobs, Y_AXIS, start, end);

  if (translates.size ())
    {
      for (vsize i = 0; i < all_grobs.size (); i++)
        if (all_grobs[i] == ch)
          return translates[i];
    }
  else
    return 0;

  programming_error (
    "tried to get a translation for something that is no child of mine");
  return 0;
}

Axis
Align_interface::axis (Grob *me)
{
  return from_scm<Axis> (scm_car (get_property (me, "axes")), X_AXIS);
}

void
Align_interface::add_element (Grob *me, Grob *element)
{
  Axis a = Align_interface::axis (me);
  SCM sym = axis_offset_symbol (a);
  SCM proc = axis_parent_positioning (a);

  set_property (element, sym, proc);
  Axis_group_interface::add_element (me, element);
}

ADD_INTERFACE (Align_interface,
               R"(
Order grobs from top to bottom, left to right, right to left or bottom to top.
For vertical alignments of staves, the @code{line-break-system-details} of the
left @rinternals{NonMusicalPaperColumn} may be set to tune vertical spacing.
               )",

               /* properties */
               R"(
align-dir
axes
elements
minimum-translations-alist
padding
positioning-done
stacking-dir
               )");
