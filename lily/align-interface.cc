/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "paper-book.hh"
#include "paper-column.hh"
#include "pointer-group-interface.hh"
#include "skyline-pair.hh"
#include "spanner.hh"
#include "system.hh"
#include "warn.hh"

using std::vector;

MAKE_SCHEME_CALLBACK (Align_interface, align_to_minimum_distances, 1);
SCM
Align_interface::align_to_minimum_distances (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);

  me->set_property ("positioning-done", SCM_BOOL_T);

  SCM axis = scm_car (me->get_property ("axes"));
  Axis ax = Axis (scm_to_int (axis));

  Align_interface::align_elements_to_minimum_distances (me, ax);

  return SCM_BOOL_T;
}

MAKE_SCHEME_CALLBACK (Align_interface, align_to_ideal_distances, 1);
SCM
Align_interface::align_to_ideal_distances (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);

  me->set_property ("positioning-done", SCM_BOOL_T);

  Align_interface::align_elements_to_ideal_distances (me);

  return SCM_BOOL_T;
}

/* Return upper and lower skylines for VerticalAxisGroup g. If the extent
   is non-empty but there is no skyline available (or pure is true), just
   create a flat skyline from the bounding box */
// TODO(jneem): the pure and non-pure parts seem to share very little
// code. Split them into 2 functions, perhaps?
static Skyline_pair
get_skylines (Grob *g, Axis a, Grob *other_common, bool pure, int start,
              int end)
{
  Skyline_pair skylines;

  if (!pure)
    {
      Skyline_pair *skys = unsmob<Skyline_pair> (g->get_property (
          a == Y_AXIS ? "vertical-skylines" : "horizontal-skylines"));
      if (skys)
        skylines = *skys;

      /* This skyline was calculated relative to the grob g. In order to compare
         it to skylines belonging to other grobs, we need to shift it so that it
         is relative to the common reference. */
      Real offset = g->relative_coordinate (other_common, other_axis (a));
      skylines.shift (offset);
    }
  else if (Hara_kiri_group_spanner::request_suicide (g, start, end))
    return skylines;
  else
    {
      assert (a == Y_AXIS);
      Interval extent = g->pure_y_extent (g, start, end);

      // This is a hack to get better accuracy on the pure-height of
      // VerticalAlignment. It's quite common for a treble clef to be the
      // highest element of one system and for a low note (or lyrics) to be the
      // lowest note on another. The two will never collide, but the pure-height
      // stuff only works with bounding boxes, so it doesn't know that. The
      // result is a significant over-estimation of the pure-height, especially
      // on systems with many staves. To correct for this, we build a skyline in
      // two parts: the part we did above contains most of the grobs
      // (note-heads, etc.) while the bit we're about to do only contains the
      // breakable grobs at the beginning of the system. This way, the tall
      // treble clefs are only compared with the treble clefs of the other staff
      // and they will be ignored if the staff above is, for example, lyrics.
      if (has_interface<Axis_group_interface> (g))
        {
          extent
              = Axis_group_interface::rest_of_line_pure_height (g, start, end);
          Interval begin_of_line_extent
              = Axis_group_interface::begin_of_line_pure_height (g, start);
          if (!begin_of_line_extent.is_empty ())
            {
              Box b;
              b[a] = begin_of_line_extent;
              b[other_axis (a)] = Interval (-infinity_f, -1);
              skylines.insert (b, other_axis (a));
            }
        }

      if (!extent.is_empty ())
        {
          Box b;
          b[a] = extent;
          b[other_axis (a)] = Interval (0, infinity_f);
          skylines.insert (b, other_axis (a));
        }
    }
  return skylines;
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
                                                Axis a, int start, int end)
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
// - If you want to find the minimum height of a system, include_fixed_spacing
// should be true.
// - If you're going to actually lay out the page, then it should be false (or
//   else centered dynamics will break when there is a fixed alignment).
vector<Real>
Align_interface::internal_get_minimum_translations (
    Grob *me, vector<Grob *> const &elems, Axis a, bool include_fixed_spacing,
    bool pure, int start, int end)
{
  if (!pure && a == Y_AXIS && dynamic_cast<Spanner *> (me)
      && !me->get_system ())
    me->programming_error ("vertical alignment called before line-breaking");

  // check the cache
  if (pure)
    {
      SCM fv = ly_assoc_get (
          scm_cons (scm_from_int (start), scm_from_int (end)),
          me->get_property ("minimum-translations-alist"), SCM_EOL);
      if (!scm_is_null (fv))
        return ly_scm2floatvector (fv);
    }

  // If include_fixed_spacing is true, we look at things like
  // system-system-spacing and alignment-distances, which only make sense for
  // the toplevel VerticalAlignment. If we aren't toplevel, we're working on
  // something like BassFigureAlignment and so we definitely don't want to
  // include alignment-distances!
  if (!dynamic_cast<System *> (me->get_parent (Y_AXIS)))
    include_fixed_spacing = false;

  Direction stacking_dir
      = robust_scm2dir (me->get_property ("stacking-dir"), DOWN);

  Grob *other_common = common_refpoint_of_array (elems, me, other_axis (a));

  Real where = 0;
  Real default_padding = robust_scm2double (me->get_property ("padding"), 0.0);
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
          // Consider the likely final spacing when estimating distance between
          // staves of the full score
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
                    dy,
                    spaceable_min_distance
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
      SCM mta = me->get_property ("minimum-translations-alist");
      mta = scm_cons (
          scm_cons (scm_cons (scm_from_int (start), scm_from_int (end)),
                    ly_floatvector2scm (translates)),
          mta);
      me->set_property ("minimum-translations-alist", mta);
    }
  return translates;
}

void
Align_interface::align_elements_to_ideal_distances (Grob *me)
{
  System *sys = me->get_system ();
  if (sys)
    {
      Page_layout_problem layout (NULL, SCM_EOL, scm_list_1 (sys->self_scm ()));
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
Align_interface::get_pure_child_y_translation (Grob *me, Grob *ch, int start,
                                               int end)
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
  return Axis (scm_to_int (scm_car (me->get_property ("axes"))));
}

void
Align_interface::add_element (Grob *me, Grob *element)
{
  Axis a = Align_interface::axis (me);
  SCM sym = axis_offset_symbol (a);
  SCM proc = axis_parent_positioning (a);

  element->set_property (sym, proc);
  Axis_group_interface::add_element (me, element);
}

void
Align_interface::set_ordered (Grob *me)
{
  SCM ga_scm = me->get_object ("elements");
  Grob_array *ga = unsmob<Grob_array> (ga_scm);
  if (!ga)
    {
      ga_scm = Grob_array::make_array ();
      ga = unsmob<Grob_array> (ga_scm);
      me->set_object ("elements", ga_scm);
    }

  ga->set_ordered (true);
}

ADD_INTERFACE (Align_interface,
               "Order grobs from top to bottom, left to right, right to left"
               " or bottom to top.  For vertical alignments of staves, the"
               " @code{line-break-system-details} of the left"
               " @rinternals{NonMusicalPaperColumn} may be set to tune"
               " vertical spacing.",

               /* properties */
               "align-dir "
               "axes "
               "elements "
               "minimum-translations-alist "
               "padding "
               "positioning-done "
               "stacking-dir ");
