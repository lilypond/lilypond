/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "slur.hh"

#include "grob-info.hh"
#include "grob-array.hh"
#include "beam.hh"
#include "bezier.hh"
#include "directional-element-interface.hh"
#include "font-interface.hh"
#include "item.hh"
#include "pointer-group-interface.hh"
#include "lookup.hh"
#include "ly-scm-list.hh"
#include "note-column.hh"
#include "output-def.hh"
#include "skyline-pair.hh"
#include "spanner.hh"
#include "staff-symbol-referencer.hh"
#include "stem.hh"
#include "text-interface.hh"
#include "tie.hh"
#include "warn.hh"
#include "slur-scoring.hh"
#include "separation-item.hh"
#include "unpure-pure-container.hh"
#include "international.hh"

using std::string;
using std::vector;

MAKE_SCHEME_CALLBACK (Slur, calc_direction, "ly:slur::calc-direction", 1)
SCM
Slur::calc_direction (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  extract_grob_set (me, "note-columns", encompasses);

  if (encompasses.empty ())
    {
      me->suicide ();
      return SCM_BOOL_F;
    }

  auto d = DOWN;
  for (const auto &col : encompasses)
    {
      if (!Note_column::has_rests (col) && Note_column::dir (col) == DOWN)
        {
          d = UP;
          break;
        }
    }
  return to_scm (d);
}

MAKE_SCHEME_CALLBACK (Slur, pure_height, "ly:slur::pure-height", 3);
SCM
Slur::pure_height (SCM smob, SCM start_scm, SCM end_scm)
{
  /*
    Note that this estimation uses a rote add-on of 0.5 to the
    highest encompassed note-head for a slur estimate.  This is,
    in most cases, shorter than the actual slur.

    Ways to improve this could include:
    -- adding extra height for scripts that avoid slurs on the inside
    -- adding extra height for the "bulge" in a slur above a note head
  */
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  int start = from_scm<int> (start_scm);
  int end = from_scm<int> (end_scm);
  Direction dir = get_grob_direction (me);

  extract_grob_set (me, "note-columns", encompasses);
  Interval ret;
  ret.set_empty ();

  Grob *parent = me->get_y_parent ();
  Drul_array<Real> extremal_heights (infinity_f, -infinity_f);
  if (common_refpoint_of_array (encompasses, me, Y_AXIS) != parent)
    /* this could happen if, for example, we are a cross-staff slur.
       in this case, we want to be ignored */
    return to_scm (Interval ());

  for (vsize i = 0; i < encompasses.size (); i++)
    {
      Interval d = encompasses[i]->pure_y_extent (parent, start, end);
      if (!d.is_empty ())
        {
          ret.add_point (d[dir]);

          if (extremal_heights[LEFT] == infinity_f)
            extremal_heights[LEFT] = d[dir];
          extremal_heights[RIGHT] = d[dir];
        }
    }

  if (ret.is_empty ())
    return to_scm (Interval ());

  Interval extremal_span;
  extremal_span.set_empty ();
  for (const auto d : {LEFT, RIGHT})
    extremal_span.add_point (extremal_heights[d]);
  ret[-dir] = minmax (dir, extremal_span[-dir], ret[-dir]);

  /*
    The +0.5 comes from the fact that we try to place a slur
    0.5 staff spaces from the note-head.
    (see Slur_score_state.get_base_attachments ())
  */
  ret += 0.5 * dir;
  return to_scm (ret);
}

MAKE_SCHEME_CALLBACK (Slur, height, "ly:slur::height", 1);
SCM
Slur::height (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);

  // FIXME uncached
  auto *m = me->get_stencil ();
  return m ? to_scm (m->extent (Y_AXIS)) : to_scm (Interval ());
}

MAKE_SCHEME_CALLBACK (Slur, print, "ly:slur::print", 1);
SCM
Slur::print (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  extract_grob_set (me, "note-columns", encompasses);
  if (encompasses.empty ())
    {
      me->suicide ();
      return SCM_EOL;
    }

  Real staff_thick = Staff_symbol_referencer::line_thickness (me);
  Real base_thick
    = staff_thick * from_scm<double> (get_property (me, "thickness"), 1);
  Real line_thick
    = staff_thick * from_scm<double> (get_property (me, "line-thickness"), 1);

  Bezier one = get_curve (me);
  Stencil a;

  SCM dash_definition = get_property (me, "dash-definition");
  a = Lookup::slur (one, get_grob_direction (me) * base_thick, line_thick,
                    dash_definition);

  SCM annotation = get_property (me, "annotation");
  if (scm_is_string (annotation))
    {
      string str;
      SCM properties = Font_interface::text_font_alist_chain (me);

      if (!scm_is_number (get_property (me, "font-size")))
        properties = scm_cons (
          scm_acons (ly_symbol2scm ("font-size"), to_scm (-6), SCM_EOL),
          properties);

      auto tm = Text_interface::interpret_markup (me->layout (), properties,
                                                  annotation);
      a.add_at_edge (Y_AXIS, get_grob_direction (me), tm, 1.0);
    }

  return a.smobbed_copy ();
}

/*
  it would be better to do this at engraver level, but that is
  fragile, as the breakable items are generated on staff level, at
  which point slur starts and ends have to be tracked
*/
void
Slur::replace_breakable_encompass_objects (Grob *me)
{
  extract_grob_set (me, "encompass-objects", extra_objects);
  vector<Grob *> new_encompasses;

  for (vsize i = 0; i < extra_objects.size (); i++)
    {
      Grob *g = extra_objects[i];

      if (has_interface<Separation_item> (g))
        {
          extract_grob_set (g, "elements", breakables);
          for (vsize j = 0; j < breakables.size (); j++)
            /* if we encompass a separation-item that spans multiple staves,
               we filter out the grobs that don't belong to our staff */
            if (me->common_refpoint (breakables[j], Y_AXIS)
                  == me->get_y_parent ()
                && scm_is_eq (get_property (breakables[j], "avoid-slur"),
                              ly_symbol2scm ("inside")))
              new_encompasses.push_back (breakables[j]);
        }
      else
        new_encompasses.push_back (g);
    }

  if (Grob_array *a = unsmob<Grob_array> (get_object (me, "encompass-objects")))
    a->set_array (new_encompasses);
}

Bezier
Slur::get_curve (Grob *me)
{
  return from_scm<Bezier> (get_property (me, "control-points"));
}

void
Slur::add_column (Spanner *me, Grob *n)
{
  Pointer_group_interface::add_grob (me, ly_symbol2scm ("note-columns"), n);
  add_bound_item (me, n);
}

void
Slur::add_extra_encompass (Spanner *me, Grob *n)
{
  Pointer_group_interface::add_grob (me, ly_symbol2scm ("encompass-objects"),
                                     n);
}

MAKE_SCHEME_CALLBACK_WITH_OPTARGS (Slur, pure_outside_slur_callback,
                                   "ly:slur::pure-outside-slur-callback", 4, 1,
                                   "");
SCM
Slur::pure_outside_slur_callback (SCM grob, SCM start_scm, SCM end_scm,
                                  SCM offset_scm)
{
  int start = from_scm (start_scm, 0);
  int end = from_scm (end_scm, 0);
  auto *const script = LY_ASSERT_SMOB (Grob, grob, 1);
  Grob *slur = unsmob<Grob> (get_object (script, "slur"));
  if (!slur)
    return offset_scm;

  SCM avoid = get_property (script, "avoid-slur");
  if (!scm_is_eq (avoid, ly_symbol2scm ("outside"))
      && !scm_is_eq (avoid, ly_symbol2scm ("around")))
    return offset_scm;

  Real offset = from_scm<double> (offset_scm, 0.0);
  Direction dir = get_grob_direction (script);
  return to_scm (offset
                 + dir * slur->pure_y_extent (slur, start, end).length () / 4);
}

MAKE_SCHEME_CALLBACK_WITH_OPTARGS (Slur, outside_slur_callback,
                                   "ly:slur::outside-slur-callback", 2, 1, "");
SCM
Slur::outside_slur_callback (SCM grob, SCM offset_scm)
{
  auto *const script = LY_ASSERT_SMOB (Grob, grob, 1);
  Grob *slur = unsmob<Grob> (get_object (script, "slur"));

  if (!slur)
    return offset_scm;

  SCM avoid = get_property (script, "avoid-slur");
  if (!scm_is_eq (avoid, ly_symbol2scm ("outside"))
      && !scm_is_eq (avoid, ly_symbol2scm ("around")))
    return offset_scm;

  Direction dir = get_grob_direction (script);
  if (dir == CENTER)
    return offset_scm;

  Grob *cx = script->common_refpoint (slur, X_AXIS);
  Grob *cy = script->common_refpoint (slur, Y_AXIS);

  Bezier curve = Slur::get_curve (slur);

  curve.translate (Offset (slur->relative_coordinate (cx, X_AXIS),
                           slur->relative_coordinate (cy, Y_AXIS)));

  Interval yext = robust_relative_extent (script, cy, Y_AXIS);
  Interval xext = robust_relative_extent (script, cx, X_AXIS);
  Interval slur_wid (curve.control_[0][X_AXIS], curve.control_[3][X_AXIS]);

  /*
    cannot use is_empty because some 0-extent scripts
    come up with TabStaffs.
  */
  if (xext.length () <= 0 || yext.length () <= 0)
    return offset_scm;

  bool contains = false;
  for (const auto d : {LEFT, RIGHT})
    contains |= slur_wid.contains (xext[d]);

  if (!contains)
    return offset_scm;

  Real offset = from_scm<double> (offset_scm, 0);
  yext.translate (offset);

  /* FIXME: slur property, script property?  */
  Real slur_padding
    = from_scm<double> (get_property (script, "slur-padding"), 0.0);
  yext.widen (slur_padding);

  Interval exts[] = {xext, yext};
  bool do_shift = false;
  Real EPS = 1.0e-5;
  if (scm_is_eq (avoid, ly_symbol2scm ("outside")))
    {
      for (const auto d : {LEFT, RIGHT})
        {
          Real x = minmax (
            -d, xext[d], curve.control_[d == LEFT ? 0 : 3][X_AXIS] + -d * EPS);
          Real y = curve.get_other_coordinate (X_AXIS, x);
          do_shift = y == minmax (dir, yext[-dir], y);
          if (do_shift)
            break;
        }
    }
  else
    {
      for (const auto a : {X_AXIS, Y_AXIS})
        {
          for (const auto d : {LEFT, RIGHT})
            {
              vector<Real> coords = curve.get_other_coordinates (a, exts[a][d]);
              for (vsize i = 0; i < coords.size (); i++)
                {
                  do_shift = exts[other_axis (a)].contains (coords[i]);
                  if (do_shift)
                    break;
                }
              if (do_shift)
                break;
            }
          if (do_shift)
            break;
        }
    }

  Real avoidance_offset
    = do_shift
        ? curve.minmax (
            X_AXIS, std::max (xext[LEFT], curve.control_[0][X_AXIS] + EPS),
            std::min (xext[RIGHT], curve.control_[3][X_AXIS] - EPS), dir)
            - yext[-dir]
        : 0.0;

  return to_scm (offset + avoidance_offset);
}

/*
 * Used by Slur_engraver:: and Phrasing_slur_engraver::
 */
void
Slur::auxiliary_acknowledge_extra_object (Grob *e, vector<Spanner *> &slurs,
                                          vector<Spanner *> &end_slurs)
{
  if (slurs.empty () && end_slurs.empty ())
    return;

  SCM avoid = get_property (e, "avoid-slur");
  Spanner *slur;
  if (end_slurs.size () && !slurs.size ())
    slur = end_slurs[0];
  else
    slur = slurs[0];

  if (has_interface<Tie> (e) || scm_is_eq (avoid, ly_symbol2scm ("inside")))
    {
      for (vsize i = slurs.size (); i--;)
        add_extra_encompass (slurs[i], e);
      for (vsize i = end_slurs.size (); i--;)
        add_extra_encompass (end_slurs[i], e);
      if (slur)
        set_object (e, "slur", slur->self_scm ());
    }
  else if (scm_is_eq (avoid, ly_symbol2scm ("outside"))
           || scm_is_eq (avoid, ly_symbol2scm ("around")))
    {
      if (slur)
        {
          chain_offset_callback (
            e,
            Unpure_pure_container::make_smob (outside_slur_callback_proc,
                                              pure_outside_slur_callback_proc),
            Y_AXIS);
          chain_callback (e, outside_slur_cross_staff_proc,
                          ly_symbol2scm ("cross-staff"));
          set_object (e, "slur", slur->self_scm ());
        }
    }
  else if (!scm_is_eq (avoid, ly_symbol2scm ("ignore")))
    e->warning (_f ("Ignoring grob for slur: %s.  avoid-slur not set?",
                    e->name ().c_str ()));
}

/*
  A callback that will be chained together with the original cross-staff
  value of a grob that is placed 'outside or 'around a slur. This just says
  that any grob becomes cross-staff if it is placed 'outside or 'around a
  cross-staff slur.
*/
MAKE_SCHEME_CALLBACK (Slur, outside_slur_cross_staff,
                      "ly:slur::outside-slur-cross-staff", 2)
SCM
Slur::outside_slur_cross_staff (SCM smob, SCM previous)
{
  if (from_scm<bool> (previous))
    return previous;

  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  Grob *slur = unsmob<Grob> (get_object (me, "slur"));

  if (!slur)
    return SCM_BOOL_F;
  return get_property (slur, "cross-staff");
}

MAKE_SCHEME_CALLBACK (Slur, calc_cross_staff, "ly:slur::calc-cross-staff", 1)
SCM
Slur::calc_cross_staff (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);

  extract_grob_set (me, "note-columns", cols);
  extract_grob_set (me, "encompass-objects", extras);

  for (vsize i = 0; i < cols.size (); i++)
    {
      if (Grob *s = Note_column::get_stem (cols[i]))
        if (from_scm<bool> (get_property (s, "cross-staff")))
          return SCM_BOOL_T;
    }

  /* the separation items are dealt with in replace_breakable_encompass_objects
     so we can ignore them here */
  vector<Grob *> non_sep_extras;
  for (vsize i = 0; i < extras.size (); i++)
    if (!has_interface<Separation_item> (extras[i]))
      non_sep_extras.push_back (extras[i]);

  Grob *common = common_refpoint_of_array (cols, me, Y_AXIS);
  common = common_refpoint_of_array (non_sep_extras, common, Y_AXIS);

  return to_scm (common != me->get_y_parent ());
}

ADD_INTERFACE (Slur,
               R"(
A slur.  Slurs are formatted by trying a number of combinations of left/right
end point, and then picking the slur with the lowest demerit score.  The
combinations are generated by going from the base attachments (i.e., note
heads) in the direction in half space increments until we have covered
@code{region-size} staff spaces.  The following properties may be set in the
@code{details} list.

@table @code
@item region-size
Size of region (in staff spaces) for determining potential endpoints in the Y
direction.
@item head-encompass-penalty
Demerit to apply when note heads collide with a slur.
@item stem-encompass-penalty
Demerit to apply when stems collide with a slur.
@item edge-attraction-factor
Factor used to calculate the demerit for distances between slur endpoints and
their corresponding base attachments.
@item same-slope-penalty
Demerit for slurs with attachment points that are horizontally aligned.
@item steeper-slope-factor
Factor used to calculate demerit only if this slur is not broken.
@item non-horizontal-penalty
Demerit for slurs with attachment points that are not horizontally aligned.
@item max-slope
The maximum slope allowed for this slur.
@item max-slope-factor
Factor that calculates demerit based on the max slope.
@item free-head-distance
The amount of vertical free space that must exist between a slur and note
heads.
@item absolute-closeness-measure
Factor to calculate demerit for variance between a note head and slur.
@item extra-object-collision-penalty
Factor to calculate demerit for extra objects that the slur encompasses,
including accidentals, fingerings, and tuplet numbers.
@item accidental-collision
Factor to calculate demerit for @code{Accidental} objects that the slur
encompasses.  This property value replaces the value of
@code{extra-object-collision-penalty}.
@item extra-encompass-free-distance
The amount of vertical free space that must exist between a slur and various
objects it encompasses, including accidentals, fingerings, and tuplet numbers.
@item extra-encompass-collision-distance
This detail is currently unused.
@item head-slur-distance-factor
Factor to calculate demerit for variance between a note head and slur.
@item head-slur-distance-max-ratio
The maximum value for the ratio of distance between a note head and slur.
@item gap-to-staffline-inside
Minimum gap inside the curve of the slur where the slur is parallel to a
staffline.
@item gap-to-staffline-outside
Minimum gap outside the curve of the slur where the slur is parallel to a
staffline.
@item free-slur-distance
The amount of vertical free space that must exist between adjacent slurs.  This
subproperty only works for @code{PhrasingSlur}.
@item edge-slope-exponent
Factor used to calculate the demerit for the slope of a slur near its
endpoints; a larger value yields a larger demerit.
@end table
               )",

               /* properties */
               R"(
annotation
avoid-slur
control-points
dash-definition
details
direction
eccentricity
encompass-objects
height-limit
inspect-quants
line-thickness
note-columns
positions
ratio
thickness
               )");
