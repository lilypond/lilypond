/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>
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
#include "beam.hh"
#include "bezier.hh"
#include "directional-element-interface.hh"
#include "font-interface.hh"
#include "grob-array.hh"
#include "grob-info.hh"
#include "international.hh"
#include "item.hh"
#include "lookup.hh"
#include "main.hh" // DEBUG_SLUR_SCORING
#include "note-column.hh"
#include "output-def.hh"
#include "pointer-group-interface.hh"
#include "separation-item.hh"
#include "skyline-pair.hh"
#include "slur-scoring.hh"
#include "spanner.hh"
#include "staff-symbol-referencer.hh"
#include "stem.hh"
#include "text-interface.hh"
#include "tie.hh"
#include "unpure-pure-container.hh"
#include "warn.hh"

using std::string;
using std::vector;

MAKE_SCHEME_CALLBACK (Slur, calc_direction, 1)
SCM
Slur::calc_direction (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);
  extract_grob_set (me, "note-columns", encompasses);

  if (encompasses.empty ())
    {
      me->suicide ();
      return SCM_BOOL_F;
    }

  Direction d = DOWN;
  for (vsize i = 0; i < encompasses.size (); i++)
    {
      if (Note_column::dir (encompasses[i]) < 0)
        {
          d = UP;
          break;
        }
    }
  return scm_from_int (d);
}

MAKE_SCHEME_CALLBACK (Slur, pure_height, 3);
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
  Grob *me = unsmob<Grob> (smob);
  int start = scm_to_int (start_scm);
  int end = scm_to_int (end_scm);
  Direction dir = get_grob_direction (me);

  extract_grob_set (me, "note-columns", encompasses);
  Interval ret;
  ret.set_empty ();

  Grob *parent = me->get_parent (Y_AXIS);
  Drul_array<Real> extremal_heights (infinity_f, -infinity_f);
  if (common_refpoint_of_array (encompasses, me, Y_AXIS) != parent)
    /* this could happen if, for example, we are a cross-staff slur.
       in this case, we want to be ignored */
    return ly_interval2scm (Interval ());

  for (vsize i = 0; i < encompasses.size (); i++)
    {
      Interval d = encompasses[i]->pure_y_extent (parent, start, end);
      if (!d.is_empty ())
        {
          for (DOWN_and_UP (downup))
            ret.add_point (d[dir]);

          if (extremal_heights[LEFT] == infinity_f)
            extremal_heights[LEFT] = d[dir];
          extremal_heights[RIGHT] = d[dir];
        }
    }

  if (ret.is_empty ())
    return ly_interval2scm (Interval ());

  Interval extremal_span;
  extremal_span.set_empty ();
  for (LEFT_and_RIGHT (d))
    extremal_span.add_point (extremal_heights[d]);
  ret[-dir] = minmax (dir, extremal_span[-dir], ret[-dir]);

  /*
    The +0.5 comes from the fact that we try to place a slur
    0.5 staff spaces from the note-head.
    (see Slur_score_state.get_base_attachments ())
  */
  ret += 0.5 * dir;
  return ly_interval2scm (ret);
}

MAKE_SCHEME_CALLBACK (Slur, height, 1);
SCM
Slur::height (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);

  // FIXME uncached
  Stencil *m = me->get_stencil ();
  return m ? ly_interval2scm (m->extent (Y_AXIS))
           : ly_interval2scm (Interval ());
}

MAKE_SCHEME_CALLBACK (Slur, print, 1);
SCM
Slur::print (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);
  extract_grob_set (me, "note-columns", encompasses);
  if (encompasses.empty ())
    {
      me->suicide ();
      return SCM_EOL;
    }

  Real staff_thick = Staff_symbol_referencer::line_thickness (me);
  Real base_thick
      = staff_thick * robust_scm2double (me->get_property ("thickness"), 1);
  Real line_thick
      = staff_thick
        * robust_scm2double (me->get_property ("line-thickness"), 1);

  Bezier one = get_curve (me);
  Stencil a;

  SCM dash_definition = me->get_property ("dash-definition");
  a = Lookup::slur (one, get_grob_direction (me) * base_thick, line_thick,
                    dash_definition);

#if DEBUG_SLUR_SCORING
  SCM annotation = me->get_property ("annotation");
  if (scm_is_string (annotation))
    {
      string str;
      SCM properties = Font_interface::text_font_alist_chain (me);

      if (!scm_is_number (me->get_property ("font-size")))
        properties = scm_cons (
            scm_acons (ly_symbol2scm ("font-size"), scm_from_int (-6), SCM_EOL),
            properties);

      Stencil tm = *unsmob<Stencil> (Text_interface::interpret_markup (
          me->layout ()->self_scm (), properties, annotation));
      a.add_at_edge (Y_AXIS, get_grob_direction (me), tm, 1.0);
    }
#endif

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
                    == me->get_parent (Y_AXIS)
                && scm_is_eq (breakables[j]->get_property ("avoid-slur"),
                              ly_symbol2scm ("inside")))
              new_encompasses.push_back (breakables[j]);
        }
      else
        new_encompasses.push_back (g);
    }

  if (Grob_array *a = unsmob<Grob_array> (me->get_object ("encompass-objects")))
    a->set_array (new_encompasses);
}

Bezier
Slur::get_curve (Grob *me)
{
  Bezier b;
  SCM s = me->get_property ("control-points");
  for (int i = 0; i < Bezier::CONTROL_COUNT; i++)
    {
      if (scm_is_pair (s))
        {
          b.control_[i] = ly_scm2offset (scm_car (s));
          s = scm_cdr (s);
        }
      else
        b.control_[i] = Offset (0.0, 0.0);
    }

  return b;
}

void
Slur::add_column (Grob *me, Grob *n)
{
  Pointer_group_interface::add_grob (me, ly_symbol2scm ("note-columns"), n);
  add_bound_item (dynamic_cast<Spanner *> (me), n);
}

void
Slur::add_extra_encompass (Grob *me, Grob *n)
{
  Pointer_group_interface::add_grob (me, ly_symbol2scm ("encompass-objects"),
                                     n);
}

MAKE_SCHEME_CALLBACK_WITH_OPTARGS (Slur, pure_outside_slur_callback, 4, 1, "");
SCM
Slur::pure_outside_slur_callback (SCM grob, SCM start_scm, SCM end_scm,
                                  SCM offset_scm)
{
  int start = robust_scm2int (start_scm, 0);
  int end = robust_scm2int (end_scm, 0);
  Grob *script = unsmob<Grob> (grob);
  Grob *slur = unsmob<Grob> (script->get_object ("slur"));
  if (!slur)
    return offset_scm;

  SCM avoid = script->get_property ("avoid-slur");
  if (!scm_is_eq (avoid, ly_symbol2scm ("outside"))
      && !scm_is_eq (avoid, ly_symbol2scm ("around")))
    return offset_scm;

  Real offset = robust_scm2double (offset_scm, 0.0);
  Direction dir = get_grob_direction (script);
  return scm_from_double (
      offset + dir * slur->pure_y_extent (slur, start, end).length () / 4);
}

MAKE_SCHEME_CALLBACK_WITH_OPTARGS (Slur, outside_slur_callback, 2, 1, "");
SCM
Slur::outside_slur_callback (SCM grob, SCM offset_scm)
{
  Grob *script = unsmob<Grob> (grob);
  Grob *slur = unsmob<Grob> (script->get_object ("slur"));

  if (!slur)
    return offset_scm;

  SCM avoid = script->get_property ("avoid-slur");
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
  for (LEFT_and_RIGHT (d))
    contains |= slur_wid.contains (xext[d]);

  if (!contains)
    return offset_scm;

  Real offset = robust_scm2double (offset_scm, 0);
  yext.translate (offset);

  /* FIXME: slur property, script property?  */
  Real slur_padding
      = robust_scm2double (script->get_property ("slur-padding"), 0.0);
  yext.widen (slur_padding);

  Interval exts[] = {xext, yext};
  bool do_shift = false;
  Real EPS = 1.0e-5;
  if (scm_is_eq (avoid, ly_symbol2scm ("outside")))
    {
      for (LEFT_and_RIGHT (d))
        {
          Real x
              = minmax (-d, xext[d],
                        curve.control_[d == LEFT ? 0 : 3][X_AXIS] + -d * EPS);
          Real y = curve.get_other_coordinate (X_AXIS, x);
          do_shift = y == minmax (dir, yext[-dir], y);
          if (do_shift)
            break;
        }
    }
  else
    {
      for (int a = X_AXIS; a < NO_AXES; a++)
        {
          for (LEFT_and_RIGHT (d))
            {
              vector<Real> coords
                  = curve.get_other_coordinates (Axis (a), exts[a][d]);
              for (vsize i = 0; i < coords.size (); i++)
                {
                  do_shift = exts[(a + 1) % NO_AXES].contains (coords[i]);
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
                  X_AXIS,
                  std::max (xext[LEFT], curve.control_[0][X_AXIS] + EPS),
                  std::min (xext[RIGHT], curve.control_[3][X_AXIS] - EPS), dir)
                  - yext[-dir]
            : 0.0;

  return scm_from_double (offset + avoidance_offset);
}

MAKE_SCHEME_CALLBACK (Slur, vertical_skylines, 1);
SCM
Slur::vertical_skylines (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);
  vector<Box> boxes;

  if (!me)
    return Skyline_pair (boxes, X_AXIS).smobbed_copy ();

  Bezier curve = Slur::get_curve (me);
  vsize box_count
      = robust_scm2vsize (me->get_property ("skyline-quantizing"), 10);
  for (vsize i = 0; i < box_count; i++)
    {
      Box b;
      b.add_point (curve.curve_point (i * 1.0 / box_count));
      b.add_point (curve.curve_point ((i + 1) * 1.0 / box_count));
      boxes.push_back (b);
    }

  return Skyline_pair (boxes, X_AXIS).smobbed_copy ();
}

/*
 * Used by Slur_engraver:: and Phrasing_slur_engraver::
 */
void
Slur::auxiliary_acknowledge_extra_object (Grob_info const &info,
                                          vector<Grob *> &slurs,
                                          vector<Grob *> &end_slurs)
{
  if (slurs.empty () && end_slurs.empty ())
    return;

  Grob *e = info.grob ();
  SCM avoid = e->get_property ("avoid-slur");
  Grob *slur;
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
        e->set_object ("slur", slur->self_scm ());
    }
  else if (scm_is_eq (avoid, ly_symbol2scm ("outside"))
           || scm_is_eq (avoid, ly_symbol2scm ("around")))
    {
      if (slur)
        {
          chain_offset_callback (
              e,
              Unpure_pure_container::make_smob (
                  outside_slur_callback_proc, pure_outside_slur_callback_proc),
              Y_AXIS);
          chain_callback (e, outside_slur_cross_staff_proc,
                          ly_symbol2scm ("cross-staff"));
          e->set_object ("slur", slur->self_scm ());
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
MAKE_SCHEME_CALLBACK (Slur, outside_slur_cross_staff, 2)
SCM
Slur::outside_slur_cross_staff (SCM smob, SCM previous)
{
  if (to_boolean (previous))
    return previous;

  Grob *me = unsmob<Grob> (smob);
  Grob *slur = unsmob<Grob> (me->get_object ("slur"));

  if (!slur)
    return SCM_BOOL_F;
  return slur->get_property ("cross-staff");
}

MAKE_SCHEME_CALLBACK (Slur, calc_cross_staff, 1)
SCM
Slur::calc_cross_staff (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);

  extract_grob_set (me, "note-columns", cols);
  extract_grob_set (me, "encompass-objects", extras);

  for (vsize i = 0; i < cols.size (); i++)
    {
      if (Grob *s = Note_column::get_stem (cols[i]))
        if (to_boolean (s->get_property ("cross-staff")))
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

  return scm_from_bool (common != me->get_parent (Y_AXIS));
}

ADD_INTERFACE (Slur,
               "A slur."
               "\n"
               "The following properties may be set in the @code{details}"
               " list.\n"
               "\n"
               "@table @code\n"
               "@item region-size\n"
               "Size of region (in staff spaces) for determining"
               " potential endpoints in the Y direction.\n"
               "@item head-encompass-penalty\n"
               "Demerit to apply when note heads collide with a slur.\n"
               "@item stem-encompass-penalty\n"
               "Demerit to apply when stems collide with a slur.\n"
               "@item edge-attraction-factor\n"
               "Factor used to calculate the demerit for distances"
               " between slur endpoints and their corresponding base"
               " attachments.\n"
               "@item same-slope-penalty\n"
               "Demerit for slurs with attachment points that are"
               " horizontally aligned.\n"
               "@item steeper-slope-factor\n"
               "Factor used to calculate demerit only if this slur is"
               " not broken.\n"
               "@item non-horizontal-penalty\n"
               "Demerit for slurs with attachment points that are not"
               " horizontally aligned.\n"
               "@item max-slope\n"
               "The maximum slope allowed for this slur.\n"
               "@item max-slope-factor\n"
               "Factor that calculates demerit based on the max slope.\n"
               "@item free-head-distance\n"
               "The amount of vertical free space that must exist"
               " between a slur and note heads.\n"
               "@item absolute-closeness-measure\n"
               "Factor to calculate demerit for variance between a note"
               " head and slur.\n"
               "@item extra-object-collision-penalty\n"
               "Factor to calculate demerit for extra objects that the"
               " slur encompasses, including accidentals, fingerings, and"
               " tuplet numbers.\n"
               "@item accidental-collision\n"
               "Factor to calculate demerit for @code{Accidental} objects"
               " that the slur encompasses.  This property value replaces"
               " the value of @code{extra-object-collision-penalty}.\n"
               "@item extra-encompass-free-distance\n"
               "The amount of vertical free space that must exist"
               " between a slur and various objects it encompasses,"
               " including accidentals, fingerings, and tuplet numbers.\n"
               "@item extra-encompass-collision-distance\n"
               "This detail is currently unused.\n"
               "@item head-slur-distance-factor\n"
               "Factor to calculate demerit for variance between a note"
               " head and slur.\n"
               "@item head-slur-distance-max-ratio\n"
               "The maximum value for the ratio of distance between a"
               " note head and slur.\n"
               "@item gap-to-staffline-inside\n"
               "Minimum gap inside the curve of the slur"
               " where the slur is parallel to a staffline.\n"
               "@item gap-to-staffline-outside\n"
               "Minimum gap outside the curve of the slur"
               " where the slur is parallel to a staffline.\n"
               "@item free-slur-distance\n"
               "The amount of vertical free space that must exist"
               " between adjacent slurs.  This subproperty only works"
               " for @code{PhrasingSlur}.\n"
               "@item edge-slope-exponent\n"
               "Factor used to calculate the demerit for the slope of"
               " a slur near its endpoints; a larger value yields a"
               " larger demerit.\n"
               "@end table\n",

               /* properties */
               "annotation "
               "avoid-slur " /* UGH. */
               "control-points "
               "dash-definition "
               "details "
               "direction "
               "eccentricity "
               "encompass-objects "
               "height-limit "
               "inspect-quants "
               "inspect-index "
               "line-thickness "
               "note-columns "
               "positions "
               "ratio "
               "thickness ");
