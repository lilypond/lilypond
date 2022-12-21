/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2022 Jan Nieuwenhuizen <janneke@gnu.org>
  Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "font-interface.hh"
#include "grob-array.hh"
#include "international.hh"
#include "item.hh"
#include "output-def.hh"
#include "paper-score.hh"
#include "system.hh"
#include "unpure-pure-container.hh"
#include "warn.hh" // error ()

LY_DEFINE (ly_grob_property_data, "ly:grob-property-data", 2, 0, 0,
           (SCM grob, SCM sym),
           R"(
Return the value for property @var{sym} of @var{grob}, but do not process
callbacks.
           )")
{
  auto *const sc = LY_ASSERT_SMOB (Grob, grob, 1);
  LY_ASSERT_TYPE (ly_is_symbol, sym, 2);

  return get_property_data (sc, sym);
}

LY_DEFINE (ly_grob_set_property_x, "ly:grob-set-property!", 3, 0, 0,
           (SCM grob, SCM sym, SCM val),
           R"(
Set @var{sym} in grob @var{grob} to value @var{val}.
           )")
{
  auto *const sc = LY_ASSERT_SMOB (Grob, grob, 1);
  LY_ASSERT_TYPE (ly_is_symbol, sym, 2);

  if (!ly_is_procedure (val)
      && !type_check_assignment (sym, val, ly_symbol2scm ("backend-type?")))
    error (_ ("typecheck failed"));

  set_property (sc, sym, val);
  return SCM_UNSPECIFIED;
}

LY_DEFINE_WITH_SETTER (ly_grob_property, "ly:grob-property",
                       ly_grob_set_property_x, 2, 1, 0,
                       (SCM grob, SCM sym, SCM val),
                       R"(
Return the value for property @var{sym} of @var{grob}.  If no value is found,
return @var{val} or @code{'()} if @var{val} is not specified.
                       )")
{
  auto *const sc = LY_ASSERT_SMOB (Grob, grob, 1);
  LY_ASSERT_TYPE (ly_is_symbol, sym, 2);
  if (SCM_UNBNDP (val))
    val = SCM_EOL;

  SCM retval = get_property (sc, sym);
  if (scm_is_null (retval))
    retval = val;

  return retval;
}

LY_DEFINE (ly_grob_set_nested_property_x, "ly:grob-set-nested-property!", 3, 0,
           0, (SCM grob, SCM symlist, SCM val),
           R"(
Set nested property @var{symlist} in grob @var{grob} to value @var{val}.
           )")
{
  auto *const sc = LY_ASSERT_SMOB (Grob, grob, 1);

  bool type_ok = scm_is_pair (symlist);

  if (type_ok)
    for (SCM s = symlist; scm_is_pair (s) && type_ok; s = scm_cdr (s))
      type_ok &= ly_is_symbol (scm_car (s));

  SCM_ASSERT_TYPE (type_ok, symlist, SCM_ARG2, __FUNCTION__, "list of symbols");

  if (scm_is_pair (scm_cdr (symlist)))
    set_nested_property (sc, symlist, val);
  else
    ly_grob_set_property_x (grob, scm_car (symlist), val);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_grob_pure_property, "ly:grob-pure-property", 4, 1, 0,
           (SCM grob, SCM sym, SCM beg, SCM end, SCM val),
           R"(
Return the pure value for property @var{sym} of @var{grob}.  If no value is
found, return @var{val} or @code{'()} if @var{val} is not specified.
           )")
{
  auto *const sc = LY_ASSERT_SMOB (Grob, grob, 1);
  LY_ASSERT_TYPE (ly_is_symbol, sym, 2);
  LY_ASSERT_TYPE (scm_is_integer, beg, 3);
  LY_ASSERT_TYPE (scm_is_integer, end, 4);
  if (SCM_UNBNDP (val))
    val = SCM_EOL;

  SCM retval = sc->internal_get_pure_property (sym, from_scm<int> (beg),
                                               from_scm<int> (end));
  if (scm_is_null (retval))
    retval = val;

  return retval;
}

LY_DEFINE (ly_grob_pure_height, "ly:grob-pure-height", 4, 1, 0,
           (SCM grob, SCM refp, SCM beg, SCM end, SCM val),
           R"(
Return the pure height of @var{grob} given refpoint @var{refp}.  If no value is
found, return @var{val} or @code{'()} if @var{val} is not specified.
           )")
{
  auto *const sc = LY_ASSERT_SMOB (Grob, grob, 1);
  auto *const ref = LY_ASSERT_SMOB (Grob, refp, 2);
  LY_ASSERT_TYPE (scm_is_integer, beg, 3);
  LY_ASSERT_TYPE (scm_is_integer, end, 4);
  if (SCM_UNBNDP (val))
    val = SCM_EOL;

  Interval retval
    = sc->pure_y_extent (ref, from_scm<int> (beg), from_scm<int> (end));

  return to_scm (retval);
}

LY_DEFINE (ly_grob_interfaces, "ly:grob-interfaces", 1, 0, 0, (SCM grob),
           R"(
Return the interfaces list of grob @var{grob}.
           )")
{
  auto *const sc = LY_ASSERT_SMOB (Grob, grob, 1);

  return sc->interfaces ();
}

LY_DEFINE (ly_grob_set_object_x, "ly:grob-set-object!", 3, 0, 0,
           (SCM grob, SCM sym, SCM val),
           R"(
Set @var{sym} in grob @var{grob} to value @var{val}.
           )")
{
  auto *const sc = LY_ASSERT_SMOB (Grob, grob, 1);
  LY_ASSERT_TYPE (ly_is_symbol, sym, 2);

  set_object (sc, sym, val);
  return SCM_UNSPECIFIED;
}

LY_DEFINE_WITH_SETTER (ly_grob_object, "ly:grob-object", ly_grob_set_object_x,
                       2, 1, 0, (SCM grob, SCM sym, SCM val),
                       R"(
Return the value of a pointer in grob @var{grob} of property @var{sym}.  When
@var{sym} is undefined in @var{grob}, it returns @var{val} if specified or
@code{'()} (end-of-list) otherwise.  The kind of properties this taps into
differs from regular properties.  It is used to store links between grobs,
either grobs or grob arrays.  For instance, a note head has a @code{stem}
property, the stem grob it belongs to.  Just after line breaking, all those
grobs are scanned and replaced by their relevant broken versions when
applicable.
                       )")
{
  auto *const sc = LY_ASSERT_SMOB (Grob, grob, 1);
  LY_ASSERT_TYPE (ly_is_symbol, sym, 2);

  SCM object = sc->internal_get_object (sym);
  if (scm_is_null (object))
    return SCM_UNBNDP (val) ? SCM_EOL : val;
  return object;
}

/* TODO: make difference between scaled and unscaled variable in
   calling (i.e different funcs.) */
LY_DEFINE (ly_grob_layout, "ly:grob-layout", 1, 0, 0, (SCM grob),
           R"(
Get @code{\layout} definition from grob @var{grob}.
           )")
{
  auto *const sc = LY_ASSERT_SMOB (Grob, grob, 1);

  return sc->layout ()->self_scm ();
}

LY_DEFINE (ly_grob_alist_chain, "ly:grob-alist-chain", 1, 1, 0,
           (SCM grob, SCM global),
           R"(
Get an alist chain for grob @var{grob}, with @var{global} as the global
default.  If unspecified, @code{font-defaults} from the layout block is taken.
           )")
{
  auto *const sc = LY_ASSERT_SMOB (Grob, grob, 1);

  if (SCM_UNBNDP (global))
    {
      global = sc->layout ()->lookup_variable (ly_symbol2scm ("font-defaults"));
      if (SCM_UNBNDP (global))
        global = SCM_EOL;
    }

  return sc->get_property_alist_chain (global);
}

// FIXME: the C++ functions should do these checks too (cf. #6149).
static void
check_refp (Grob *grob, Grob *refp, Axis a, const char *function)
{
  if (!grob->has_in_ancestry (refp, a))
    {
      scm_misc_error (function,
                      "grob ~a is an invalid refpoint for ~a on ~a axis.\n"
                      "The refpoint should be an ancestor (direct or indirect) of the grob.",
                      ly_list (ly_string2scm (grob->name ()),
                               ly_string2scm (refp->name ()),
                               ly_string2scm (a == X_AXIS ? "X" : "Y")));

    }
}

LY_DEFINE (ly_grob_extent, "ly:grob-extent", 3, 0, 0,
           (SCM grob, SCM refp, SCM axis),
           R"(
Get the extent in @var{axis} direction of @var{grob} relative to the grob
@var{refp}.
           )")
{
  auto *const sc = LY_ASSERT_SMOB (Grob, grob, 1);
  auto *const ref = LY_ASSERT_SMOB (Grob, refp, 2);
  LY_ASSERT_TYPE (is_scm<Axis>, axis, 3);

  const auto a = from_scm<Axis> (axis);

  check_refp (sc, ref, a, "ly:grob-extent");

  return to_scm (sc->extent (ref, a));
}

LY_DEFINE (ly_grob_robust_relative_extent, "ly:grob-robust-relative-extent", 3,
           0, 0, (SCM grob, SCM refp, SCM axis),
           R"(
Get the extent in @var{axis} direction of @var{grob} relative to the grob
@var{refp}, or @code{(0,0)} if empty.
           )")
{
  auto *const sc = LY_ASSERT_SMOB (Grob, grob, 1);
  auto *const ref = LY_ASSERT_SMOB (Grob, refp, 2);
  LY_ASSERT_TYPE (is_scm<Axis>, axis, 3);

  const auto a = from_scm<Axis> (axis);

  check_refp (sc, ref, a, "ly:grob-robust-relative-extent");

  return to_scm (robust_relative_extent (sc, ref, a));
}

LY_DEFINE (ly_grob_relative_coordinate, "ly:grob-relative-coordinate", 3, 0, 0,
           (SCM grob, SCM refp, SCM axis),
           R"(
Get the coordinate in @var{axis} direction of @var{grob} relative to the grob
@var{refp}.
           )")
{
  auto *const sc = LY_ASSERT_SMOB (Grob, grob, 1);
  auto *const ref = LY_ASSERT_SMOB (Grob, refp, 2);
  LY_ASSERT_TYPE (is_scm<Axis>, axis, 3);

  const auto a = from_scm<Axis> (axis);

  check_refp (sc, ref, a, "ly:grob-relative-coordinate");

  return to_scm (sc->relative_coordinate (ref, a));
}

LY_DEFINE (ly_grob_set_parent_x, "ly:grob-set-parent!", 3, 0, 0,
           (SCM grob, SCM axis, SCM parent_grob),
           R"(
Set @var{parent-grob} as the parent of grob @var{grob} in axis @var{axis}.
           )")
{
  auto *const gr = LY_ASSERT_SMOB (Grob, grob, 1);
  LY_ASSERT_TYPE (is_scm<Axis>, axis, 2);
  auto *const parent = LY_ASSERT_SMOB (Grob, parent_grob, 3);

  gr->set_parent (parent, from_scm<Axis> (axis));
  return SCM_UNSPECIFIED;
}

LY_DEFINE_WITH_SETTER (ly_grob_parent, "ly:grob-parent", ly_grob_set_parent_x,
                       2, 1, 0, (SCM grob, SCM axis, SCM def),
                       R"(
Get the parent of @var{grob}.  @var{axis} is @code{0} for the x@tie{}axis,
@code{1}@tie{}for the y@tie{}axis.  If @var{grob} has no parent on this
axis (yet), return @var{def}, or @code{'()} if @var{def} is not specified.
                       )")
{
  auto *const sc = LY_ASSERT_SMOB (Grob, grob, 1);
  LY_ASSERT_TYPE (is_scm<Axis>, axis, 2);

  Grob *par = sc->get_parent (from_scm<Axis> (axis));
  if (par)
    return par->self_scm ();
  if (SCM_UNBNDP (def))
    return SCM_EOL;
  return def;
}

LY_DEFINE (ly_grob_basic_properties, "ly:grob-basic-properties", 1, 0, 0,
           (SCM grob),
           R"(
Get the immutable properties of @var{grob}.
           )")
{
  auto *const g = LY_ASSERT_SMOB (Grob, grob, 1);

  /* FIXME: uhg? copy/read only? */
  return g->immutable_property_alist_;
}

LY_DEFINE (ly_grob_system, "ly:grob-system", 1, 0, 0, (SCM grob),
           R"(
Return the system grob of @var{grob}.
           )")
{
  auto *const me = LY_ASSERT_SMOB (Grob, grob, 1);

  if (System *g = me->get_system ())
    return g->self_scm ();

  return SCM_EOL;
}

LY_DEFINE (ly_grob_original, "ly:grob-original", 1, 0, 0, (SCM grob),
           R"(
Return the unbroken original grob of @var{grob}, @var{grob} may be an item or
spanner.
           )")
{
  auto *const me = LY_ASSERT_SMOB (Grob, grob, 1);
  return me->original () ? me->original ()->self_scm () : me->self_scm ();
}

LY_DEFINE (ly_grob_suicide_x, "ly:grob-suicide!", 1, 0, 0, (SCM grob),
           R"(
Kill @var{grob}.
           )")
{
  auto *const me = LY_ASSERT_SMOB (Grob, grob, 1);

  me->suicide ();
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_grob_translate_axis_x, "ly:grob-translate-axis!", 3, 0, 0,
           (SCM grob, SCM d, SCM a),
           R"(
Translate @var{grob} on axis@tie{}@var{a} over distance@tie{}@var{d}.
           )")
{
  auto *const me = LY_ASSERT_SMOB (Grob, grob, 1);
  LY_ASSERT_TYPE (scm_is_number, d, 2);
  LY_ASSERT_TYPE (is_scm<Axis>, a, 3);

  me->translate_axis (from_scm<double> (d), from_scm<Axis> (a));
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_grob_default_font, "ly:grob-default-font", 1, 0, 0, (SCM grob),
           R"(
Return the default font for grob @var{grob}.
           )")
{
  auto *const gr = LY_ASSERT_SMOB (Grob, grob, 1);

  return Font_interface::get_default_font (gr)->self_scm ();
}

/*
  TODO: consider swapping order, so we can do

  (grob-common-refpoint a b c d e)
 */
LY_DEFINE (ly_grob_common_refpoint, "ly:grob-common-refpoint", 3, 0, 0,
           (SCM grob, SCM other, SCM axis),
           R"(
Find the common refpoint of @var{grob} and @var{other} for @var{axis}.
           )")
{

  auto *const gr = LY_ASSERT_SMOB (Grob, grob, 1);
  auto *const o = LY_ASSERT_SMOB (Grob, other, 2);

  LY_ASSERT_TYPE (is_scm<Axis>, axis, 3);

  Grob *refp = gr->common_refpoint (o, from_scm<Axis> (axis));
  return refp ? refp->self_scm () : SCM_BOOL_F;
}

LY_DEFINE (ly_grob_common_refpoint_of_array, "ly:grob-common-refpoint-of-array",
           3, 0, 0, (SCM grob, SCM others, SCM axis),
           R"(
Find the common refpoint of @var{grob} and @var{others} (a grob-array) for
@var{axis}.
           )")
{
  auto *const gr = LY_ASSERT_SMOB (Grob, grob, 1);
  auto *const ga = LY_ASSERT_SMOB (Grob_array, others, 2);
  LY_ASSERT_TYPE (is_scm<Axis>, axis, 3);

  const auto a = from_scm<Axis> (axis);
  Grob *refp = common_refpoint_of_array (ga->array (), gr, a);
  return refp ? refp->self_scm () : SCM_BOOL_F;
}

LY_DEFINE (ly_grob_chain_callback, "ly:grob-chain-callback", 3, 0, 0,
           (SCM grob, SCM proc, SCM sym),
           R"(
Find the callback that is stored as property @var{sym} of grob @var{grob} and
chain @var{proc} to the head of this, meaning that it is called using
@var{grob} and the previous callback's result.
           )")
{
  auto *const gr = LY_ASSERT_SMOB (Grob, grob, 1);
  SCM_ASSERT_TYPE (
    ly_is_procedure (proc) || unsmob<Unpure_pure_container> (proc), proc,
    SCM_ARG2, __FUNCTION__, "procedure or unpure pure container");
  LY_ASSERT_TYPE (ly_is_symbol, sym, 3);

  chain_callback (gr, proc, sym);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_grob_vertical_less_p, "ly:grob-vertical<?", 2, 0, 0,
           (SCM a, SCM b),
           R"(
Does @var{a} lie above @var{b} on the page?
           )")
{
  auto *const ga = LY_ASSERT_SMOB (Grob, a, 1);
  auto *const gb = LY_ASSERT_SMOB (Grob, b, 2);

  return to_scm (Grob::vertical_less (ga, gb));
}

LY_DEFINE (ly_grob_get_vertical_axis_group_index,
           "ly:grob-get-vertical-axis-group-index", 1, 0, 0, (SCM grob),
           R"(
Get the index of the vertical axis group the grob @var{grob} belongs to; return
@code{-1} if none is found.
           )")
{
  auto *const gr = LY_ASSERT_SMOB (Grob, grob, 1);

  return to_scm (Grob::get_vertical_axis_group_index (gr));
}

LY_DEFINE (ly_grob_spanned_column_rank_interval,
           "ly:grob-spanned-column-rank-interval", 1, 0, 0, (SCM grob),
           R"(
Return a pair with the @code{rank} of the furthest left column and the
@code{rank} of the furthest right column spanned by @code{grob}.
           )")
{
  auto *const gr = LY_ASSERT_SMOB (Grob, grob, 1);

  Interval_t<int> iv = gr->spanned_column_rank_interval ();

  return scm_cons (to_scm (iv[LEFT]), to_scm (iv[RIGHT]));
}
