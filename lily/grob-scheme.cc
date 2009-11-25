/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2009 Jan Nieuwenhuizen <janneke@gnu.org>
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

#include "warn.hh"		// error ()
#include "item.hh"
#include "output-def.hh"
#include "system.hh"
#include "font-interface.hh"
#include "paper-score.hh"
#include "grob-array.hh"

LY_DEFINE (ly_grob_property_data, "ly:grob-property-data",
	   2, 0, 0, (SCM grob, SCM sym),
	   "Return the value for property @var{sym} of @var{grob},"
	   " but do not process callbacks.")
{
  Grob *sc = unsmob_grob (grob);

  LY_ASSERT_SMOB (Grob, grob, 1);
  LY_ASSERT_TYPE (ly_is_symbol, sym, 2);

  return sc->get_property_data (sym);
}

LY_DEFINE (ly_grob_set_property_x, "ly:grob-set-property!",
	   3, 0, 0, (SCM grob, SCM sym, SCM val),
	   "Set @var{sym} in grob @var{grob} to value @var{val}.")
{
  Grob *sc = unsmob_grob (grob);
 
  LY_ASSERT_SMOB (Grob, grob, 1);
  LY_ASSERT_TYPE (ly_is_symbol, sym, 2);

  if (!ly_is_procedure (val)
      && !type_check_assignment (sym, val, ly_symbol2scm ("backend-type?")))
    error ("typecheck failed");

  sc->set_property (sym, val);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_grob_property, "ly:grob-property",
	   2, 1, 0, (SCM grob, SCM sym, SCM val),
	   "Return the value for property @var{sym} of @var{grob}."
	   "  If no value is found, return @var{val} or @code{'()}"
	   " if @var{val} is not specified.")
{
  Grob *sc = unsmob_grob (grob);

  LY_ASSERT_SMOB (Grob, grob, 1);
  LY_ASSERT_TYPE (ly_is_symbol, sym, 2);
  if (val == SCM_UNDEFINED)
    val = SCM_EOL;

  SCM retval = sc->internal_get_property (sym);
  if (retval == SCM_EOL)
    retval = val;
  
  return retval;
}


LY_DEFINE (ly_grob_interfaces, "ly:grob-interfaces",
	   1, 0, 0, (SCM grob),
	   "Return the interfaces list of grob @var{grob}.")
{
  Grob *sc = unsmob_grob (grob);
   
  LY_ASSERT_SMOB (Grob, grob, 1);

  return sc->interfaces ();
}

LY_DEFINE (ly_grob_object, "ly:grob-object",
	   2, 0, 0, (SCM grob, SCM sym),
	   "Return the value of a pointer in grob@tie{}@var{g} of property"
	   " @var{sym}.  It returns @code{'()} (end-of-list) if @var{sym}"
	   " is undefined in@tie{}@var{g}.")
{
  Grob *sc = unsmob_grob (grob);
   
  LY_ASSERT_SMOB (Grob, grob, 1);
  LY_ASSERT_TYPE (ly_is_symbol, sym, 2);

  return sc->internal_get_object (sym);
}



/* TODO: make difference between scaled and unscalead variable in
   calling (i.e different funcs.) */
LY_DEFINE (ly_grob_layout, "ly:grob-layout",
	   1, 0, 0, (SCM grob),
	   "Get @code{\\layout} definition from grob @var{grob}.")
{
  Grob *sc = unsmob_grob (grob);
   
  LY_ASSERT_SMOB (Grob, grob, 1);

  return sc->layout ()->self_scm ();
}

LY_DEFINE (ly_grob_alist_chain, "ly:grob-alist-chain",
	   1, 1, 0, (SCM grob, SCM global),
	   "Get an alist chain for grob @var{grob}, with @var{global} as"
	   " the global default.  If unspecified, @code{font-defaults}"
	   " from the layout block is taken.")
{
  Grob *sc = unsmob_grob (grob);
   
  LY_ASSERT_SMOB (Grob, grob, 1);

  if (global == SCM_UNDEFINED)
    {
      global = sc->layout ()->lookup_variable (ly_symbol2scm ("font-defaults"));
      if (global == SCM_UNDEFINED)
	global = SCM_EOL;
    }

  return sc->get_property_alist_chain (global);
}

LY_DEFINE (ly_grob_extent, "ly:grob-extent",
	   3, 0, 0, (SCM grob, SCM refp, SCM axis),
	   "Get the extent in @var{axis} direction of @var{grob} relative to"
	   " the grob @var{refp}.")
{
  Grob *sc = unsmob_grob (grob);
  Grob *ref = unsmob_grob (refp);
  
   
  LY_ASSERT_SMOB (Grob, grob, 1);
  LY_ASSERT_SMOB (Grob, refp, 2);
  LY_ASSERT_TYPE (is_axis, axis, 3);

  Axis a = Axis (scm_to_int (axis));

    
  if (ref->common_refpoint (sc, a) != ref)
    {
      // ugh. should use other error message
      SCM_ASSERT_TYPE (false, refp, SCM_ARG2, __FUNCTION__, "common refpoint");
    }
  return ly_interval2scm (sc->extent (ref, a));
}

LY_DEFINE (ly_grob_robust_relative_extent, "ly:grob-robust-relative-extent",
	   3, 0, 0, (SCM grob, SCM refp, SCM axis),
	   "Get the extent in @var{axis} direction of @var{grob} relative to"
	   " the grob @var{refp}, or @code{(0,0)} if empty.")
{
  Grob *sc = unsmob_grob (grob);
  Grob *ref = unsmob_grob (refp);
  
   
  LY_ASSERT_SMOB (Grob, grob, 1);
  LY_ASSERT_SMOB (Grob, refp, 2);
  LY_ASSERT_TYPE (is_axis, axis, 3);

  Axis a = Axis (scm_to_int (axis));
    
  if (ref->common_refpoint (sc, a) != ref)
    {
      // ugh. should use other error message
      SCM_ASSERT_TYPE (false, refp, SCM_ARG2, __FUNCTION__, "common refpoint");
    }

  return ly_interval2scm (robust_relative_extent (sc, ref, a));
}

LY_DEFINE (ly_grob_relative_coordinate, "ly:grob-relative-coordinate",
	   3, 0, 0, (SCM grob, SCM refp, SCM axis),
	   "Get the coordinate in @var{axis} direction of @var{grob} relative"
	   " to the grob @var{refp}.")
{
  Grob *sc = unsmob_grob (grob);
  Grob *ref = unsmob_grob (refp);
  
   
  LY_ASSERT_SMOB (Grob, grob, 1);
  LY_ASSERT_SMOB (Grob, refp, 2);
  LY_ASSERT_TYPE (is_axis, axis, 3);

  Axis a = Axis (scm_to_int (axis));

    
  if (ref->common_refpoint (sc, a) != ref)
    {
      // ugh. should use other error message
      SCM_ASSERT_TYPE (false, refp, SCM_ARG2, __FUNCTION__, "common refpoint");
    }

  return scm_from_double (sc->relative_coordinate (ref, a));
}


LY_DEFINE (ly_grob_parent, "ly:grob-parent",
	   2, 0, 0, (SCM grob, SCM axis),
	   "Get the parent of @var{grob}.  @var{axis} is 0 for the X-axis,"
	   " 1@tie{}for the Y-axis.")
{
  Grob *sc = unsmob_grob (grob);
   
  LY_ASSERT_SMOB (Grob, grob, 1);
  LY_ASSERT_TYPE (is_axis, axis, 2);

  Grob *par = sc->get_parent (Axis (scm_to_int (axis)));
  return par ? par->self_scm () : SCM_EOL;
}

LY_DEFINE (ly_grob_properties, "ly:grob-properties",
	   1, 0, 0, (SCM grob),
	   "Get the mutable properties of @var{grob}.")
{
  Grob *g = unsmob_grob (grob);
   
  LY_ASSERT_SMOB (Grob, grob, 1);

  /* FIXME: uhg? copy/read only? */
  return g->mutable_property_alist_;
}

LY_DEFINE (ly_grob_basic_properties, "ly:grob-basic-properties",
	   1, 0, 0, (SCM grob),
	   "Get the immutable properties of @var{grob}.")
{
  Grob *g = unsmob_grob (grob);
   
  LY_ASSERT_SMOB (Grob, grob, 1);

  /* FIXME: uhg? copy/read only? */
  return g->immutable_property_alist_;
}

LY_DEFINE (ly_grob_system, "ly:grob-system",
	   1, 0, 0, (SCM grob),
	   "Return the system grob of @var{grob}.")
{
  Grob *me = unsmob_grob (grob);
   
  LY_ASSERT_SMOB (Grob, grob, 1);

  if (System *g = me->get_system ())
    return g->self_scm ();

  return SCM_EOL;
}

LY_DEFINE (ly_grob_original, "ly:grob-original",
	   1, 0, 0, (SCM grob),
	   "Return the unbroken original grob of @var{grob}.")
{
  Grob *me = unsmob_grob (grob);
   
  LY_ASSERT_SMOB (Grob, grob, 1);
  return me->original () ? me->original ()->self_scm () : me->self_scm ();
}


LY_DEFINE (ly_grob_suicide_x, "ly:grob-suicide!",
	   1, 0, 0, (SCM grob),
	   "Kill @var{grob}.")
{
  Grob *me = unsmob_grob (grob);
   
  LY_ASSERT_SMOB (Grob, grob, 1);

  me->suicide ();
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_grob_translate_axis_x, "ly:grob-translate-axis!",
	   3, 0, 0, (SCM grob, SCM d, SCM a),
	   "Translate @var{g} on axis@tie{}@var{a} over"
	   " distance@tie{}@var{d}.")
{
  Grob *me = unsmob_grob (grob);
   
  LY_ASSERT_SMOB (Grob, grob, 1);
  LY_ASSERT_TYPE (scm_is_number, d, 2);
  LY_ASSERT_TYPE (is_axis, a, 3);

  me->translate_axis (scm_to_double (d), Axis (scm_to_int (a)));
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_grob_default_font, "ly:grob-default-font",
	   1, 0, 0, (SCM grob),
	   "Return the default font for grob @var{gr}.")
{
  Grob *gr = unsmob_grob (grob);
   
  LY_ASSERT_SMOB (Grob, grob, 1);

  return Font_interface::get_default_font (gr)->self_scm ();
}


/*
  TODO: consider swapping order, so we can do

  (grob-common-refpoint a b c d e)
 */
LY_DEFINE (ly_grob_common_refpoint, "ly:grob-common-refpoint",
	   3, 0, 0,  (SCM grob, SCM other, SCM axis),
	   "Find the common refpoint of @var{grob} and @var{other}"
	   " for @var{axis}.")
{
  
  Grob *gr = unsmob_grob (grob);
   
  LY_ASSERT_SMOB (Grob, grob, 1);
  LY_ASSERT_SMOB (Grob, other, 2);

  Grob *o = unsmob_grob (other);

  LY_ASSERT_TYPE (is_axis, axis, 3);

  Grob *refp = gr->common_refpoint (o,  Axis (scm_to_int (axis)));
  return refp ? refp->self_scm () : SCM_BOOL_F;
}

LY_DEFINE (ly_grob_common_refpoint_of_array, "ly:grob-common-refpoint-of-array",
	   3, 0, 0,  (SCM grob, SCM others, SCM axis),
	   "Find the common refpoint of @var{grob} and @var{others}"
	   " (a grob-array) for @var{axis}.")
{
  Grob *gr = unsmob_grob (grob);
   
  LY_ASSERT_SMOB (Grob, grob, 1);
  LY_ASSERT_SMOB (Grob_array, others, 2);

  Grob_array *ga = unsmob_grob_array (others);
  LY_ASSERT_TYPE (is_axis, axis, 3);

  Grob *refp = common_refpoint_of_array (ga->array (), gr, Axis (scm_to_int (axis)));
  return refp ? refp->self_scm () : SCM_BOOL_F;
}
