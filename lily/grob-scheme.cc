/*
  grob-scheme.cc -- Scheme entry points for the grob datatype

  source file of the GNU LilyPond music typesetter

  (c) 1998--2007 Jan Nieuwenhuizen <janneke@gnu.org>
  Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "grob.hh"

#include "warn.hh"		// error()
#include "item.hh"
#include "output-def.hh"
#include "system.hh"
#include "font-interface.hh"
#include "paper-score.hh"


LY_DEFINE (ly_grob_property_data, "ly:grob-property-data",
	   2, 0, 0, (SCM grob, SCM sym),
	   "Retrieve @var{sym} for @var{grob} but don't process callbacks.")
{
  Grob *sc = unsmob_grob (grob);
  SCM_ASSERT_TYPE (sc, grob, SCM_ARG1, __FUNCTION__, "grob");
  SCM_ASSERT_TYPE (scm_is_symbol (sym), sym, SCM_ARG2, __FUNCTION__, "symbol");

  return sc->get_property_data (sym);
}

LY_DEFINE (ly_grob_set_property_x, "ly:grob-set-property!",
	   3, 0, 0, (SCM grob, SCM sym, SCM val),
	   "Set @var{sym} in grob @var{grob} to value @var{val}")
{
  Grob *sc = unsmob_grob (grob);
  SCM_ASSERT_TYPE (sc, grob, SCM_ARG1, __FUNCTION__, "grob");
  SCM_ASSERT_TYPE (scm_is_symbol (sym), sym, SCM_ARG2, __FUNCTION__, "symbol");

  if (!ly_is_procedure (val)
      && !type_check_assignment (sym, val, ly_symbol2scm ("backend-type?")))
    error ("typecheck failed");

  sc->set_property (sym, val);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_grob_property, "ly:grob-property",
	   2, 1, 0, (SCM grob, SCM sym, SCM deflt),
	   "Return the value of a value in grob @var{g} of property @var{sym}. "
	   "It will return @code{'()} or @var{deflt} (if specified) "
	   "if  @var{sym} is undefined in @var{g}."
	   "\n\n")
{
  Grob *sc = unsmob_grob (grob);
  SCM_ASSERT_TYPE (sc, grob, SCM_ARG1, __FUNCTION__, "grob");
  SCM_ASSERT_TYPE (scm_is_symbol (sym), sym, SCM_ARG2, __FUNCTION__, "symbol");
  if (deflt == SCM_UNDEFINED)
    deflt = SCM_EOL;

  SCM retval = sc->internal_get_property (sym);
  if (retval == SCM_EOL)
    retval = deflt;
  
  return retval;
}


LY_DEFINE (ly_grob_interfaces, "ly:grob-interfaces",
	   1, 0, 0, (SCM grob),
	   "Return the interfaces list of grob @var{grob}.")
{
  Grob *sc = unsmob_grob (grob);
  SCM_ASSERT_TYPE (sc, grob, SCM_ARG1, __FUNCTION__, "grob");

  return sc->interfaces ();
}

LY_DEFINE (ly_grob_object, "ly:grob-object",
	   2, 0, 0, (SCM grob, SCM sym),
	   "Return the value of a pointer in grob @var{g} of property @var{sym}. "
	   "It will return @code{' ()} (end-of-list) "
	   "if  @var{sym} is undefined in @var{g}."
	   "\n\n")
{
  Grob *sc = unsmob_grob (grob);
  SCM_ASSERT_TYPE (sc, grob, SCM_ARG1, __FUNCTION__, "grob");
  SCM_ASSERT_TYPE (scm_is_symbol (sym), sym, SCM_ARG2, __FUNCTION__, "symbol");

  return sc->internal_get_object (sym);
}

LY_DEFINE (ly_spanner_get_bound, "ly:spanner-bound",
	   2, 0, 0, (SCM slur, SCM dir),
	   "Get one of the bounds of @var{spanner}. @var{dir} is @code{-1} "
	   "for left, and @code{1} for right.")
{
  Spanner *sl = dynamic_cast<Spanner *> (unsmob_grob (slur));
  SCM_ASSERT_TYPE (sl, slur, SCM_ARG1, __FUNCTION__, "spanner grob");
  SCM_ASSERT_TYPE (is_direction (dir), slur, SCM_ARG2, __FUNCTION__, "dir");
  return sl->get_bound (to_dir (dir))->self_scm ();
}

/* TODO: make difference between scaled and unscalead variable in
   calling (i.e different funcs.) */
LY_DEFINE (ly_grob_layout, "ly:grob-layout",
	   1, 0, 0, (SCM g),
	   "Get @code{\\layout} definition from grob @var{g}.")
{
  Grob *sc = unsmob_grob (g);
  SCM_ASSERT_TYPE (sc, g, SCM_ARG1, __FUNCTION__, "grob");

  return sc->layout ()->self_scm ();
}

LY_DEFINE (ly_grob_alist_chain, "ly:grob-alist-chain",
	   1, 1, 0, (SCM grob, SCM global),
	   "Get an alist chain for grob @var{grob}, with @var{global} as the "
	   "global default. If unspecified, @code{font-defaults} "
	   "from the layout block is taken. ")
{
  Grob *sc = unsmob_grob (grob);
  SCM_ASSERT_TYPE (sc, grob, SCM_ARG1, __FUNCTION__, "grob");

  if (global == SCM_UNDEFINED)
    {
      global = sc->layout ()->lookup_variable (ly_symbol2scm ("font-defaults"));
      if (global == SCM_UNDEFINED)
	global = SCM_EOL;
    }

  return sc->get_property_alist_chain (global);
}

LY_DEFINE (ly_get_extent, "ly:grob-extent",
	   3, 0, 0, (SCM grob, SCM refp, SCM axis),
	   "Get the extent in @var{axis} direction of @var{grob} relative to "
	   "the grob @var{refp}")
{
  Grob *sc = unsmob_grob (grob);
  Grob *ref = unsmob_grob (refp);
  
  SCM_ASSERT_TYPE (sc, grob, SCM_ARG1, __FUNCTION__, "grob");
  SCM_ASSERT_TYPE (ref, refp, SCM_ARG2, __FUNCTION__, "grob");
  SCM_ASSERT_TYPE (is_axis (axis), axis, SCM_ARG3, __FUNCTION__, "axis");

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
	   "Get the extent in @var{axis} direction of @var{grob} relative to "
	   "the grob @var{refp}, or (0,0) if empty")
{
  Grob *sc = unsmob_grob (grob);
  Grob *ref = unsmob_grob (refp);
  
  SCM_ASSERT_TYPE (sc, grob, SCM_ARG1, __FUNCTION__, "grob");
  SCM_ASSERT_TYPE (ref, refp, SCM_ARG2, __FUNCTION__, "grob");
  SCM_ASSERT_TYPE (is_axis (axis), axis, SCM_ARG3, __FUNCTION__, "axis");

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
	   "Get the coordinate in @var{axis} direction of @var{grob} relative to "
	   "the grob @var{refp}")
{
  Grob *sc = unsmob_grob (grob);
  Grob *ref = unsmob_grob (refp);
  
  SCM_ASSERT_TYPE (sc, grob, SCM_ARG1, __FUNCTION__, "grob");
  SCM_ASSERT_TYPE (ref, refp, SCM_ARG2, __FUNCTION__, "grob");
  SCM_ASSERT_TYPE (is_axis (axis), axis, SCM_ARG3, __FUNCTION__, "axis");

  Axis a = Axis (scm_to_int (axis));

    
  if (ref->common_refpoint (sc, a) != ref)
    {
      // ugh. should use other error message
      SCM_ASSERT_TYPE (false, refp, SCM_ARG2, __FUNCTION__, "common refpoint");
    }

  return scm_from_double (sc->relative_coordinate (ref,a));
}


LY_DEFINE (ly_grob_parent, "ly:grob-parent",
	   2, 0, 0, (SCM grob, SCM axis),
	   "Get the parent of @var{grob}.  @var{axis} is 0 for the X-axis, "
	   "1 for the Y-axis.")
{
  Grob *sc = unsmob_grob (grob);
  SCM_ASSERT_TYPE (sc, grob, SCM_ARG1, __FUNCTION__, "grob");
  SCM_ASSERT_TYPE (is_axis (axis), axis, SCM_ARG2, __FUNCTION__, "axis");

  Grob *par = sc->get_parent (Axis (scm_to_int (axis)));
  return par ? par->self_scm () : SCM_EOL;
}

LY_DEFINE (ly_grob_properties, "ly:grob-properties",
	   1, 0, 0, (SCM grob),
	   "Get the mutable proprerties of @var{grob}.")
{
  Grob *g = unsmob_grob (grob);
  SCM_ASSERT_TYPE (g, grob, SCM_ARG1, __FUNCTION__, "grob");

  /* FIXME: uhg? copy/read only? */
  return g->mutable_property_alist_;
}

LY_DEFINE (ly_grob_basic_properties, "ly:grob-basic-properties",
	   1, 0, 0, (SCM grob),
	   "Get the immutable properties of @var{grob}.")
{
  Grob *g = unsmob_grob (grob);
  SCM_ASSERT_TYPE (g, grob, SCM_ARG1, __FUNCTION__, "grob");

  /* FIXME: uhg? copy/read only? */
  return g->immutable_property_alist_;
}

LY_DEFINE (ly_grob_system, "ly:grob-system",
	   1, 0, 0, (SCM g),
	   "Return the System Grob of @var{g}.")
{
  Grob *me = unsmob_grob (g);
  SCM_ASSERT_TYPE (me, g, SCM_ARG1, __FUNCTION__, "grob");

  if (System *g = me->get_system ())
    return g->self_scm ();

  return SCM_EOL;
}

LY_DEFINE (ly_grob_original, "ly:grob-original",
	   1, 0, 0, (SCM grob),
	   "Return the unbroken original Grob of @var{grob}.")
{
  Grob *me = unsmob_grob (grob);
  SCM_ASSERT_TYPE (me, grob, SCM_ARG1, __FUNCTION__, "grob");
  return me->original () ? me->original ()->self_scm () : me->self_scm ();
}

/* TODO: maybe we should return a vector -- random access is more
   logical for this list? */
LY_DEFINE (ly_spanner_broken_into, "ly:spanner-broken-into",
	   1, 0, 0, (SCM spanner),
	   "Return broken-into list for @var{spanner}.")
{
  Spanner *me = dynamic_cast<Spanner *> (unsmob_grob (spanner));
  SCM_ASSERT_TYPE (me, spanner, SCM_ARG1, __FUNCTION__, "spanner");

  SCM s = SCM_EOL;
  for (vsize i = me->broken_intos_.size (); i--;)
    s = scm_cons (me->broken_intos_[i]->self_scm (), s);
  return s;
}

LY_DEFINE (ly_grob_suicide_x, "ly:grob-suicide!",
	   1, 0, 0, (SCM g),
	   "Kill @var{g}.")
{
  Grob *me = unsmob_grob (g);
  SCM_ASSERT_TYPE (me, g, SCM_ARG1, __FUNCTION__, "grob");

  me->suicide ();
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_grob_translate_axis_x, "ly:grob-translate-axis!",
	   3, 0, 0, (SCM g, SCM d, SCM a),
	   "Translate @var{g} on axis @var{a} over distance @var{d}.")
{
  Grob *me = unsmob_grob (g);
  SCM_ASSERT_TYPE (me, g, SCM_ARG1, __FUNCTION__, "grob");
  SCM_ASSERT_TYPE (scm_is_number (d), d, SCM_ARG2, __FUNCTION__, "dimension");
  SCM_ASSERT_TYPE (is_axis (a), a, SCM_ARG3, __FUNCTION__, "axis");

  me->translate_axis (scm_to_double (d), Axis (scm_to_int (a)));
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_spanner_p, "ly:spanner?",
	   1, 0, 0, (SCM g),
	   "Is  @var{g} a spanner object?")
{
  Grob *me = unsmob_grob (g);
  bool b = dynamic_cast<Spanner *> (me);

  return ly_bool2scm (b);
}

LY_DEFINE (ly_grob_key, "ly:grob-key",
	   1, 0, 0,
	   (SCM grob),
	   "Return the object-key for @var{grob}.")
{
  Grob *me = unsmob_grob (grob);
  SCM_ASSERT_TYPE (me, grob, SCM_ARG1, __FUNCTION__, "Grob");
  
  return me->key () ?  me->key ()->self_scm () : SCM_EOL;
}

LY_DEFINE (ly_grob_default_font, "ly:grob-default-font",
	   1, 0, 0, (SCM grob),
	   "Return the default font for grob @var{gr}.")
{
  Grob *gr = unsmob_grob (grob);
  SCM_ASSERT_TYPE (gr, grob, SCM_ARG1, __FUNCTION__, "grob");

  return Font_interface::get_default_font (gr)->self_scm ();
}


/*
  TODO: consider swapping order, so we can do

  (grob-common-refpoint a b c d e)
 */
LY_DEFINE (ly_grob_common_refpoint, "ly:grob-common-refpoint",
	   3, 0, 0,  (SCM grob, SCM other, SCM axis),
	   "Find the common refpoint of @var{grob} and @var{other} for @var{axis}."
	   )
{
  
  Grob *gr = unsmob_grob (grob);
  SCM_ASSERT_TYPE (gr, grob, SCM_ARG1, __FUNCTION__, "grob");

  Grob *o = unsmob_grob (other);
  SCM_ASSERT_TYPE (o, other, SCM_ARG2, __FUNCTION__, "grob");

  SCM_ASSERT_TYPE (is_axis (axis), axis, SCM_ARG3, __FUNCTION__, "axis");

  Grob *refp = gr->common_refpoint (o,  Axis (scm_to_int (axis)));
  return refp ? refp->self_scm () : SCM_BOOL_F;
}

LY_DEFINE (ly_grob_common_refpoint_of_array, "ly:grob-common-refpoint-of-array",
	   3, 0, 0,  (SCM grob, SCM others, SCM axis),
	   "Find the common refpoint of @var{grob} and @var{others} "
	   "(a grob-array) for @var{axis}."
	   )
{
  Grob *gr = unsmob_grob (grob);
  SCM_ASSERT_TYPE (gr, grob, SCM_ARG1, __FUNCTION__, "grob");

  Grob_array *ga = unsmob_grob_array (others);
  SCM_ASSERT_TYPE (ga, others, SCM_ARG2, __FUNCTION__, "grob array");
  SCM_ASSERT_TYPE (is_axis (axis), axis, SCM_ARG3, __FUNCTION__, "axis");

  Grob *refp = common_refpoint_of_array (ga->array (), gr, Axis (scm_to_int (axis)));
  return refp ? refp->self_scm () : SCM_BOOL_F;
}
