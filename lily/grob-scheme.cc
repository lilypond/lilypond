/*
  grob-scheme.cc -- Scheme entry points for the grob datatype

  source file of the GNU LilyPond music typesetter

  (c) 1998--2004 Jan Nieuwenhuizen <janneke@gnu.org>
                 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "grob.hh"
#include "warn.hh"
#include "spanner.hh"
#include "item.hh"
#include "output-def.hh"
#include "system.hh"


LY_DEFINE (ly_grob_set_property_x, "ly:grob-set-property!",
	   3, 0, 0, (SCM grob, SCM sym, SCM val),
	   "Set @var{sym} in grob @var{grob} to value @var{val}")
{
  Grob *sc = unsmob_grob (grob);
  SCM_ASSERT_TYPE (sc, grob, SCM_ARG1, __FUNCTION__, "grob");
  SCM_ASSERT_TYPE (ly_c_symbol_p (sym), sym, SCM_ARG2, __FUNCTION__, "symbol");

  if (!type_check_assignment (sym, val, ly_symbol2scm ("backend-type?")))
    error ("typecheck failed");

  sc->internal_set_property (sym, val);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_grob_property, "ly:grob-property",
	   2, 0, 0, (SCM grob, SCM sym),
	  "Return the value of a value in grob @var{g} of property @var{sym}. "
	   "It will return @code{' ()} (end-of-list) "
	   "if  @var{sym} is undefined in @var{g}."
	   "\n\n"
	   )
{
  Grob *sc = unsmob_grob (grob);
  SCM_ASSERT_TYPE (sc, grob, SCM_ARG1, __FUNCTION__, "grob");
  SCM_ASSERT_TYPE (ly_c_symbol_p (sym), sym, SCM_ARG2, __FUNCTION__, "symbol");

  return sc->internal_get_property (sym);
}

LY_DEFINE (ly_spanner_get_bound, "ly:spanner-get-bound",
	   2, 0, 0, (SCM slur, SCM dir),
	   "Get one of the bounds of @var{spanner}. @var{dir} is @code{-1} "
	   "for left, and @code{1} for right.")
{
  Spanner * sl = dynamic_cast<Spanner*> (unsmob_grob (slur));
  SCM_ASSERT_TYPE (sl, slur, SCM_ARG1, __FUNCTION__, "spanner grob");
  SCM_ASSERT_TYPE (is_direction (dir), slur, SCM_ARG2, __FUNCTION__, "dir");
  return sl->get_bound (to_dir (dir))->self_scm ();
}

/* TODO: make difference between scaled and unscalead variable in
   calling (i.e different funcs.) */
LY_DEFINE (ly_grob_paper, "ly:grob-paper",
	   1, 0, 0, (SCM g),
	   "Get @code{\\paper} definition from grob @var{g}.")
{
  Grob * sc = unsmob_grob (g);
  SCM_ASSERT_TYPE (sc, g, SCM_ARG1, __FUNCTION__, "grob");

  return sc->get_paper ()->self_scm ();
}

LY_DEFINE (ly_grob_alist_chain, "ly:grob-alist-chain",
	   1, 1, 0, (SCM g, SCM global),
	   "Get an alist chain for grob @var{g}, with @var{global} as the "
	   "global default. If unspecified, @code{font-defaults} "
	   "from the paper block is taken. ")
{
  Grob *sc = unsmob_grob (g);
  SCM_ASSERT_TYPE (sc, g, SCM_ARG1, __FUNCTION__, "grob");

  if (global == SCM_UNDEFINED)
    global
      = sc->get_paper ()->lookup_variable (ly_symbol2scm ("font-defaults"));

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

  return ly_interval2scm ( sc->extent (ref, Axis (ly_scm2int (axis))));
}

LY_DEFINE (ly_grob_parent, "ly:grob-parent",
	   2, 0, 0, (SCM grob, SCM axis),
	   "Get the parent of @var{grob}.  @var{axis} is 0 for the X-axis, "
	   "1 for the Y-axis.")
{
  Grob *sc = unsmob_grob (grob);
  SCM_ASSERT_TYPE (sc, grob, SCM_ARG1, __FUNCTION__, "grob");
  SCM_ASSERT_TYPE (is_axis (axis), axis, SCM_ARG2, __FUNCTION__, "axis");

  Grob *par = sc->get_parent (Axis (ly_scm2int (axis)));
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
  return me->original_ ? me->original_->self_scm () : me->self_scm ();
}

/* TODO: maybe we should return a vector -- random access is more
   logical for this list? */
LY_DEFINE (ly_spanner_broken_into, "ly:spanner-broken-into",
	   1, 0, 0, (SCM spanner),
	   "Return broken-into list for @var{spanner}.")
{
  Spanner *me = dynamic_cast<Spanner*> (unsmob_grob (spanner));
  SCM_ASSERT_TYPE (me, spanner, SCM_ARG1, __FUNCTION__, "spanner");

  SCM s = SCM_EOL;
  for (int i = me->broken_intos_.size (); i--;)
    s = scm_cons (me->broken_intos_[i]->self_scm (), s);
  return s;
}

LY_DEFINE (ly_grob_suicide, "ly:grob-suicide",
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
  SCM_ASSERT_TYPE (ly_c_number_p (d), d, SCM_ARG2, __FUNCTION__, "dimension");
  SCM_ASSERT_TYPE (is_axis (a), a, SCM_ARG3, __FUNCTION__, "axis");

  me->translate_axis (ly_scm2double (d), Axis (ly_scm2int (a)));
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_spanner_p, "ly:spanner?",
	   1, 0, 0, (SCM g),
	   "Is  @var{g} a spanner object?")
{
  Grob *me = unsmob_grob (g);
  bool b = dynamic_cast<Spanner*> (me);

  return ly_bool2scm (b);
}

LY_DEFINE (ly_item_p, "ly:item?",
	   1, 0, 0, (SCM g),
	   "Is @var{g} an @code{Item} object?")
{
  Grob *me = unsmob_grob (g);
  bool b = dynamic_cast<Item*> (me);
  return ly_bool2scm (b);
}

LY_DEFINE (ly_item_break_dir, "ly:item-break-dir",
	   1, 0, 0, (SCM it),
	   "The break status dir of item @var{it}. @code{-1} is end of "
	   "line, @code{0} unbroken, and @code{1} begin of line.")
{
  Item *me = dynamic_cast<Item*> (unsmob_grob (it));
  SCM_ASSERT_TYPE (me, it, SCM_ARG1, __FUNCTION__, "Item");
  return scm_int2num (me->break_status_dir ());
}

