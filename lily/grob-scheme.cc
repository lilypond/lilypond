#include "grob.hh"
#include "warn.hh"
#include "spanner.hh"
#include "item.hh"
#include "paper-def.hh"

LY_DEFINE(ly_set_grob_property,"ly-set-grob-property!", 3, 0, 0,
  (SCM grob, SCM sym, SCM val),
  "Set @var{sym} in grob @var{grob} to value @var{val}")
{
  Grob * sc = unsmob_grob (grob);
  SCM_ASSERT_TYPE(sc, grob, SCM_ARG1, __FUNCTION__, "grob");
  SCM_ASSERT_TYPE(gh_symbol_p (sym), sym, SCM_ARG2, __FUNCTION__, "symbol");  

  if (!type_check_assignment (sym, val, ly_symbol2scm ("backend-type?")))
    error ("typecheck failed");
      
  sc->internal_set_grob_property (sym, val);
  return SCM_UNSPECIFIED;
}

LY_DEFINE(ly_get_grob_property,
	  "ly-get-grob-property", 2, 0, 0, (SCM grob, SCM sym),
	  "Get the value of a value in grob @var{g} of property @var{sym}. It
will return @code{'()} (end-of-list) if @var{g} doesn't have @var{sym} set.

Grob properties are stored as GUILE association lists, with symbols as
keys. All lookup functions identify undefined properties with
end-of-list (i.e. @code{'()} in Scheme or @code{SCM_EOL} in C)

Properties are stored in two ways:
@itemize @bullet
@item mutable properties.
Grob properties that change from object to object. The storage of
these are private to a grob. For example pointers to other grobs are
always stored in the mutable properties.

@item immutable properties.
Grob properties that are shared across different grobs of the same
type. The storage is shared, and hence it is read-only. Typically, this
is used to store function callbacks, and default settings. They are
initially read from @file{scm/grob-description.scm}.
@end itemize

")
{
  Grob * sc = unsmob_grob (grob);
  SCM_ASSERT_TYPE(sc, grob, SCM_ARG1, __FUNCTION__, "grob");
  SCM_ASSERT_TYPE(gh_symbol_p (sym), sym, SCM_ARG2, __FUNCTION__, "symbol");  

  return sc->internal_get_grob_property (sym);
}

LY_DEFINE(spanner_get_bound, "ly-get-spanner-bound", 2 , 0, 0,
	  (SCM slur, SCM dir),
	  "Get one of the bounds of @var{spanner}. @var{dir} may be @code{-1} for
left, and @code{1} for right.
")
{
  Spanner * sl = dynamic_cast<Spanner*> (unsmob_grob (slur));
  SCM_ASSERT_TYPE(sl, slur, SCM_ARG1, __FUNCTION__, "spanner grob");
  SCM_ASSERT_TYPE(ly_dir_p (dir), slur, SCM_ARG2, __FUNCTION__, "dir");
  return sl->get_bound (to_dir (dir))->self_scm ();
}

LY_DEFINE(ly_get_paper_var,"ly-get-paper-variable", 2, 0, 0,
  (SCM grob, SCM sym),
  "Get a variable from the \\paper block.")
{
  Grob * sc = unsmob_grob (grob);
  SCM_ASSERT_TYPE(sc, grob, SCM_ARG1, __FUNCTION__, "grob");
  SCM_ASSERT_TYPE(gh_symbol_p (sym), sym, SCM_ARG2, __FUNCTION__, "symbol");  

  return sc->get_paper () ->get_scmvar_scm (sym);
}



LY_DEFINE(ly_get_extent, "ly-get-extent", 3, 0, 0,
	  (SCM grob, SCM refp, SCM axis),
	  "Get the extent in @var{axis} direction of @var{grob} relative to the
grob @var{refp}")
{
  Grob * sc = unsmob_grob (grob);
  Grob * ref = unsmob_grob (refp);
  SCM_ASSERT_TYPE(sc, grob, SCM_ARG1, __FUNCTION__, "grob");
  SCM_ASSERT_TYPE(ref, refp, SCM_ARG2, __FUNCTION__, "grob");
  
  SCM_ASSERT_TYPE(ly_axis_p (axis), axis, SCM_ARG3, __FUNCTION__, "axis");

  return ly_interval2scm ( sc->extent (ref, Axis (gh_scm2int (axis))));
}

LY_DEFINE (ly_get_parent,   "ly-get-parent", 2, 0, 0, (SCM grob, SCM axis),
	   "Get the parent of @var{grob}.  @var{axis} can be 0 for the X-axis, 1
for the Y-axis.")
{
  Grob * sc = unsmob_grob (grob);
  SCM_ASSERT_TYPE(sc, grob, SCM_ARG1, __FUNCTION__, "grob");
  SCM_ASSERT_TYPE(ly_axis_p (axis), axis, SCM_ARG2, __FUNCTION__, "axis");

  Grob * par = sc->get_parent (Axis (gh_scm2int (axis)));
  return par ? par->self_scm() : SCM_EOL;
}

/* ly prefix? */
LY_DEFINE (get_system,
	   "get-system",
	   1, 0, 0, (SCM grob),
	   "
Return the System Grob of @var{grob}.
")
{
  Grob *me = unsmob_grob (grob);
  SCM_ASSERT_TYPE (me, grob, SCM_ARG1, __FUNCTION__, "grob");
  
  if (Grob *g = me->get_system ())
    return g->self_scm ();
    
  return SCM_EOL;
}

/* ly prefix? */
LY_DEFINE (get_original,
	   "get-original",
	   1, 0, 0, (SCM grob),
	   "
Return the original Grob of @var{grob}
")
{
  Grob *me = unsmob_grob (grob);
  SCM_ASSERT_TYPE (me, grob, SCM_ARG1, __FUNCTION__, "grob");
  return me->original_ ? me->original_->self_scm () : me->self_scm ();
}


/* ly prefix? spanner in name? */
LY_DEFINE (get_broken_into,
	  "get-broken-into", 1, 0, 0, (SCM spanner),
	   "
Return broken-into list for @var{spanner}.
"
)
{
  ///  Spanner *me = unsmob_spanner (spanner);
  Spanner *me = dynamic_cast<Spanner*> (unsmob_grob (spanner));
  SCM_ASSERT_TYPE (me, spanner, SCM_ARG1, __FUNCTION__, "spanner");

  SCM s = SCM_EOL;
  for (int i = me->broken_intos_.size (); i; i--)
    s = gh_cons (me->broken_intos_[i-1]->self_scm (), s);
  return s;
}

