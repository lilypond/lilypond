/*
  slur.cc -- implement external interface for Slur

  source file of the GNU LilyPond music typesetter

  (c) 1996--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
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
#include "main.hh"		// DEBUG_SLUR_SCORING
#include "note-column.hh"
#include "output-def.hh"
#include "spanner.hh"
#include "staff-symbol-referencer.hh"
#include "stem.hh"
#include "text-interface.hh"
#include "tie.hh"
#include "warn.hh"
#include "slur-scoring.hh"
#include "separation-item.hh"
#include "international.hh"



MAKE_SCHEME_CALLBACK (Slur, calc_direction, 1)
SCM
Slur::calc_direction (SCM smob)
{
  Grob *me = unsmob_grob (smob);
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
  Grob *me = unsmob_grob (smob);
  int start = scm_to_int (start_scm);
  int end = scm_to_int (end_scm);
  Real height = robust_scm2double (me->get_property ("height-limit"), 2.0);

  extract_grob_set (me, "note-columns", encompasses);
  Interval ret;

  Grob *parent = me->get_parent (Y_AXIS);
  if (common_refpoint_of_array (encompasses, me, Y_AXIS) != parent)
    /* this could happen if, for example, we are a cross-staff slur.
       in this case, we want to be ignored */
    return ly_interval2scm (Interval ());

  for (vsize i = 0; i < encompasses.size (); i++)
    {
      Interval d = encompasses[i]->pure_height (parent, start, end);
      if (!d.is_empty ())
	ret.unite (d);
    }

  ret.widen (height * 0.5);
  return ly_interval2scm (ret);
}

MAKE_SCHEME_CALLBACK (Slur, height, 1);
SCM
Slur::height (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  // FIXME uncached
  Stencil *m = me->get_stencil ();
  return m ? ly_interval2scm (m->extent (Y_AXIS))
    : ly_interval2scm (Interval ());
}

MAKE_SCHEME_CALLBACK (Slur, print, 1);
SCM
Slur::print (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  extract_grob_set (me, "note-columns", encompasses);
  if (encompasses.empty ())
    {
      me->suicide ();
      return SCM_EOL;
    }

  Real staff_thick = Staff_symbol_referencer::line_thickness (me);
  Real base_thick = staff_thick
    * robust_scm2double (me->get_property ("thickness"), 1);
  Real line_thick = staff_thick
    * robust_scm2double (me->get_property ("line-thickness"), 1);

  Bezier one = get_curve (me);
  Stencil a;

  SCM dash_definition = me->get_property ("dash-definition");
  a = Lookup::slur (one,
                    get_grob_direction (me) * base_thick,
		    line_thick,
                    dash_definition);

#if DEBUG_SLUR_SCORING
  SCM annotation = me->get_property ("annotation");
  if (!scm_is_string (annotation))
    {
      SCM debug = me->layout ()->lookup_variable (ly_symbol2scm ("debug-slur-scoring"));
      if (to_boolean (debug))
	annotation = me->get_property ("quant-score");
    }
  
  if (scm_is_string (annotation))
    {
      string str;
      SCM properties = Font_interface::text_font_alist_chain (me);

      if (!scm_is_number (me->get_property ("font-size")))
	properties = scm_cons (scm_acons (ly_symbol2scm ("font-size"), scm_from_int (-6), SCM_EOL),
			     properties);
      
      Stencil tm = *unsmob_stencil (Text_interface::interpret_markup
				    (me->layout ()->self_scm (), properties,
				     annotation));
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
      
      if (Separation_item::has_interface (g))
	{
	  extract_grob_set (g, "elements", breakables);
	  for (vsize j = 0; j < breakables.size (); j++)
	    /* if we encompass a separation-item that spans multiple staves,
	       we filter out the grobs that don't belong to our staff */
	    if (me->common_refpoint (breakables[j], Y_AXIS) == me->get_parent (Y_AXIS)
		&& breakables[j]->get_property ("avoid-slur") == ly_symbol2scm ("inside"))
	      new_encompasses.push_back (breakables[j]);
	}
      else
	new_encompasses.push_back (g);
    }

  SCM encompass_scm = me->get_object ("encompass-objects");
  if (Grob_array::unsmob (encompass_scm))
    {
      vector<Grob *> &arr =
	unsmob_grob_array (encompass_scm)->array_reference ();
      arr = new_encompasses;
    }
}

Bezier
Slur::get_curve (Grob *me)
{
  Bezier b;
  int i = 0;
  for (SCM s = me->get_property ("control-points"); scm_is_pair (s);
       s = scm_cdr (s))
    b.control_[i++] = ly_scm2offset (scm_car (s));

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
  Pointer_group_interface::add_grob (me, ly_symbol2scm ("encompass-objects"), n);
}

MAKE_SCHEME_CALLBACK_WITH_OPTARGS (Slur, pure_outside_slur_callback, 4, 1, "");
SCM
Slur::pure_outside_slur_callback (SCM grob, SCM start_scm, SCM end_scm, SCM offset_scm)
{
  int start = robust_scm2int (start_scm, 0);
  int end = robust_scm2int (end_scm, 0);
  Grob *script = unsmob_grob (grob);
  Grob *slur = unsmob_grob (script->get_object ("slur"));
  if (!slur)
    return offset_scm;

  SCM avoid = script->get_property ("avoid-slur");
  if (avoid != ly_symbol2scm ("outside") && avoid != ly_symbol2scm ("around"))
    return offset_scm;

  Real offset = robust_scm2double (offset_scm, 0.0);
  Direction dir = get_grob_direction (script);
  return scm_from_double (offset + dir * slur->pure_height (slur, start, end).length () / 4);
}

MAKE_SCHEME_CALLBACK_WITH_OPTARGS (Slur, outside_slur_callback, 2, 1, "");
SCM
Slur::outside_slur_callback (SCM grob, SCM offset_scm)
{
  Grob *script = unsmob_grob (grob);
  Grob *slur = unsmob_grob (script->get_object ("slur")); 

  if (!slur)
    return offset_scm;

  SCM avoid = script->get_property ("avoid-slur");
  if (avoid != ly_symbol2scm ("outside")
      && avoid != ly_symbol2scm ("around"))
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

  Real offset = robust_scm2double (offset_scm, 0);
  yext.translate (offset);
  
  /* FIXME: slur property, script property?  */
  Real slur_padding = robust_scm2double (script->get_property ("slur-padding"),
					 0.0);
  yext.widen (slur_padding);

  const Real EPS = 1e-3;
  Interval bezext (curve.control_[0][X_AXIS], curve.control_[3][X_AXIS]);
  bool consider[] = {false, false, false};
  Real ys[] = {0, 0, 0};
  bool do_shift = false;
  
  for (int d = LEFT, k = 0; d <= RIGHT; d++, k++)
    {
      Real x = xext.linear_combination ((Direction) d);
      consider[k] = bezext.contains (x);

      if (consider[k])
	{
	  ys[k]
	    = (fabs (bezext[LEFT] - x) < EPS)
	    ? curve.control_[0][Y_AXIS]
	    : ((fabs (bezext[RIGHT] - x) < EPS)
	       ? curve.control_[3][Y_AXIS]
	       : curve.get_other_coordinate (X_AXIS, x));

	  /* Request shift if slur is contained script's Y, or if
	     script is inside slur and avoid == outside.  */
	  if (yext.contains (ys[k])
	      || (dir * ys[k] > dir * yext[-dir] && avoid == ly_symbol2scm ("outside")))
	    do_shift = true;
	}
    }

  Real avoidance_offset = 0.0;
  if (do_shift)
    {
      for (int d = LEFT, k = 0; d <= RIGHT; d++, k++)
	if (consider[k])
	  avoidance_offset = dir * (max (dir * avoidance_offset,
					 dir * (ys[k] - yext[-dir] + dir * slur_padding)));
    }
  return scm_from_double (offset + avoidance_offset);
}

/*
 * Used by Slur_engraver:: and Phrasing_slur_engraver::
 */
void
Slur::auxiliary_acknowledge_extra_object (Grob_info const &info,
					  vector<Grob*> &slurs,
					  vector<Grob*> &end_slurs)
{
  if (slurs.empty () && end_slurs.empty ())
    return;
  
  Grob *e = info.grob ();
  SCM avoid = e->get_property ("avoid-slur");
  if (Tie::has_interface (e)
      || avoid == ly_symbol2scm ("inside"))
    {
      for (vsize i = slurs.size (); i--;)
	add_extra_encompass (slurs[i], e);
      for (vsize i = end_slurs.size (); i--;)
	add_extra_encompass (end_slurs[i], e);
    }
  else if (avoid == ly_symbol2scm ("outside")
	   || avoid == ly_symbol2scm ("around"))
    {
      Grob *slur;
      if (end_slurs.size () && !slurs.size ())
	slur = end_slurs[0];
      else
	slur = slurs[0];

      if (slur)
	{
	  chain_offset_callback (e, outside_slur_callback_proc, Y_AXIS);
	  chain_callback (e, outside_slur_cross_staff_proc, ly_symbol2scm("cross-staff"));
	  e->set_object ("slur", slur->self_scm ());
	}
    }
  else if (avoid != ly_symbol2scm ("ignore"))
    e->warning (_f ("Ignoring grob for slur: %s. avoid-slur not set?",
		    e->name().c_str ()));
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
  if (previous == SCM_BOOL_T)
    return previous;

  Grob *me = unsmob_grob (smob);
  Grob *slur = unsmob_grob (me->get_object ("slur"));

  if (!slur)
    return SCM_BOOL_F;
  return slur->get_property ("cross-staff");
}

MAKE_SCHEME_CALLBACK (Slur, calc_cross_staff, 1)
SCM
Slur::calc_cross_staff (SCM smob)
{
  Grob *me = unsmob_grob (smob);

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
  vector<Grob*> non_sep_extras;
  for (vsize i = 0; i < extras.size (); i++)
    if (!Separation_item::has_interface (extras[i]))
      non_sep_extras.push_back (extras[i]);

  Grob *common = common_refpoint_of_array (cols, me, Y_AXIS);
  common = common_refpoint_of_array (non_sep_extras, common, Y_AXIS);

  return scm_from_bool (common != me->get_parent (Y_AXIS));
}

ADD_INTERFACE (Slur,
	       "A slur.",
	       
	       /* properties */
	       "annotation "
	       "avoid-slur " 	/* UGH. */
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
	       "quant-score "
	       "ratio "
	       "thickness "
	       );

