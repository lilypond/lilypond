/*
  slur.cc -- implement external interface for Slur

  source file of the GNU LilyPond music typesetter

  (c) 1996--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/


#include <math.h>

#include "beam.hh"
#include "bezier.hh"
#include "directional-element-interface.hh"
#include "font-interface.hh"
#include "group-interface.hh"
#include "lily-guile.hh"
#include "lookup.hh"
#include "main.hh"		// DEBUG_SLUR_SCORING
#include "note-column.hh"
#include "output-def.hh"
#include "rod.hh"
#include "slur.hh"
#include "spanner.hh"
#include "staff-symbol-referencer.hh"
#include "staff-symbol.hh"
#include "stem.hh"
#include "stencil.hh"
#include "text-item.hh"
#include "warn.hh"
#include "slur-scoring.hh"

MAKE_SCHEME_CALLBACK (Slur, height, 2);
SCM
Slur::height (SCM smob, SCM ax)
{
  Axis a = (Axis)scm_to_int (ax);
  Grob *me = unsmob_grob (smob);
  assert (a == Y_AXIS);

  SCM mol = me->get_uncached_stencil ();
  Interval ext;
  if (Stencil *m = unsmob_stencil (mol))
    ext = m->extent (a);
  return ly_interval2scm (ext);
}

/*
  Ugh should have dash-length + dash-period
*/
MAKE_SCHEME_CALLBACK (Slur, print,1);
SCM
Slur::print (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  if (!scm_ilength (me->get_property ("note-columns")))
    {
      me->suicide ();
      return SCM_EOL;
    }

  Real base_thick = robust_scm2double (me->get_property ("thickness"), 1);
  Real thick = base_thick * Staff_symbol_referencer::line_thickness (me);

  Real ss = Staff_symbol_referencer::staff_space (me);
  Bezier one = get_curve (me);

  Stencil a;

  /*
    TODO: replace dashed with generic property.
  */
  SCM p =  me->get_property ("dash-period");
  SCM f =  me->get_property ("dash-fraction");
  if (scm_is_number (p) && scm_is_number (f))
    a = Lookup::dashed_slur (one, thick, robust_scm2double (p, 1.0), 
            robust_scm2double(f,0));
  else
    a = Lookup::slur (one, get_grob_direction (me) * base_thick * ss / 10.0,
		      thick);

#if DEBUG_SLUR_SCORING
  SCM quant_score = me->get_property ("quant-score");

  if (to_boolean (me->get_layout ()
		  ->lookup_variable (ly_symbol2scm ("debug-slur-scoring")))
      && scm_is_string (quant_score))
    {
      String str;
      SCM properties = Font_interface::text_font_alist_chain (me);

      Stencil tm = *unsmob_stencil (Text_interface::interpret_markup
				    (me->get_layout ()->self_scm (), properties,
				     quant_score));
      a.add_at_edge (Y_AXIS, get_grob_direction (me), tm, 1.0, 0);
    }
#endif

  return a.smobbed_copy ();
}


Bezier
Slur::get_curve (Grob*me)
{
  Bezier b;
  int i = 0;
  for (SCM s = me->get_property ("control-points"); s != SCM_EOL;
       s = scm_cdr (s))
    b.control_[i++] = ly_scm2offset (scm_car (s));

  return b;
}

void
Slur::add_column (Grob*me, Grob*n)
{
  Pointer_group_interface::add_grob (me, ly_symbol2scm ("note-columns"), n);
  add_bound_item (dynamic_cast<Spanner*> (me), dynamic_cast<Item*> (n));
}


void
Slur::add_extra_encompass (Grob*me, Grob*n)
{
  Pointer_group_interface::add_grob (me, ly_symbol2scm ("encompass-objects"), n);
}



MAKE_SCHEME_CALLBACK (Slur, outside_slur_callback, 2);
SCM
Slur::outside_slur_callback (SCM grob, SCM axis)
{
  Grob *script = unsmob_grob (grob);
  Axis a = Axis (scm_to_int (axis));
  assert (a == Y_AXIS);

  Grob *slur = unsmob_grob (script->get_property ("slur"));

  if (!slur)
    return scm_from_int (0);
  
  Grob *cx = script->common_refpoint (slur, X_AXIS);
  Grob *cy = script->common_refpoint (slur, Y_AXIS);

  Bezier curve = Slur::get_curve (slur);

  curve.translate (Offset (slur->relative_coordinate (cx, X_AXIS),
			   slur->relative_coordinate (cy, Y_AXIS)));

  Interval yext = robust_relative_extent (script, cy, Y_AXIS);
  Interval xext = robust_relative_extent (script, cx, X_AXIS);


  Real slur_padding = robust_scm2double (script->get_property ("slur-padding"),
					 0.0);	// todo: slur property, script property?
  yext.widen (slur_padding);
  Real EPS = 1e-3;
  
  Interval bezext (curve.control_[0][X_AXIS],
		   curve.control_[3][X_AXIS]);

  bool consider[] = { false, false, false };
  Real ys[] = {0, 0, 0};
  int k = 0;
  bool do_shift = false;

  for (int d = LEFT; d <= RIGHT; d++)
    {
      Real x = xext.linear_combination ((Direction) d);
      consider[k] = bezext.contains (x);

      if (consider[k])
	{
	  ys[k]
	    = (fabs(bezext[LEFT] - x) < EPS)
	    ? curve.control_[0][Y_AXIS]
	    : ((fabs(bezext[RIGHT] - x) < EPS)
	       ? curve.control_[3][Y_AXIS]
	       : curve.get_other_coordinate (X_AXIS, x));
	  consider[k] = true;
	  
	  if (yext.contains (ys[k]))
	    do_shift = true;
	}
    }
  Real offset = 0.0;
  if (do_shift)
    {
      k = 0;
      Direction dir = get_grob_direction (script);
      for (int d = LEFT; d <= RIGHT; d++)
	{
	  offset = dir * (dir * offset >? dir
			  * (ys[k] - yext[-dir] + dir * slur_padding));
	  k++;
	}
    }
  
  return scm_make_real (offset);
}

static Direction
get_default_dir (Grob*me)
{
  Link_array<Grob> encompasses
    = Pointer_group_interface__extract_grobs (me, (Grob*) 0, "note-columns");

  Direction d = DOWN;
  for (int i= 0; i < encompasses.size (); i ++)
    {
      if (Note_column::dir (encompasses[i]) < 0)
	{
	  d = UP;
	  break;
	}
    }
  return d;
}


MAKE_SCHEME_CALLBACK (Slur, after_line_breaking,1);
SCM
Slur::after_line_breaking (SCM smob)
{
  Spanner *me = dynamic_cast<Spanner*> (unsmob_grob (smob));
  if (!scm_ilength (me->get_property ("note-columns")))
    {
      me->suicide ();
      return SCM_UNSPECIFIED;
    }

  if (!get_grob_direction (me))
    set_grob_direction (me, get_default_dir (me));

  if (scm_ilength (me->get_property ("control-points")) < 4)
    set_slur_control_points (me);

  return SCM_UNSPECIFIED;
}

ADD_INTERFACE (Slur, "slur-interface",
	       "A slur",
	       "quant-score excentricity encompass-objects control-points dash-period dash-fraction slur-details direction height-limit note-columns ratio thickness");


