/*
  beam.cc -- implement Beam

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
    Jan Nieuwenhuizen <janneke@gnu.org>

*/

/*
  [TODO]

  -* shorter! (now +- 1000 lines)
  
  -* less hairy code

  -* Remove #'direction from beam. The beam has no direction per se.
  It may only set directions for stems.
  */


#include <math.h> // tanh.

#include "molecule.hh" 
#include "directional-element-interface.hh"
#include "beaming.hh"
#include "beam.hh"
#include "misc.hh"
#include "least-squares.hh"
#include "stem.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "group-interface.hh"
#include "staff-symbol-referencer.hh"
#include "item.hh"
#include "spanner.hh"
#include "warn.hh"

void
Beam::add_stem (Grob*me, Grob*s)
{
  Pointer_group_interface::add_element (me, ly_symbol2scm ("stems"), s);
  
  s->add_dependency (me);

  assert (!Stem::beam_l (s));
  s->set_grob_property ("beam", me->self_scm ());

  add_bound_item (dynamic_cast<Spanner*> (me), dynamic_cast<Item*> (s));
}

int
Beam::get_multiplicity (Grob*me) 
{
  int m = 0;
  for (SCM s = me->get_grob_property ("stems"); gh_pair_p (s); s = ly_cdr (s))
    {
      Grob * sc = unsmob_grob (ly_car (s));

      if (Stem::has_interface (sc))
	m = m >? Stem::beam_count (sc,LEFT) >? Stem::beam_count (sc,RIGHT);
    }
  return m;
}

/*
  After pre-processing all directions should be set.
  Several post-processing routines (stem, slur, script) need stem/beam
  direction.
  Currenly, this means that beam has set all stem's directions.
  [Alternatively, stems could set its own directions, according to
   their beam, during 'final-pre-processing'.]
 */
MAKE_SCHEME_CALLBACK (Beam,before_line_breaking,1);
SCM
Beam::before_line_breaking (SCM smob)
{
  Grob * me =  unsmob_grob (smob);

  /*
    Beams with less than 2 two stems don't make much sense, but could happen
    when you do

    [r8 c8 r8].
    
    For a beam that  only has one stem, we try to do some disappearance magic:
    we revert the flag, and move on to The Eternal Engraving Fields.*/
  
  
  if (visible_stem_count (me) < 2)
    {
      warning (_ ("beam has less than two visible stems"));

      SCM stems = me->get_grob_property ("stems");
      if (scm_ilength (stems) == 1)
	{
	  warning (_("Beam has less than two stems. Removing beam."));

	  unsmob_grob (gh_car (stems))->remove_grob_property ("beam");
	  me->suicide ();

	  return SCM_UNSPECIFIED;
	}
      else if (scm_ilength (stems) == 0)
	{
	  me->suicide ();
	  return SCM_UNSPECIFIED;	  
	}
    }
  if (visible_stem_count (me) >= 1)
    {
      if (!Directional_element_interface::get (me))
	Directional_element_interface::set (me, get_default_dir (me));
      
      consider_auto_knees (me);
      set_stem_directions (me);
      set_stem_shorten (me);
    }
  return SCM_EOL;
}

Direction
Beam::get_default_dir (Grob*me) 
{
  Drul_array<int> total;
  total[UP]  = total[DOWN] = 0;
  Drul_array<int> count; 
  count[UP]  = count[DOWN] = 0;
  Direction d = DOWN;

  Link_array<Item> stems=
	Pointer_group_interface__extract_elements (me, (Item*)0, "stems");

  for (int i=0; i <stems.size (); i++)
    do {
      Grob *s = stems[i];
      Direction sd = Directional_element_interface::get (s);
      int current = sd	? (1 + d * sd)/2
	: Stem::get_center_distance (s, (Direction)-d);

      if (current)
	{
	  total[d] += current;
	  count[d] ++;
	}

    } while (flip (&d) != DOWN);
  
  SCM func = me->get_grob_property ("dir-function");
  SCM s = gh_call2 (func,
		    gh_cons (gh_int2scm (count[UP]),
			     gh_int2scm (count[DOWN])),
		    gh_cons (gh_int2scm (total[UP]),
			     gh_int2scm (total[DOWN])));

  if (gh_number_p (s) && gh_scm2int (s))
    return to_dir (s);
  
  /*
    If dir is not determined: get default
  */
  return to_dir (me->get_grob_property ("neutral-direction"));
}


/*
  Set all stems with non-forced direction to beam direction.
  Urg: non-forced should become `without/with unforced' direction,
       once stem gets cleaned-up.
 */
void
Beam::set_stem_directions (Grob*me)
{
  Link_array<Item> stems
    =Pointer_group_interface__extract_elements (me, (Item*) 0, "stems");
  Direction d = Directional_element_interface::get (me);
  
  for (int i=0; i <stems.size (); i++)
    {
      Grob *s = stems[i];
      SCM force = s->remove_grob_property ("dir-forced");
      if (!gh_boolean_p (force) || !gh_scm2bool (force))
	Directional_element_interface ::set (s,d);
    }
} 

/*
  Simplistic auto-knees; only consider vertical gap between two
  adjacent chords.

  `Forced' stem directions are ignored.  If you don't want auto-knees,
  don't set, or unset auto-knee-gap.
 */
void
Beam::consider_auto_knees (Grob *me)
{
  SCM scm = me->get_grob_property ("auto-knee-gap");

  if (gh_number_p (scm))
    {
      bool knee_b = false;
      Real knee_y = 0;
      Real staff_space = Staff_symbol_referencer::staff_space (me);
      Real gap = gh_scm2double (scm) / staff_space;

      Direction d = Directional_element_interface::get (me);
      Link_array<Item> stems=
	Pointer_group_interface__extract_elements (me, (Item*)0, "stems");
      
      Grob *common = me->common_refpoint (stems[0], Y_AXIS);
      for (int i=1; i < stems.size (); i++)
	if (!Stem::invisible_b (stems[i]))
	  common = common->common_refpoint (stems[i], Y_AXIS);

      int l = 0;
      for (int i=1; i < stems.size (); i++)
        {
	  if (!Stem::invisible_b (stems[i-1]))
	    l = i - 1;
	  if (Stem::invisible_b (stems[l]))
	    continue;
	  if (Stem::invisible_b (stems[i]))
	    continue;
	  
	  Real left = Stem::extremal_heads (stems[l])[d]
	    ->relative_coordinate (common, Y_AXIS);
	  Real right = Stem::extremal_heads (stems[i])[-d]
	    ->relative_coordinate (common, Y_AXIS);

	  Real dy = right - left;

	  if (abs (dy) >= gap)
	    {
	      knee_y = (right + left) / 2;
	      knee_b = true;
	      break;
	    }
	}
      
      if (knee_b)
	{
	  for (int i=0; i < stems.size (); i++)
	    {
	      if (Stem::invisible_b (stems[i]))
		continue;
	      Item *s = stems[i];	  
	      Real y = Stem::extremal_heads (stems[i])[d]
		->relative_coordinate (common, Y_AXIS);

	      Directional_element_interface::set (s, y < knee_y ? UP : DOWN);
	      s->set_grob_property ("dir-forced", SCM_BOOL_T);
	    }
	}
    }
}

/*
 Set stem's shorten property if unset.
 TODO:
    take some y-position (chord/beam/nearest?) into account
    scmify forced-fraction
 */
void
Beam::set_stem_shorten (Grob*m)
{
  Spanner*me = dynamic_cast<Spanner*> (m);

  Real forced_fraction = forced_stem_count (me) / visible_stem_count (me);
  if (forced_fraction < 0.5)
    return;

  int multiplicity = get_multiplicity (me);

  SCM shorten = me->get_grob_property ("beamed-stem-shorten");
  if (shorten == SCM_EOL)
    return;

  int sz = scm_ilength (shorten);
  
  Real staff_space = Staff_symbol_referencer::staff_space (me);
  SCM shorten_elt = scm_list_ref (shorten, gh_int2scm (multiplicity <? (sz - 1)));
  Real shorten_f = gh_scm2double (shorten_elt) * staff_space;

  /* cute, but who invented me -- how to customise ? */
  if (forced_fraction < 1)
    shorten_f /= 2;

  Link_array<Item> stems=
    Pointer_group_interface__extract_elements (me, (Item*)0, "stems");

  for (int i=0; i < stems.size (); i++)
    {
      Item* s = stems[i];
      if (Stem::invisible_b (s))
        continue;
      if (gh_number_p (s->get_grob_property ("shorten")))
	s->set_grob_property ("shorten", gh_double2scm (shorten_f));
    }
}

/*
  Call list of y-dy-callbacks, that handle setting of
  grob-properties y, dy.

  User may set grob-properties: y-position-hs and height-hs
 (to be fixed) that override the calculated y and dy.

  Because y and dy cannot be calculated and quanted separately, we
  always calculate both, then check for user override.
 */
MAKE_SCHEME_CALLBACK (Beam, after_line_breaking, 1);
SCM
Beam::after_line_breaking (SCM smob)
{
  Grob * me =  unsmob_grob (smob);
  
  me->set_grob_property ("y", gh_double2scm (0));
  me->set_grob_property ("dy", gh_double2scm (0));

  /* Hmm, callbacks should be called by, a eh, callback mechanism
    somewhere (?), I guess, not by looping here. */
  
  SCM list = me->get_grob_property ("y-dy-callbacks");
  for (SCM i = list; gh_pair_p (i); i = ly_cdr (i))
    gh_call1 (ly_car (i), smob);

  // UGH. Y is not in staff position unit?
  // Ik dacht datwe daar juist van weg wilden?
  
  // Hmm, nu hebben we 3 dimensies, want inmiddels zijn we daar
  // weer terug, maar dan / 2
  // (staff-space iso staff-position)
  
  set_stem_lengths (me);

  return SCM_UNSPECIFIED;
}


MAKE_SCHEME_CALLBACK (Beam, least_squares, 1);
SCM
Beam::least_squares (SCM smob)
{
 Grob *me = unsmob_grob (smob);

 if (visible_stem_count (me) <= 1)
   return SCM_UNSPECIFIED;

  Real y = 0;
  Real dy = 0;

  /* Stem_info, and thus y,dy in this function are corrected for beam-dir */
  Real first_ideal = Stem::calc_stem_info (first_visible_stem (me)).idealy_f_;
  if (first_ideal == Stem::calc_stem_info (last_visible_stem (me)).idealy_f_)
    {
      y = first_ideal;
      dy = 0;
    }
  else
    {
      Array<Offset> ideals;

      // ugh -> use commonx
      Real x0 = first_visible_stem (me)->relative_coordinate (0, X_AXIS);
      Link_array<Item> stems=
	Pointer_group_interface__extract_elements (me, (Item*)0, "stems");

      for (int i=0; i < stems.size (); i++)
	{
	  Item* s = stems[i];
	  if (Stem::invisible_b (s))
	    continue;
	  ideals.push (Offset (s->relative_coordinate (0, X_AXIS) - x0, 
			       Stem::calc_stem_info (s).idealy_f_));
	}
      Real dydx;
      minimise_least_squares (&dydx, &y, ideals);

      Real dx = last_visible_stem (me)->relative_coordinate (0, X_AXIS) - x0;
      dy = dydx * dx;
    }

  /* Store true, not dir-corrected values */
  Direction dir = Directional_element_interface::get (me);
  me->set_grob_property ("y", gh_double2scm (y * dir));
  me->set_grob_property ("dy", gh_double2scm (dy * dir));
  return SCM_UNSPECIFIED;
}

MAKE_SCHEME_CALLBACK (Beam, cancel_suspect_slope, 1);
SCM
Beam::cancel_suspect_slope (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  
  if (visible_stem_count (me) <= 1)
    return SCM_UNSPECIFIED;
  
  /* Stem_info, and thus y,dy in this function are corrected for beam-dir */
  Direction dir = Directional_element_interface::get (me);
  Real y = gh_scm2double (me->get_grob_property ("y")) * dir;
  Real dy = gh_scm2double (me->get_grob_property ("dy")) * dir;
  
 /* steep slope running against lengthened stem is suspect */
  Real first_ideal = Stem::calc_stem_info (first_visible_stem (me)).idealy_f_;
  Real last_ideal = Stem::calc_stem_info (last_visible_stem (me)).idealy_f_;
  Real lengthened = gh_scm2double (me->get_grob_property ("outer-stem-length-limit"));
  Real steep = gh_scm2double (me->get_grob_property ("slope-limit"));

  // ugh -> use commonx
  Real dx = last_visible_stem (me)->relative_coordinate (0, X_AXIS) - first_visible_stem (me)->relative_coordinate (0, X_AXIS);
  Real dydx = dy && dx ? dy/dx : 0;

  if (( (y - first_ideal > lengthened) && (dydx > steep))
      || ((y + dy - last_ideal > lengthened) && (dydx < -steep)))
    {
      Real adjusted_y = y + dy / 2;
      /* Store true, not dir-corrected values */
      me->set_grob_property ("y", gh_double2scm (adjusted_y * dir));
      me->set_grob_property ("dy", gh_double2scm (0)); 
    }
  return SCM_UNSPECIFIED;
}

/*
  This neat trick is by Werner Lemberg,
  damped = tanh (slope)
  corresponds with some tables in [Wanske]
*/
MAKE_SCHEME_CALLBACK (Beam, slope_damping, 1);
SCM
Beam::slope_damping (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  if (visible_stem_count (me) <= 1)
    return SCM_UNSPECIFIED;

  SCM s = me->get_grob_property ("damping"); 
  int damping = gh_scm2int (s);

  if (damping)
    {
      /* y,dy in this function are corrected for beam-dir */
      Direction dir = Directional_element_interface::get (me);
      Real y = gh_scm2double (me->get_grob_property ("y")) * dir;
      Real dy = gh_scm2double (me->get_grob_property ("dy")) * dir;
      
      // ugh -> use commonx
      Real dx = last_visible_stem (me)->relative_coordinate (0, X_AXIS)
	- first_visible_stem (me)->relative_coordinate (0, X_AXIS);
      Real dydx = dy && dx ? dy/dx : 0;
      dydx = 0.6 * tanh (dydx) / damping;

      Real damped_dy = dydx * dx;
      Real adjusted_y = y + (dy - damped_dy) / 2;
      /* Store true, not dir-corrected values */
      me->set_grob_property ("y", gh_double2scm (adjusted_y * dir));
      me->set_grob_property ("dy", gh_double2scm (damped_dy * dir));
    }
    return SCM_UNSPECIFIED;
}

/*
  Quantise dy (height) of beam.
  Generalisation of [Ross].
  */
MAKE_SCHEME_CALLBACK (Beam, quantise_dy, 1);
SCM
Beam::quantise_dy (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  if (visible_stem_count (me) <= 1)
    return SCM_UNSPECIFIED;

  Array<Real> a;
  SCM proc = me->get_grob_property ("height-quants");
  SCM quants = gh_call2 (proc, me->self_scm (),
			 gh_double2scm (me->paper_l ()->get_var ("stafflinethickness")
					/ 1.0));
  
  for (SCM s = quants; gh_pair_p (s); s = ly_cdr (s))
    a.push (gh_scm2double (ly_car (s)));
  
  if (a.size () > 1)
    {
      /* y,dy in this function are corrected for beam-dir */
      Direction dir = Directional_element_interface::get (me);
      Real y = gh_scm2double (me->get_grob_property ("y")) * dir;
      Real dy = gh_scm2double (me->get_grob_property ("dy")) * dir;

      Real staff_space = Staff_symbol_referencer::staff_space (me);
      
      Interval iv = quantise_iv (a, abs (dy)/staff_space) * staff_space;
      Real q = (abs (dy) - iv[SMALLER] <= iv[BIGGER] - abs (dy))
	? iv[SMALLER]
	: iv[BIGGER];
      
      Real quantised_dy = q * sign (dy);
      Real adjusted_y = y + (dy - quantised_dy) / 2;
      /* Store true, not dir-corrected values */
      me->set_grob_property ("y", gh_double2scm (adjusted_y * dir));
      me->set_grob_property ("dy", gh_double2scm (quantised_dy * dir));
    }
  return SCM_UNSPECIFIED;
}

/* It's tricky to have the user override y,dy directly, so we use this
   translation func.  Also, if our staff_space != 1 (smaller staff, eg),
   user will expect staff-position to be discrete values. */
MAKE_SCHEME_CALLBACK (Beam, user_override, 1);
SCM
Beam::user_override (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Real staff_space = Staff_symbol_referencer::staff_space (me);

  SCM s = me->get_grob_property ("staff-position");
  if (gh_number_p (s))
    {
      Real y = gh_scm2double (s) * staff_space * 0.5;
      me->set_grob_property ("y", gh_double2scm (y));
    }

  /* Name suggestions? Tilt, slope, vertical-* ? */
  s = me->get_grob_property ("height");
  if (gh_number_p (s))
    {
      Real dy = gh_scm2double (s) * staff_space * 0.5;
      me->set_grob_property ("dy", gh_double2scm (dy));
    }
  
  return SCM_UNSPECIFIED;
}

/*
  Ugh, this must be last, after user_override
  Assumes directionised y/dy.
 */
MAKE_SCHEME_CALLBACK (Beam, do_quantise_y, 1);
SCM
Beam::do_quantise_y (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  /*
    If the user set y-position, we shouldn't do quanting.
   */
  if (gh_number_p (me->get_grob_property ("y-position-hs")))
    return SCM_UNSPECIFIED;

  Real y = gh_scm2double (me->get_grob_property ("y"));
  Real dy = gh_scm2double (me->get_grob_property ("dy"));
      
  /* we can modify y, so we should quantise y */
  Real half_space = Staff_symbol_referencer::staff_space (me) / 2;
  Real y_shift = check_stem_length_f (me, y, dy);
  y += y_shift;
  y = quantise_y_f (me, y, dy, 0);

  /*
    Hmm, this is a bit keyhole operation: we're passing `this' as a
    parameter, and member vars as SCM properties.  We should decide on
    SCM/C/C++ boundary */
  me->set_grob_property ("y", gh_double2scm (y));
  set_stem_lengths (me);
  y = gh_scm2double (me->get_grob_property ("y"));
  
  y_shift = check_stem_length_f (me, y, dy);

  if (y_shift > half_space / 4)
    {
      y += y_shift;

      /*
	for significantly lengthened or shortened stems,
	request quanting the other way.
      */
      int quant_dir = 0;
      if (abs (y_shift) > half_space / 2)
	quant_dir = sign (y_shift) * Directional_element_interface::get (me);
      y = quantise_y_f (me, y, dy, quant_dir);
    }
  
  me->set_grob_property ("y", gh_double2scm (y));
  // me->set_grob_property ("dy", gh_double2scm (dy));
  return SCM_UNSPECIFIED;
}


Real
Beam::calc_stem_y_f (Grob*me,Item* s, Real y, Real dy) 
{
  int beam_multiplicity = get_multiplicity (me);
  int stem_multiplicity = (Stem::flag_i (s) - 2) >? 0;

  SCM space_proc = me->get_grob_property ("space-function");
  SCM space = gh_call1 (space_proc, gh_int2scm (beam_multiplicity));

  Real thick = gh_scm2double (me->get_grob_property ("thickness")) ;
  Real interbeam_f = gh_scm2double (space) ;

  // ugh -> use commonx
  Real x0 = first_visible_stem (me)->relative_coordinate (0, X_AXIS);
  Real dx = last_visible_stem (me)->relative_coordinate (0, X_AXIS) - x0;
  Real stem_y = (dy && dx ? (s->relative_coordinate (0, X_AXIS) - x0) / dx * dy : 0) + y;

  /* knee */
   Direction dir  = Directional_element_interface::get (me);
   Direction sdir = Directional_element_interface::get (s);
   
    /* knee */
   if (dir!= sdir)
      {
       stem_y -= dir 
	* (thick / 2 + (beam_multiplicity - 1) * interbeam_f);


      
      // huh, why not for first visible?
       if (Staff_symbol_referencer::staff_symbol_l (s)
	   != Staff_symbol_referencer::staff_symbol_l (last_visible_stem (me)))
	 stem_y += Directional_element_interface::get (me)
	   * (beam_multiplicity - stem_multiplicity) * interbeam_f;
      }

  return stem_y;
}

Real
Beam::check_stem_length_f (Grob*me,Real y, Real dy) 
{
  Real shorten = 0;
  Real lengthen = 0;
  Direction dir = Directional_element_interface::get (me);

  Link_array<Item> stems=
    Pointer_group_interface__extract_elements (me, (Item*)0, "stems");

  for (int i=0; i < stems.size (); i++)
    {
      Item* s = stems[i];
      if (Stem::invisible_b (s))
	continue;

      Real stem_y = calc_stem_y_f (me, s, y, dy);
	
      stem_y *= dir;
      Stem_info info = Stem::calc_stem_info (s);

      // if (0 > info.maxy_f_ - stem_y)
      shorten = shorten <? info.maxy_f_ - stem_y;
      // if (0 < info.miny_f_ - stem_y)
      lengthen = lengthen >? info.miny_f_ - stem_y; 
    }

  if (lengthen && shorten)
    warning (_ ("weird beam vertical offset"));

  /* when all stems are too short, normal stems win */
  return dir * ((shorten) ?  shorten : lengthen);
}

/*
  Hmm.  At this time, beam position and slope are determined.  Maybe,
  stem directions and length should set to relative to the chord's
  position of the beam.  */
void
Beam::set_stem_lengths (Grob *me)
{
  if (visible_stem_count (me) <= 1)
    return;
  
  Real y = gh_scm2double (me->get_grob_property ("y"));
  Real dy = gh_scm2double (me->get_grob_property ("dy"));

  Real half_space = Staff_symbol_referencer::staff_space (me)/2;
  Link_array<Item> stems=
    Pointer_group_interface__extract_elements (me, (Item*)0, "stems");

  Grob *common = me->common_refpoint (stems[0], Y_AXIS);
  for (int i=1; i < stems.size (); i++)
    if (!Stem::invisible_b (stems[i]))
      common = common->common_refpoint (stems[i], Y_AXIS);

  for (int i=0; i < stems.size (); i++)
    {
      Item* s = stems[i];
      if (Stem::invisible_b (s))
	continue;

      Real stem_y = calc_stem_y_f (me, s, y, dy);

      // doesn't play well with dvips
      if (scm_definedp (ly_symbol2scm ("ps-testing"), SCM_UNDEFINED)
	  == SCM_BOOL_T)
	if (Stem::get_direction (s) == Directional_element_interface::get (me))
	  stem_y += Stem::get_direction (s)
	    * gh_scm2double (me->get_grob_property ("thickness")) / 2;
      
      /* caution: stem measures in staff-positions */
      Real id = me->relative_coordinate (common, Y_AXIS)
	- stems[i]->relative_coordinate (common, Y_AXIS);
      Stem::set_stemend (s, (stem_y + id) / half_space);
    }
}

/*
  Prevent interference from stafflines and beams.

  We only need to quantise the (left) y of the beam,
  since dy is quantised too.
  if extend_b then stems must *not* get shorter
 */
Real
Beam::quantise_y_f (Grob*me,Real y, Real dy, int quant_dir)
{
  int multiplicity = get_multiplicity (me);

  Real staff_space = Staff_symbol_referencer::staff_space (me);
  Real thick = me->paper_l ()->get_var ("stafflinethickness");


  SCM proc = me->get_grob_property ("vertical-position-quant-function");
  SCM quants = scm_apply (proc,
			  me->self_scm (),
			  scm_list_n (gh_int2scm (multiplicity),
				   gh_double2scm (dy/staff_space),
				   gh_double2scm (thick/staff_space),
				   SCM_EOL, SCM_UNDEFINED));
  
  Array<Real> a;

  for (; gh_pair_p (quants); quants = ly_cdr (quants))
    a.push (gh_scm2double (ly_car (quants)));

  if (a.size () <= 1)
    return y;

  Real up_y = Directional_element_interface::get (me) * y;
  Interval iv = quantise_iv (a, up_y/staff_space) * staff_space;

  Real q = up_y - iv[SMALLER] <= iv[BIGGER] - up_y 
    ? iv[SMALLER] : iv[BIGGER];
  if (quant_dir)
    q = iv[ (Direction)quant_dir];

  return q * Directional_element_interface::get (me);
}

void
Beam::set_beaming (Grob*me,Beaming_info_list *beaming)
{
  Link_array<Grob> stems=
    Pointer_group_interface__extract_elements (me, (Grob*)0, "stems");
  
  Direction d = LEFT;
  for (int i=0; i  < stems.size (); i++)
    {
      do
	{
	  /* Don't overwrite user override (?) */
	  if (Stem::beam_count (stems[i], d) == 0
	      /* Don't set beaming for outside of outer stems */
	      && ! (d == LEFT && i == 0)
	      && ! (d == RIGHT && i == stems.size () -1))
	    {
	      int b = beaming->infos_.elem (i).beams_i_drul_[d];
	      Stem::set_beaming (stems[i], b, d);
	    }
	}
      while (flip (&d) != LEFT);
    }
}



/*
  beams to go with one stem.

  FIXME: clean me up.
  */
Molecule
Beam::stem_beams (Grob*me,Item *here, Item *next, Item *prev,
		  Real /* dy */ , Real dydx
		  ) 
{
  // ugh -> use commonx
  if ((next && ! (next->relative_coordinate (0, X_AXIS) > here->relative_coordinate (0, X_AXIS))) ||
 (prev && ! (prev->relative_coordinate (0, X_AXIS) < here->relative_coordinate (0, X_AXIS))))
      programming_error ("Beams are not left-to-right");

  int multiplicity = get_multiplicity (me);

  SCM space_proc = me->get_grob_property ("space-function");
  SCM space = gh_call1 (space_proc, gh_int2scm (multiplicity));

  Real thick = gh_scm2double (me->get_grob_property ("thickness")) ;
  Real interbeam_f = gh_scm2double (space) ;
    
  Real bdy = interbeam_f;
  
#if 0
    // ugh -> use commonx
  Real dx = visible_stem_count (me) ?
    last_visible_stem (me)->relative_coordinate (0, X_AXIS) - first_visible_stem (me)->relative_coordinate (0, X_AXIS)
    : 0.0;
#endif
  
  Molecule leftbeams;
  Molecule rightbeams;

  Real nw_f;
  if (!Stem::first_head (here))
    nw_f = 0;
  else {
    int t = Stem::type_i (here); 

    SCM proc = me->get_grob_property ("flag-width-function");
    SCM result = gh_call1 (proc, gh_int2scm (t));
    nw_f = gh_scm2double (result);
  }


  Direction dir = Directional_element_interface::get (me);

  /* [Tremolo] beams on whole notes may not have direction set? */
 if (dir == CENTER)
    dir = Directional_element_interface::get (here);


  /* half beams extending to the left. */
  if (prev)
    {
      int lhalfs= lhalfs = Stem::beam_count (here,LEFT) - Stem::beam_count (prev,RIGHT);
      int lwholebeams= Stem::beam_count (here,LEFT) <? Stem::beam_count (prev,RIGHT) ;
      /*
       Half beam should be one note-width, 
       but let's make sure two half-beams never touch
       */

      // FIXME: TODO (check) stem width / sloped beams
      Real w = here->relative_coordinate (0, X_AXIS)
	- prev->relative_coordinate (0, X_AXIS);
      Real stem_w = gh_scm2double (prev->get_grob_property ("thickness"))
	// URG
	* me->paper_l ()->get_var ("stafflinethickness");

      w = w/2 <? nw_f;
      Molecule a;
      if (lhalfs)		// generates warnings if not
	a =  Lookup::beam (dydx, w + stem_w, thick);
      a.translate (Offset (-w, -w * dydx));
      a.translate_axis (-stem_w/2, X_AXIS);
      for (int j = 0; j  < lhalfs; j++)
	{
	  Molecule b (a);
	  b.translate_axis (-dir * bdy * (lwholebeams+j), Y_AXIS);
	  leftbeams.add_molecule (b);
	}
    }

  if (next)
    {
      int rhalfs  = Stem::beam_count (here,RIGHT)
	- Stem::beam_count (next,LEFT);
      int rwholebeams= Stem::beam_count (here,RIGHT)
	<? Stem::beam_count (next,LEFT) ;

      Real w = next->relative_coordinate (0, X_AXIS)
	- here->relative_coordinate (0, X_AXIS);

      Real stem_w = gh_scm2double (next->get_grob_property ("thickness"))
	// URG
	* me->paper_l ()->get_var ("stafflinethickness");

      Molecule a = Lookup::beam (dydx, w + stem_w, thick);
      a.translate_axis (- stem_w/2, X_AXIS);
      int j = 0;
      Real gap_f = 0;
      
      SCM gap = me->get_grob_property ("gap");
      if (gh_number_p (gap))
	{
	  int gap_i = gh_scm2int ((gap));
	  int nogap = rwholebeams - gap_i;
	  
	  for (; j  < nogap; j++)
	    {
	      Molecule b (a);
	      b.translate_axis (-dir  * bdy * j, Y_AXIS);
	      rightbeams.add_molecule (b);
	    }
	  if (Stem::invisible_b (here))
	    gap_f = nw_f;
	  else
	    gap_f = nw_f / 2;
	  w -= 2 * gap_f;
	  a = Lookup::beam (dydx, w + stem_w, thick);
	}

      for (; j  < rwholebeams; j++)
	{
	  Molecule b (a);
	  Real tx = 0;
	  if (Stem::invisible_b (here))
	    // ugh, see chord-tremolo.ly
	    tx = (-dir + 1) / 2 * nw_f * 1.5 + gap_f/4;
	  else
	    tx = gap_f;
	  b.translate (Offset (tx, -dir * bdy * j));
	  rightbeams.add_molecule (b);
	}

      w = w/2 <? nw_f;
      if (rhalfs)
	a = Lookup::beam (dydx, w, thick);

      for (; j  < rwholebeams + rhalfs; j++)
	{
	  Molecule b (a);
	  b.translate_axis (- dir * bdy * j, Y_AXIS);
	  rightbeams.add_molecule (b);
	}

    }
  leftbeams.add_molecule (rightbeams);

  /*
    Does beam quanting think  of the asymetry of beams? 
    Refpoint is on bottom of symbol. (FIXTHAT) --hwn.
   */
  return leftbeams;
}

MAKE_SCHEME_CALLBACK (Beam,brew_molecule,1);
SCM
Beam::brew_molecule (SCM smob)
{
  Grob * me =unsmob_grob (smob);

  Molecule mol;
  if (!gh_pair_p (me->get_grob_property ("stems")))
    return SCM_EOL;
  Real x0,dx;
  Link_array<Item>stems = 
    Pointer_group_interface__extract_elements (me, (Item*) 0, "stems");  
  if (visible_stem_count (me))
    {
  // ugh -> use commonx
      x0 = first_visible_stem (me)->relative_coordinate (0, X_AXIS);
      dx = last_visible_stem (me)->relative_coordinate (0, X_AXIS) - x0;
    }
  else
    {
      x0 = stems[0]->relative_coordinate (0, X_AXIS);
      dx = stems.top ()->relative_coordinate (0, X_AXIS) - x0;
    }
  


  /*
    TODO: the naming of the grob properties sucks.
   */
  SCM dy_s = me->get_grob_property ("dy");
  SCM y_s = me->get_grob_property ("y");

  
  Real dy = gh_number_p (dy_s) ? gh_scm2double (dy_s) : 0.0;
  Real dydx = dy && dx ? dy/dx : 0;
  Real y = gh_number_p (y_s) ? gh_scm2double (y_s) : 0.0;


  for (int j=0; j <stems.size (); j++)
    {
      Item *i = stems[j];
      Item * prev = (j > 0)? stems[j-1] : 0;
      Item * next = (j < stems.size ()-1) ? stems[j+1] :0;

      Molecule sb = stem_beams (me, i, next, prev, dy, dydx);
      Real x = i->relative_coordinate (0, X_AXIS)-x0;
      sb.translate (Offset (x, x * dydx + y));
      mol.add_molecule (sb);
    }
  mol.translate_axis (x0 
    - dynamic_cast<Spanner*> (me)->get_bound (LEFT)->relative_coordinate (0, X_AXIS), X_AXIS);

  return mol.smobbed_copy ();
}

int
Beam::forced_stem_count (Grob*me) 
{
  Link_array<Item>stems = 
    Pointer_group_interface__extract_elements (me, (Item*) 0, "stems");
  int f = 0;
  for (int i=0; i < stems.size (); i++)
    {
      Item *s = stems[i];

      if (Stem::invisible_b (s))
	continue;

      if (( (int)Stem::chord_start_f (s)) 
        && (Stem::get_direction (s) != Stem::get_default_dir (s)))
        f++;
    }
  return f;
}




/* TODO:
   use filter and standard list functions.
 */
int
Beam::visible_stem_count (Grob*me) 
{
  Link_array<Item>stems = 
    Pointer_group_interface__extract_elements (me, (Item*) 0, "stems");
  int c = 0;
  for (int i = stems.size (); i--;)
    {
      if (!Stem::invisible_b (stems[i]))
        c++;
    }
  return c;
}

Item*
Beam::first_visible_stem (Grob*me) 
{
  Link_array<Item>stems = 
    Pointer_group_interface__extract_elements (me, (Item*) 0, "stems");
  
  for (int i = 0; i < stems.size (); i++)
    {
      if (!Stem::invisible_b (stems[i]))
        return stems[i];
    }
  return 0;
}

Item*
Beam::last_visible_stem (Grob*me) 
{
  Link_array<Item>stems = 
    Pointer_group_interface__extract_elements (me, (Item*) 0, "stems");
  for (int i = stems.size (); i--;)
    {
      if (!Stem::invisible_b (stems[i]))
        return stems[i];
    }
  return 0;
}


/*
  [TODO]
  handle rest under beam (do_post: beams are calculated now)
  what about combination of collisions and rest under beam.

  Should lookup
    
    rest -> stem -> beam -> interpolate_y_position ()
*/
MAKE_SCHEME_CALLBACK (Beam,rest_collision_callback,2);
SCM
Beam::rest_collision_callback (SCM element_smob, SCM axis)
{
  Grob *rest = unsmob_grob (element_smob);
  Axis a = (Axis) gh_scm2int (axis);
  
  assert (a == Y_AXIS);

  Grob * st = unsmob_grob (rest->get_grob_property ("stem"));
  Grob * stem = st;
  if (!stem)
    return gh_double2scm (0.0);
  Grob * beam = unsmob_grob (stem->get_grob_property ("beam"));
  if (!beam || !Beam::has_interface (beam) || !Beam::visible_stem_count (beam))
    return gh_double2scm (0.0);

  // make callback for rest from this.
  Real beam_dy = 0;
  Real beam_y = 0;


  // todo: make sure this calced already.
  SCM s = beam->get_grob_property ("dy");
  if (gh_number_p (s))
    beam_dy = gh_scm2double (s);
  
  s = beam->get_grob_property ("y");
  if (gh_number_p (s))
    beam_y = gh_scm2double (s);
  
  // ugh -> use commonx
  Real x0 = first_visible_stem (beam)->relative_coordinate (0, X_AXIS);
  Real dx = last_visible_stem (beam)->relative_coordinate (0, X_AXIS) - x0;
  Real dydx = beam_dy && dx ? beam_dy/dx : 0;

  Direction d = Stem::get_direction (stem);
  Real beamy = (stem->relative_coordinate (0, X_AXIS) - x0) * dydx + beam_y;

  Real staff_space =   Staff_symbol_referencer::staff_space (rest);

  
  Real rest_dim = rest->extent (rest, Y_AXIS)[d]*2.0 / staff_space ; // refp??

  Real minimum_dist
    = gh_scm2double (rest->get_grob_property ("minimum-beam-collision-distance"));
  Real dist =
    minimum_dist +  -d  * (beamy - rest_dim) >? 0;

  int stafflines = Staff_symbol_referencer::line_count (rest);

  // move discretely by half spaces.
  int discrete_dist = int (ceil (dist));

  // move by whole spaces inside the staff.
  if (discrete_dist < stafflines+1)
    discrete_dist = int (ceil (discrete_dist / 2.0)* 2.0);

  return gh_double2scm (-d *  discrete_dist);
}


bool
Beam::has_interface (Grob*me)
{
  return me->has_interface (ly_symbol2scm ("beam-interface"));
}

