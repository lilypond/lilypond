/*
  beam.cc -- implement Beam

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
    Jan Nieuwenhuizen <janneke@gnu.org>

*/

/*
  [TODO]
  * shorter! (now +- 1000 lines)
    * less hairy code
    * move paper vars to scm

  remove *-hs variables, and do all y-position stuff in staff-space.
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
#include "cross-staff.hh"
#include "item.hh"
#include "spanner.hh"
#include "warn.hh"

void
Beam::add_stem (Score_element*me, Score_element*s)
{
  Pointer_group_interface:: add_element(me, "stems", s);
  
  s->add_dependency (me);

  assert (!Stem::beam_l (s));
  s->set_elt_property ("beam", me->self_scm ());

  add_bound_item (dynamic_cast<Spanner*> (me), dynamic_cast<Item*> (s));
}

int
Beam::get_multiplicity (Score_element*me) 
{
  int m = 0;
  for (SCM s = me->get_elt_property ("stems"); gh_pair_p (s); s = gh_cdr (s))
    {
      Score_element * sc = unsmob_element (gh_car (s));

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
MAKE_SCHEME_CALLBACK(Beam,before_line_breaking,1);
SCM
Beam::before_line_breaking (SCM smob)
{
  Score_element * me =  unsmob_element (smob);

  // Why?
  if (visible_stem_count (me) < 2)
    {
      warning (_ ("beam has less than two stems"));
    }

  if (!Directional_element_interface::get (me))
    Directional_element_interface::set (me, get_default_dir (me));

  auto_knees (me);
  set_stem_directions (me);
  set_stem_shorten (me);

  return SCM_EOL;
}

Direction
Beam::get_default_dir (Score_element*me) 
{
  Drul_array<int> total;
  total[UP]  = total[DOWN] = 0;
  Drul_array<int> count; 
  count[UP]  = count[DOWN] = 0;
  Direction d = DOWN;

  Link_array<Item> stems=
	Pointer_group_interface__extract_elements (me, (Item*)0, "stems");

  for (int i=0; i <stems.size (); i++)
    do { // HUH -- waar slaat dit op?
      Score_element *s = stems[i];
      Direction sd = Directional_element_interface::get (s);
      int current = sd	? (1 + d * sd)/2
	: Stem::get_center_distance (s, (Direction)-d);

      if (current)
	{
	  total[d] += current;
	  count[d] ++;
	}

    } while (flip(&d) != DOWN);
  
  SCM func = me->get_elt_property ("dir-function");
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
  return to_dir (me->get_elt_property ("default-neutral-direction"));
}


/*
  Set all stems with non-forced direction to beam direction.
  Urg: non-forced should become `without/with unforced' direction,
       once stem gets cleaned-up.
 */
void
Beam::set_stem_directions (Score_element*me)
{
  Link_array<Item> stems
    =Pointer_group_interface__extract_elements (me,  (Item*) 0, "stems");
  Direction d = Directional_element_interface::get (me);
  
  for (int i=0; i <stems.size (); i++)
    {
      Score_element *s = stems[i];
      SCM force = s->remove_elt_property ("dir-forced");
      if (!gh_boolean_p (force) || !gh_scm2bool (force))
	Directional_element_interface ::set (s,d);
    }
} 

void
Beam::auto_knees (Score_element*me)
{
  if (!auto_knee (me,"auto-interstaff-knee-gap", true))
    auto_knee (me, "auto-knee-gap", false);
}

/*
  Simplistic auto-knees; only consider vertical gap between two
  adjacent chords.

  `Forced' stem directions are ignored.  If you don't want auto-knees,
  don't set, or unset autoKneeGap/autoInterstaffKneeGap.
 */
bool
Beam::auto_knee (Score_element*me, String gap_str, bool interstaff_b)
{
  bool knee_b = false;
  int knee_y = 0;
  SCM gap = me->get_elt_property (gap_str.ch_C());

  Direction d = Directional_element_interface::get (me);
      Link_array<Item> stems=
	Pointer_group_interface__extract_elements (me, (Item*)0, "stems");
  
  if (gh_number_p (gap))
    {
      Spanner*sp = dynamic_cast<Spanner*> (me);
      int auto_gap_i = gh_scm2int (gap);
      for (int i=1; i < stems.size (); i++)
        {
	  bool is_b = (bool)(calc_interstaff_dist (stems[i], sp) 
	    - calc_interstaff_dist (stems[i-1], sp));
	  int l_y = (int)(Stem::head_positions(stems[i-1])[d])
	    + (int)calc_interstaff_dist (stems[i-1], sp);
	  int r_y = (int)(Stem::head_positions(stems[i])[d])
	    + (int)calc_interstaff_dist (stems[i], sp);
	  int gap_i = r_y - l_y;

	  if ((abs (gap_i) >= auto_gap_i) && (!interstaff_b || is_b))
	    {
	      knee_y = (r_y + l_y) / 2;
	      knee_b = true;
	      break;
	    }
	}
    }
  if (knee_b)
    {
      for (int i=0; i < stems.size (); i++)
        {
	  Item *s = stems[i];	  
	  int y = (int)(Stem::head_positions(s)[d])
	    + (int)calc_interstaff_dist (s, dynamic_cast<Spanner*> (me));

	  Directional_element_interface::set (s,y < knee_y ? UP : DOWN);
	  s->set_elt_property ("dir-forced", SCM_BOOL_T);
	}
    }
  return knee_b;
}

/*
 Set stem's shorten property if unset.
 TODO:
    take some y-position (chord/beam/nearest?) into account
    scmify forced-fraction
 */
void
Beam::set_stem_shorten (Score_element*m)
{
  Spanner*me = dynamic_cast<Spanner*> (m);
  if (!visible_stem_count (me))
    return;

  Real forced_fraction = forced_stem_count (me) / visible_stem_count (me);
  if (forced_fraction < 0.5)
    return;

  int multiplicity = get_multiplicity (me);

  SCM shorten = me->get_elt_property ("beamed-stem-shorten");
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
      if (gh_number_p (s->get_elt_property ("shorten")))
	s->set_elt_property ("shorten", gh_double2scm (shorten_f));
    }
}

/*
  Set elt properties height and y-position if not set.
  Adjust stem lengths to reach beam.
 */
MAKE_SCHEME_CALLBACK(Beam,after_line_breaking,1);
SCM
Beam::after_line_breaking (SCM smob)
{
  Score_element * me =  unsmob_element (smob);

  /* first, calculate y, dy */
  Real y, dy;
  calc_default_position_and_height (me, &y, &dy);
  if (visible_stem_count (me))
    {
      if (suspect_slope_b (me, y, dy))
	dy = 0;

      Real damped_dy = calc_slope_damping_f (me, dy);
      Real quantised_dy = quantise_dy_f (me, damped_dy);

      y += (dy - quantised_dy) / 2;
      dy = quantised_dy;
    }
  /*
    until here, we used only stem_info, which acts as if dir=up
   */
  y *= Directional_element_interface::get (me);
  dy *= Directional_element_interface::get (me);


  Real half_space = Staff_symbol_referencer::staff_space (me) / 2;

  /* weird: why do we do calc_position_and_height () ? regardless of
     this setting?

  */
  /* check for user-override of dy */
  SCM s = me->remove_elt_property ("height-hs");
  if (gh_number_p (s))
    {
      dy = gh_scm2double (s) * half_space;
    }
  me->set_elt_property ("height", gh_double2scm (dy));

  /* check for user-override of y */
  s = me->remove_elt_property ("y-position-hs");
  if (gh_number_p (s))
    {
      y = gh_scm2double (s) * half_space;
    }
  else
    { 
      /* we can modify y, so we should quantise y */
      Real y_shift = check_stem_length_f (me, y, dy);
      y += y_shift;
      y = quantise_y_f (me,y, dy, 0);
      set_stem_length (me, y, dy);
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
    }
  // UGH. Y is not in staff position unit?
  // Ik dacht datwe daar juist van weg wilden?
  set_stem_length (me, y, dy);
  me->set_elt_property ("y-position", gh_double2scm (y));

  return SCM_UNSPECIFIED;
}

/*
  See Documentation/tex/fonts.doc
 */
void
Beam::calc_default_position_and_height (Score_element*me,Real* y, Real* dy) 
{
  *y = 0;
  *dy = 0;  
  if (visible_stem_count (me) <= 1)
    return;

  Real first_ideal = Stem::calc_stem_info (first_visible_stem (me)).idealy_f_;
  if (first_ideal == Stem::calc_stem_info (last_visible_stem (me)).idealy_f_)
    {
      *dy = 0;
      *y = first_ideal;
      return;
    }

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
  minimise_least_squares (&dydx, y, ideals); // duh, takes references

  Real dx = last_visible_stem (me)->relative_coordinate (0, X_AXIS) - x0;
  *dy = dydx * dx;
}

bool
Beam::suspect_slope_b (Score_element*me, Real y, Real dy) 
{
  /* first, calculate y, dy */
  /*
    steep slope running against lengthened stem is suspect
  */
  Real first_ideal = Stem::calc_stem_info (first_visible_stem (me)).idealy_f_;
  Real last_ideal = Stem::calc_stem_info (last_visible_stem (me)).idealy_f_;
  Real lengthened = gh_scm2double (me->get_elt_property ("outer-stem-length-limit"));
  Real steep = gh_scm2double (me->get_elt_property ("slope-limit"));

  // ugh -> use commonx
  Real dx = last_visible_stem (me)->relative_coordinate (0, X_AXIS) - first_visible_stem (me)->relative_coordinate (0, X_AXIS);
  Real dydx = dy && dx ? dy/dx : 0;

  if (((y - first_ideal > lengthened) && (dydx > steep))
      || ((y + dy - last_ideal > lengthened) && (dydx < -steep)))
    {
      return true;
    }
  return false;
}

/*
  This neat trick is by Werner Lemberg,
  damped = tanh (slope)
  corresponds with some tables in [Wanske]
*/
Real
Beam::calc_slope_damping_f (Score_element*me,Real dy) 
{
  SCM damp = me->get_elt_property ("damping"); 
  int damping = gh_scm2int (damp);

  if (damping)
    {
  // ugh -> use commonx
      Real dx = last_visible_stem (me)->relative_coordinate (0, X_AXIS)
	- first_visible_stem (me)->relative_coordinate (0, X_AXIS);
      Real dydx = dy && dx ? dy/dx : 0;
      dydx = 0.6 * tanh (dydx) / damping;
      return dydx * dx;
    }
  return dy;
}

Real
Beam::calc_stem_y_f (Score_element*me,Item* s, Real y, Real dy) 
{
  int beam_multiplicity = get_multiplicity (me);
  int stem_multiplicity = (Stem::flag_i (s) - 2) >? 0;

  SCM space_proc = me->get_elt_property ("space-function");
  SCM space = gh_call1 (space_proc, gh_int2scm (beam_multiplicity));

  Real thick = gh_scm2double (me->get_elt_property ("thickness")) ;
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
Beam::check_stem_length_f (Score_element*me,Real y, Real dy) 
{
  Real shorten = 0;
  Real lengthen = 0;
  Direction dir = Directional_element_interface::get (me);

  Link_array<Item> stems=
    Pointer_group_interface__extract_elements (me, (Item*)0, "stems");

  for (int i=0; i < stems.size(); i++)
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
Beam::set_stem_length (Score_element*me,Real y, Real dy)
{
  Real half_space = Staff_symbol_referencer::staff_space (me)/2;
  Link_array<Item> stems=
    Pointer_group_interface__extract_elements (me, (Item*)0, "stems");


  for (int i=0; i < stems.size (); i++)
    {
      Item* s = stems[i];
      if (Stem::invisible_b (s))
	continue;

      Real stem_y = calc_stem_y_f (me, s, y, dy);

      /* caution: stem measures in staff-positions */
      Stem::set_stemend (s,(stem_y + calc_interstaff_dist (s, dynamic_cast<Spanner*> (me))) / half_space);
    }
}

/*
  [Ross] (simplification of)
  Set dy complying with:
    - zero
    - thick / 2 + staffline_f / 2
    - thick + staffline_f
  + n * staff_space
*/
Real
Beam::quantise_dy_f (Score_element*me,Real dy) 
{
  Array<Real> a;

  SCM proc = me->get_elt_property ("height-quants");
  SCM quants = gh_call2 (proc, me->self_scm (),
			 gh_double2scm (me->paper_l ()->get_var ("stafflinethickness")
					/ 1.0));
  
  
  for (SCM s = quants; gh_pair_p (s); s = gh_cdr (s))
    a.push (gh_scm2double (gh_car (s)));
  
  if (a.size () <= 1)
    return dy;

  Real staff_space = Staff_symbol_referencer::staff_space (me);
  
  Interval iv = quantise_iv (a, abs (dy)/staff_space) * staff_space;
  Real q = (abs (dy) - iv[SMALLER] <= iv[BIGGER] - abs (dy))
    ? iv[SMALLER]
    : iv[BIGGER];
  
  return q * sign (dy);
}

/*
  Prevent interference from stafflines and beams.
  See Documentation/tex/fonts.doc

  We only need to quantise the (left) y-position of the beam,
  since dy is quantised too.
  if extend_b then stems must *not* get shorter
 */
Real
Beam::quantise_y_f (Score_element*me,Real y, Real dy, int quant_dir)
{
  int multiplicity = get_multiplicity (me);

  Real staff_space = Staff_symbol_referencer::staff_space (me);
  Real thick = me->paper_l ()->get_var ("stafflinethickness");


  SCM proc = me->get_elt_property ("vertical-position-quant-function");
  SCM quants = scm_apply (proc,
			  me->self_scm (),
			  gh_list (gh_int2scm (multiplicity),
				   gh_double2scm (dy/staff_space),
				   gh_double2scm (thick/staff_space),
				   SCM_EOL, SCM_UNDEFINED));
  
  Array<Real> a;

  for (; gh_pair_p (quants); quants = gh_cdr (quants))
    a.push (gh_scm2double (gh_car (quants)));

  if (a.size () <= 1)
    return y;

  Real up_y = Directional_element_interface::get (me) * y;
  Interval iv = quantise_iv (a, up_y/staff_space) * staff_space;

  Real q = up_y - iv[SMALLER] <= iv[BIGGER] - up_y 
    ? iv[SMALLER] : iv[BIGGER];
  if (quant_dir)
    q = iv[(Direction)quant_dir];

  return q * Directional_element_interface::get (me);
}

void
Beam::set_beaming (Score_element*me,Beaming_info_list *beaming)
{
  Link_array<Score_element> stems=
    Pointer_group_interface__extract_elements (me, (Score_element*)0, "stems");
  
  Direction d = LEFT;
  for (int i=0; i  < stems.size(); i++)
    {
      do
	{
	  if (Stem::beam_count (stems[i], d) == 0)
	    Stem::set_beaming ( stems[i], beaming->infos_.elem (i).beams_i_drul_[d],d);
	}
      while (flip (&d) != LEFT);
    }
}



/*
  beams to go with one stem.

  FIXME: clean me up.
  */
Molecule
Beam::stem_beams (Score_element*me,Item *here, Item *next, Item *prev) 
{
  // ugh -> use commonx
  if ((next && !(next->relative_coordinate (0, X_AXIS) > here->relative_coordinate (0, X_AXIS))) ||
      (prev && !(prev->relative_coordinate (0, X_AXIS) < here->relative_coordinate (0, X_AXIS))))
      programming_error ("Beams are not left-to-right");

  Real staffline_f = me->paper_l ()->get_var ("stafflinethickness");
  int multiplicity = get_multiplicity (me);

  SCM space_proc = me->get_elt_property ("space-function");
  SCM space = gh_call1 (space_proc, gh_int2scm (multiplicity));

  Real thick = gh_scm2double (me->get_elt_property ("thickness")) ;
  Real interbeam_f = gh_scm2double (space) ;
    
  Real bdy = interbeam_f;
  Real stemdx = staffline_f;

    // ugh -> use commonx
  Real dx = visible_stem_count (me) ?
    last_visible_stem (me)->relative_coordinate (0, X_AXIS) - first_visible_stem (me)->relative_coordinate (0, X_AXIS)
    : 0.0;
  Real dy = gh_scm2double (me->get_elt_property ("height"));
  Real dydx = dy && dx ? dy/dx : 0;

  Molecule leftbeams;
  Molecule rightbeams;

  Real nw_f;
  if (!Stem::first_head (here))
    nw_f = 0;
  else {
    int t = Stem::type_i (here); 

    SCM proc = me->get_elt_property ("flag-width-function");
    SCM result = gh_call1 (proc, gh_int2scm (t));
    nw_f = gh_scm2double (result);
  }


  Direction dir = Directional_element_interface::get (me);
  
  /* half beams extending to the left. */
  if (prev)
    {
      int lhalfs= lhalfs = Stem::beam_count (here,LEFT) - Stem::beam_count (prev,RIGHT);
      int lwholebeams= Stem::beam_count (here,LEFT) <? Stem::beam_count (prev,RIGHT) ;
      /*
       Half beam should be one note-width, 
       but let's make sure two half-beams never touch
       */
      Real w = here->relative_coordinate (0, X_AXIS) - prev->relative_coordinate (0, X_AXIS);
      w = w/2 <? nw_f;
      Molecule a;
      if (lhalfs)		// generates warnings if not
	a =  Lookup::beam (dydx, w, thick);
      a.translate (Offset (-w, -w * dydx));
      for (int j = 0; j  < lhalfs; j++)
	{
	  Molecule b (a);
	  b.translate_axis (-dir * bdy * (lwholebeams+j), Y_AXIS);
	  leftbeams.add_molecule (b);
	}
    }

  if (next)
    {
      int rhalfs  = Stem::beam_count (here,RIGHT) - Stem::beam_count (next,LEFT);
      int rwholebeams= Stem::beam_count (here,RIGHT) <? Stem::beam_count (next,LEFT) ;

      Real w = next->relative_coordinate (0, X_AXIS) - here->relative_coordinate (0, X_AXIS);
      Molecule a = Lookup::beam (dydx, w + stemdx, thick);
      a.translate_axis( - stemdx/2, X_AXIS);
      int j = 0;
      Real gap_f = 0;

      SCM gap = me->get_elt_property ("gap");
      if (gh_number_p (gap))
	{
	  int gap_i = gh_scm2int ( (gap));
	  int nogap = rwholebeams - gap_i;
	  
	  for (; j  < nogap; j++)
	    {
	      Molecule b (a);
	      b.translate_axis (-dir  * bdy * j, Y_AXIS);
	      rightbeams.add_molecule (b);
	    }
	  // TODO: notehead widths differ for different types
	  gap_f = nw_f / 2;
	  w -= 2 * gap_f;
	  a = Lookup::beam (dydx, w + stemdx, thick);
	}

      for (; j  < rwholebeams; j++)
	{
	  Molecule b (a);
	  b.translate (Offset (Stem::invisible_b (here) ? 0 : gap_f, -dir * bdy * j));
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

MAKE_SCHEME_CALLBACK(Beam,brew_molecule,1);
SCM
Beam::brew_molecule (SCM smob)
{
  Score_element * me =unsmob_element (smob);

  Molecule mol;
  if (!gh_pair_p (me->get_elt_property ("stems")))
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
      dx = stems.top()->relative_coordinate (0, X_AXIS) - x0;
    }
  
  
  Real dy = gh_scm2double (me->get_elt_property ("height"));
  Real dydx = dy && dx ? dy/dx : 0;
  Real y = gh_scm2double (me->get_elt_property ("y-position"));


  for (int j=0; j <stems.size  (); j++)
    {
      Item *i = stems[j];
      Item * prev = (j > 0)? stems[j-1] : 0;
      Item * next = (j < stems.size()-1) ? stems[j+1] :0;

      Molecule sb = stem_beams (me, i, next, prev);
      Real x = i->relative_coordinate (0, X_AXIS)-x0;
      sb.translate (Offset (x, x * dydx + y));
      mol.add_molecule (sb);
    }
  mol.translate_axis (x0 
    - dynamic_cast<Spanner*> (me)->get_bound (LEFT)->relative_coordinate (0, X_AXIS), X_AXIS);

  return mol.create_scheme ();
}

int
Beam::forced_stem_count (Score_element*me) 
{
  Link_array<Item>stems = 
    Pointer_group_interface__extract_elements ( me, (Item*) 0, "stems");
  int f = 0;
  for (int i=0; i < stems.size (); i++)
    {
      Item *s = stems[i];

      if (Stem::invisible_b (s))
	continue;

      if (((int)Stem::chord_start_f (s)) 
        && (Stem::get_direction (s ) != Stem::get_default_dir (s )))
        f++;
    }
  return f;
}




/* TODO:
   use filter and standard list functions.
 */
int
Beam::visible_stem_count (Score_element*me) 
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
Beam::first_visible_stem(Score_element*me) 
{
  Link_array<Item>stems = 
    Pointer_group_interface__extract_elements ( me, (Item*) 0, "stems");
  
  for (int i = 0; i < stems.size (); i++)
    {
      if (!Stem::invisible_b (stems[i]))
        return stems[i];
    }
  return 0;
}

Item*
Beam::last_visible_stem(Score_element*me) 
{
  Link_array<Item>stems = 
    Pointer_group_interface__extract_elements ( me, (Item*) 0, "stems");
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
MAKE_SCHEME_CALLBACK(Beam,rest_collision_callback,2);
SCM
Beam::rest_collision_callback (SCM element_smob, SCM axis)
{
  Score_element *rest = unsmob_element (element_smob);
  Axis a = (Axis) gh_scm2int (axis);
  
  assert (a == Y_AXIS);

  Score_element * st = unsmob_element (rest->get_elt_property ("stem"));
  Score_element * stem = st;
  if (!stem)
    return gh_double2scm (0.0);
  Score_element * beam = unsmob_element (stem->get_elt_property ("beam"));
  if (!beam || !Beam::has_interface (beam) || !Beam::visible_stem_count (beam))
    return gh_double2scm (0.0);

  // make callback for rest from this.
  Real beam_dy = 0;
  Real beam_y = 0;


  // todo: make sure this calced already.
  SCM s = beam->get_elt_property ("height");
  if (gh_number_p (s))
    beam_dy = gh_scm2double (s);
  
  s = beam->get_elt_property ("y-position");
  if (gh_number_p (s))
    beam_y = gh_scm2double (s);
  
  // ugh -> use commonx
  Real x0 = first_visible_stem(beam)->relative_coordinate (0, X_AXIS);
  Real dx = last_visible_stem(beam)->relative_coordinate (0, X_AXIS) - x0;
  Real dydx = beam_dy && dx ? beam_dy/dx : 0;

  Direction d = Stem::get_direction (stem);
  Real beamy = (stem->relative_coordinate (0, X_AXIS) - x0) * dydx + beam_y;

  Real staff_space =   Staff_symbol_referencer::staff_space (rest);

  
  Real rest_dim = rest->extent (rest, Y_AXIS)[d]*2.0 / staff_space ; // refp??

  Real minimum_dist
    = gh_scm2double (rest->get_elt_property ("minimum-beam-collision-distance"));
  Real dist =
    minimum_dist +  -d  * (beamy - rest_dim) >? 0;

  int stafflines = Staff_symbol_referencer::line_count (rest);

  // move discretely by half spaces.
  int discrete_dist = int (ceil (dist));

  // move by whole spaces inside the staff.
  if (discrete_dist < stafflines+1)
    discrete_dist = int (ceil (discrete_dist / 2.0)* 2.0);

  return gh_double2scm  (-d *  discrete_dist);
}


bool
Beam::has_interface (Score_element*me)
{
  return me->has_interface (ly_symbol2scm ("beam-interface"));
}

void
Beam::set_interface (Score_element*me)
{
  /*
    why the init? No way to tell difference between default and user
    override.  */
  me->set_elt_property ("height", gh_int2scm (0)); // ugh.
  me->set_elt_property ("y-position" ,gh_int2scm (0));
  me->set_interface (ly_symbol2scm("beam-interface"));
}
