/*
  beam.cc -- implement Beam

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
    Jan Nieuwenhuizen <janneke@gnu.org>

*/

/*
  [TODO]
    * less hairy code
    * move paper vars to scm


    remove *-hs variables.
    
*/


#include <math.h>		// tanh.
#include "directional-element-interface.hh"
#include "beaming.hh"
#include "dimensions.hh"
#include "beam.hh"
#include "misc.hh"
#include "debug.hh"
#include "least-squares.hh"
#include "stem.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "group-interface.hh"
#include "staff-symbol-referencer.hh"
#include "cross-staff.hh"

Beam::Beam ()
{
  Group_interface g (this, "stems");
  g.set_interface ();

  set_elt_property ("height", gh_int2scm (0)); // ugh.
  set_elt_property ("y-position" ,gh_int2scm (0));
}

void
Beam::add_stem (Stem*s)
{
  Group_interface gi (this, "stems");
  gi.add_element (s);
  
  s->add_dependency (this);

  assert (!s->beam_l ());
  s->set_elt_property ("beam", self_scm_);

  if (!get_bound (LEFT))
    set_bound (LEFT,s);
  else
    set_bound (RIGHT,s);
}

int
Beam::get_multiplicity () const
{
  int m = 0;
  for (SCM s = get_elt_property ("stems"); gh_pair_p (s); s = gh_cdr (s))
    {
      Score_element * sc = unsmob_element (gh_car (s));

      if (Stem * st = dynamic_cast<Stem*> (sc))
	m = m >? st->beam_count (LEFT) >? st->beam_count (RIGHT);
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
void
Beam::before_line_breaking ()
{
  // Why?
  if (visible_stem_count () < 2)
    {
      warning (_ ("beam has less than two stems"));
      //      set_elt_property ("transparent", SCM_BOOL_T);
    }

  if (!directional_element (this).get ())
    directional_element (this).set (get_default_dir ());

  auto_knees ();
  set_stem_directions ();
  set_stem_shorten (); 
}

/*
 FIXME
 */
Direction
Beam::get_default_dir () const
{
  Drul_array<int> total;
  total[UP]  = total[DOWN] = 0;
  Drul_array<int> count; 
  count[UP]  = count[DOWN] = 0;
  Direction d = DOWN;

  for (int i=0; i <stem_count (); i++)
    do { // HUH -- waar slaat dit op?
      Stem *s = stem (i);
      Direction sd = directional_element (s).get ();
      int current = sd	? (1 + d * sd)/2
	: s->get_center_distance ((Direction)-d);

      if (current)
	{
	  total[d] += current;
	  count[d] ++;
	}

    } while (flip(&d) != DOWN);
  

  SCM s = scm_eval (gh_list (ly_symbol2scm ("beam-dir-algorithm"),
			     ly_quote_scm (gh_cons (gh_int2scm (count[UP]),
						    gh_int2scm (count[DOWN]))),
			     ly_quote_scm (gh_cons (gh_int2scm (total[UP]),
						    gh_int2scm (total[DOWN]))),
			     SCM_UNDEFINED));
  if (gh_number_p (s) && gh_scm2int (s))
    return to_dir (s);
  
  /*
    If dir is not determined: get from paper
  */
  return (Direction)(int)
    paper_l ()->get_var ("stem_default_neutral_direction");
}


/*
  Set all stems with non-forced direction to beam direction.
  Urg: non-forced should become `without/with unforced' direction,
       once stem gets cleaned-up.
 */
void
Beam::set_stem_directions ()
{
  Direction d = directional_element (this).get ();
  for (int i=0; i <stem_count (); i++)
    {
      Stem *s = stem (i);
      SCM force = s->remove_elt_property ("dir-forced");
      if (!gh_boolean_p (force) || !gh_scm2bool (force))
	directional_element (s).set (d);
    }
} 

void
Beam::auto_knees ()
{
  if (!auto_knee ("auto-interstaff-knee-gap", true))
    auto_knee ("auto-knee-gap", false);
}

/*
  Simplistic auto-knees; only consider vertical gap between two
  adjacent chords.

  `Forced' stem directions are ignored.  If you don't want auto-knees,
  don't set, or unset autoKneeGap/autoInterstaffKneeGap.
 */
bool
Beam::auto_knee (String gap_str, bool interstaff_b)
{
  bool knee_b = false;
  int knee_y = 0;
  SCM gap = get_elt_property (gap_str);
  Direction d = directional_element (this).get ();
  
  if (gh_number_p (gap))
    {
      int auto_gap_i = gh_scm2int (gap);
      for (int i=1; i < stem_count (); i++)
        {
	  bool is_b = (bool)(calc_interstaff_dist (stem (i), this) 
	    - calc_interstaff_dist (stem (i-1), this));
	  int l_y = (int)(stem (i-1)->head_positions()[d])
	    + (int)calc_interstaff_dist (stem (i-1), this);
	  int r_y = (int)(stem (i)->head_positions()[d])
	    + (int)calc_interstaff_dist (stem (i), this);
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
      for (int i=0; i < stem_count (); i++)
        {
	  int y = (int)(stem (i)->head_positions()[d])
	    + (int)calc_interstaff_dist (stem (i), this);
	  directional_element (stem (i)).set (y < knee_y ? UP : DOWN);
	  stem (i)->set_elt_property ("dir-forced", SCM_BOOL_T);
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
Beam::set_stem_shorten ()
{
  if (!visible_stem_count ())
    return;

  Real forced_fraction = forced_stem_count () / visible_stem_count ();
  if (forced_fraction < 0.5)
    return;

  int multiplicity = get_multiplicity ();

  // grace stems?
  SCM shorten = scm_eval (ly_symbol2scm ("beamed-stem-shorten"));

  if (shorten == SCM_EOL)
    return;

  int sz = scm_ilength (shorten);
  
  Staff_symbol_referencer_interface st (this);
  Real staff_space = st.staff_space ();
  SCM shorten_elt = scm_list_ref (shorten, gh_int2scm (multiplicity <? (sz - 1)));
  Real shorten_f = gh_scm2double (shorten_elt) * staff_space;

  /* cute, but who invented this -- how to customise ? */
  if (forced_fraction < 1)
    shorten_f /= 2;

  for (int i=0; i < stem_count (); i++)
    {
      Stem* s = stem (i);
      if (s->invisible_b ())
        continue;
      if (gh_number_p (s->get_elt_property ("shorten")))
	s->set_elt_property ("shorten", gh_double2scm (shorten_f));
    }
}

/*
  Set elt properties height and y-position if not set.
  Adjust stem lengths to reach beam.
 */
void
Beam::after_line_breaking ()
{
  /* first, calculate y, dy */
  Real y, dy;
  calc_default_position_and_height (&y, &dy);
  if (visible_stem_count ())
    {
      if (suspect_slope_b (y, dy))
	dy = 0;

      Real damped_dy = calc_slope_damping_f (dy);
      Real quantised_dy = quantise_dy_f (damped_dy);

      y += (dy - quantised_dy) / 2;
      dy = quantised_dy;
    }
  /*
    until here, we used only stem_info, which acts as if dir=up
   */
  y *= directional_element (this).get ();
  dy *= directional_element (this).get ();

  Staff_symbol_referencer_interface st (this);
  Real half_space = st.staff_space () / 2;

  /* check for user-override of dy */
  SCM s = remove_elt_property ("height-hs");
  if (gh_number_p (s))
    {
      dy = gh_scm2double (s) * half_space;
    }
  set_elt_property ("height", gh_double2scm (dy));

  /* check for user-override of y */
  s = remove_elt_property ("y-position-hs");
  if (gh_number_p (s))
    {
      y = gh_scm2double (s) * half_space;
    }
  else
    { 
      /* we can modify y, so we should quantise y */
      Real y_shift = check_stem_length_f (y, dy);
      y += y_shift;
      y = quantise_y_f (y, dy, 0);
      set_stem_length (y, dy);
      y_shift = check_stem_length_f (y, dy);

      if (y_shift > half_space / 4)
	{
	  y += y_shift;

	  /*
	    for significantly lengthened or shortened stems,
	    request quanting the other way.
	  */
	  int quant_dir = 0;
	  if (abs (y_shift) > half_space / 2)
	    quant_dir = sign (y_shift) * directional_element (this).get ();
	  y = quantise_y_f (y, dy, quant_dir);
	}
    }
  // UGH. Y is not in staff position unit?
  // Ik dacht datwe daar juist van weg wilden?
  set_stem_length (y, dy);
  set_elt_property ("y-position", gh_double2scm (y)); 
}

/*
  See Documentation/tex/fonts.doc
 */
void
Beam::calc_default_position_and_height (Real* y, Real* dy) const
{
  *y = 0;
  *dy = 0;  
  if (visible_stem_count () <= 1)
    return;

  Real first_ideal = first_visible_stem ()->calc_stem_info ().idealy_f_;
  if (first_ideal == last_visible_stem ()->calc_stem_info ().idealy_f_)
    {
      *dy = 0;
      *y = first_ideal;
      return;
    }

  Array<Offset> ideals;
  Real x0 = first_visible_stem ()->relative_coordinate (0, X_AXIS);
  for (int i=0; i < stem_count (); i++)
    {
      Stem* s = stem (i);
      if (s->invisible_b ())
        continue;
      ideals.push (Offset (s->relative_coordinate (0, X_AXIS) - x0, 
			   s->calc_stem_info ().idealy_f_));
    }
  Real dydx;
  minimise_least_squares (&dydx, y, ideals); // duh, takes references

  Real dx = last_visible_stem ()->relative_coordinate (0, X_AXIS) - x0;
  *dy = dydx * dx;
}

bool
Beam::suspect_slope_b (Real y, Real dy) const
{
  /* first, calculate y, dy */
  /*
    steep slope running against lengthened stem is suspect
  */
  Real first_ideal = first_visible_stem ()->calc_stem_info ().idealy_f_;
  Real last_ideal = last_visible_stem ()->calc_stem_info ().idealy_f_;
  Real lengthened = paper_l ()->get_var ("beam_lengthened");
  Real steep = paper_l ()->get_var ("beam_steep_slope");

  Real dx = last_visible_stem ()->relative_coordinate (0, X_AXIS) - first_visible_stem ()->relative_coordinate (0, X_AXIS);
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
Beam::calc_slope_damping_f (Real dy) const
{
  SCM damp = get_elt_property ("damping"); // remove?
  int damping = 1;		// ugh.
  if (gh_number_p (damp))
    damping = gh_scm2int (damp);

  if (damping)
    {
      Real dx = last_visible_stem ()->relative_coordinate (0, X_AXIS)
	- first_visible_stem ()->relative_coordinate (0, X_AXIS);
      Real dydx = dy && dx ? dy/dx : 0;
      dydx = 0.6 * tanh (dydx) / damping;
      return dydx * dx;
    }
  return dy;
}

Real
Beam::calc_stem_y_f (Stem* s, Real y, Real dy) const
{
  Real thick = gh_scm2double (get_elt_property ("beam-thickness"));
  int beam_multiplicity = get_multiplicity ();
  int stem_multiplicity = (s->flag_i () - 2) >? 0;

  Real interbeam_f = paper_l ()->interbeam_f (beam_multiplicity);
  Real x0 = first_visible_stem ()->relative_coordinate (0, X_AXIS);
  Real dx = last_visible_stem ()->relative_coordinate (0, X_AXIS) - x0;
  Real stem_y = (dy && dx ? (s->relative_coordinate (0, X_AXIS) - x0) / dx * dy : 0) + y;

  /* knee */
   Direction dir  = directional_element(this).get ();
   Direction sdir = directional_element (s).get ();
   
    /* knee */
   if (dir!= sdir)
      {
       stem_y -= dir 
	* (thick / 2 + (beam_multiplicity - 1) * interbeam_f);

      Staff_symbol_referencer_interface me (s);
      Staff_symbol_referencer_interface last (last_visible_stem ());
      
      // huh, why not for first visible?
      if (//(s != first_visible_stem ()) &&
	  me.staff_symbol_l () != last.staff_symbol_l ())
	stem_y += directional_element (this).get ()
	  * (beam_multiplicity - stem_multiplicity) * interbeam_f;
    }
  return stem_y;
}

Real
Beam::check_stem_length_f (Real y, Real dy) const
{
  Real shorten = 0;
  Real lengthen = 0;
  Direction dir = directional_element (this).get ();
  
  for (int i=0; i < stem_count (); i++)
    {
      Stem* s = stem (i);
      if (s->invisible_b ())
	continue;

      Real stem_y = calc_stem_y_f (s, y, dy);
	
      stem_y *= dir;
      Stem_info info = s->calc_stem_info ();

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
Beam::set_stem_length (Real y, Real dy)
{
  Staff_symbol_referencer_interface st (this);
  Real half_space = st.staff_space ()/2;
  for (int i=0; i < stem_count (); i++)
    {
      Stem* s = stem (i);
      if (s->invisible_b ())
	continue;

      Real stem_y = calc_stem_y_f (s, y, dy);

      /* caution: stem measures in staff-positions */
      s->set_stemend ((stem_y + calc_interstaff_dist (s, this)) / half_space);
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
Beam::quantise_dy_f (Real dy) const
{
  Array<Real> a;
  for (SCM s = scm_eval (ly_symbol2scm ("beam-height-quants")); s !=SCM_EOL; s = gh_cdr (s))
    a.push (gh_scm2double (gh_car (s)));
  
  if (a.size () <= 1)
    return dy;

  Staff_symbol_referencer_interface st (this);
  Real staff_space = st.staff_space ();
  
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
Beam::quantise_y_f (Real y, Real dy, int quant_dir)
{
  int multiplicity = get_multiplicity ();
  Staff_symbol_referencer_interface st (this);
  Real staff_space = st.staff_space ();
  SCM quants = scm_eval (gh_list (ly_symbol2scm ("beam-vertical-position-quants"),
				  gh_int2scm (multiplicity),
				  gh_double2scm (dy/staff_space),
				  SCM_UNDEFINED));

  Array<Real> a;

  for (; quants != SCM_EOL; quants = gh_cdr (quants))
    a.push (gh_scm2double (gh_car (quants)));

  if (a.size () <= 1)
    return y;

  Real up_y = directional_element (this).get () * y;
  Interval iv = quantise_iv (a, up_y/staff_space) * staff_space;

  Real q = up_y - iv[SMALLER] <= iv[BIGGER] - up_y 
    ? iv[SMALLER] : iv[BIGGER];
  if (quant_dir)
    q = iv[(Direction)quant_dir];

  return q * directional_element (this).get ();
}

void
Beam::set_beaming (Beaming_info_list *beaming)
{
  Direction d = LEFT;
  for (int i=0; i  < stem_count (); i++)
    {
      do
	{
	  if (stem (i)->beam_count (d) == 0)
	    stem (i)->set_beaming ( beaming->infos_.elem (i).beams_i_drul_[d],d);
	}
      while (flip (&d) != LEFT);
    }
}



/*
  beams to go with one stem.

  BURP
  clean  me up.
  */
Molecule
Beam::stem_beams (Stem *here, Stem *next, Stem *prev) const
{
  if ((next && !(next->relative_coordinate (0, X_AXIS) > here->relative_coordinate (0, X_AXIS))) ||
      (prev && !(prev->relative_coordinate (0, X_AXIS) < here->relative_coordinate (0, X_AXIS))))
      programming_error ("Beams are not left-to-right");

  Real staffline_f = paper_l ()->get_var ("stafflinethickness");
  int   multiplicity = get_multiplicity ();


  Real interbeam_f = paper_l ()->interbeam_f (multiplicity);
  Real thick = gh_scm2double (get_elt_property ("beam-thickness"));

  Real bdy = interbeam_f;
  Real stemdx = staffline_f;

  Real dx = visible_stem_count () ?
    last_visible_stem ()->relative_coordinate (0, X_AXIS) - first_visible_stem ()->relative_coordinate (0, X_AXIS)
    : 0.0;
  Real dy = gh_scm2double (get_elt_property ("height"));
  Real dydx = dy && dx ? dy/dx : 0;

  Molecule leftbeams;
  Molecule rightbeams;

  // UGH
  Real nw_f;
  if (!here->first_head ())
    nw_f = 0;
  else if (here->type_i ()== 1)
    nw_f = paper_l ()->get_var ("wholewidth");
  else if (here->type_i () == 2)
    nw_f = paper_l ()->get_var ("notewidth") * 0.8;
  else
    nw_f = paper_l ()->get_var ("quartwidth");


  Direction dir = directional_element (this).get ();
  
  /* half beams extending to the left. */
  if (prev)
    {
      int lhalfs= lhalfs = here->beam_count (LEFT) - prev->beam_count (RIGHT);
      int lwholebeams= here->beam_count (LEFT) <? prev->beam_count (RIGHT) ;
      /*
       Half beam should be one note-width, 
       but let's make sure two half-beams never touch
       */
      Real w = here->relative_coordinate (0, X_AXIS) - prev->relative_coordinate (0, X_AXIS);
      w = w/2 <? nw_f;
      Molecule a;
      if (lhalfs)		// generates warnings if not
	a =  lookup_l ()->beam (dydx, w, thick);
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
      int rhalfs  = here->beam_count (RIGHT) - next->beam_count (LEFT);
      int rwholebeams= here->beam_count (RIGHT) <? next->beam_count (LEFT) ;

      Real w = next->relative_coordinate (0, X_AXIS) - here->relative_coordinate (0, X_AXIS);
      Molecule a = lookup_l ()->beam (dydx, w + stemdx, thick);
      a.translate_axis( - stemdx/2, X_AXIS);
      int j = 0;
      Real gap_f = 0;

      SCM gap = get_elt_property ("beam-gap");
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
	  a = lookup_l ()->beam (dydx, w + stemdx, thick);
	}

      for (; j  < rwholebeams; j++)
	{
	  Molecule b (a);
	  b.translate (Offset (here->invisible_b () ? 0 : gap_f, -dir * bdy * j));
	  rightbeams.add_molecule (b);
	}

      w = w/2 <? nw_f;
      if (rhalfs)
	a = lookup_l ()->beam (dydx, w, thick);

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


Molecule 
Beam::do_brew_molecule () const
{
  Molecule mol;
  if (!stem_count ())
    return mol;
  Real x0,dx;
  if (visible_stem_count ())
    {
      x0 = first_visible_stem ()->relative_coordinate (0, X_AXIS);
      dx = last_visible_stem ()->relative_coordinate (0, X_AXIS) - x0;
    }
  else
    {
      x0 = stem (0)->relative_coordinate (0, X_AXIS);
      dx = stem_top ()->relative_coordinate (0, X_AXIS) - x0;
    }
  
  
  Real dy = gh_scm2double (get_elt_property ("height"));
  Real dydx = dy && dx ? dy/dx : 0;
  Real y = gh_scm2double (get_elt_property ("y-position"));
  for (int j=0; j <stem_count (); j++)
    {
      Stem *i = stem (j);
      Stem * prev = (j > 0)? stem (j-1) : 0;
      Stem * next = (j < stem_count ()-1) ? stem (j+1) :0;

      Molecule sb = stem_beams (i, next, prev);
      Real x = i->relative_coordinate (0, X_AXIS)-x0;
      sb.translate (Offset (x, x * dydx + y));
      mol.add_molecule (sb);
    }
  mol.translate_axis (x0 
    - get_bound (LEFT)->relative_coordinate (0, X_AXIS), X_AXIS);

  return mol;
}

int
Beam::forced_stem_count () const
{
  int f = 0;
  for (int i=0; i < stem_count (); i++)
    {
      Stem *s = stem (i);

      if (s->invisible_b ())
	continue;

      if (((int)s->chord_start_f ()) 
        && (s->get_direction () != s->get_default_dir ()))
        f++;
    }
  return f;
}

/*
  TODO: Fix this class. This is wildly inefficient.
  And it sux.  Yet another array/list 'interface'.
 */
Stem *
Beam::stem (int i) const
{
  return Group_interface__extract_elements ((Beam*) this, (Stem*) 0, "stems")[i];
}

int
Beam::stem_count () const
{
  Group_interface gi (this, "stems");
  return gi.count ();
}

Stem*
Beam::stem_top () const
{
  SCM s = get_elt_property ("stems");
  
  return gh_pair_p (s) ? dynamic_cast<Stem*> (unsmob_element (gh_car (s))) : 0;
}

/* burp */
int
Beam::visible_stem_count () const
{
  int c = 0;
  for (int i = 0; i < stem_count (); i++)
    {
      if (!stem (i)->invisible_b ())
        c++;
    }
  return c;
}

Stem*
Beam::first_visible_stem () const
{
  for (int i = 0; i < stem_count (); i++)
    {
      Stem* s = stem (i);
      if (!s->invisible_b ())
        return s;
    }
  return 0;
}

Stem*
Beam::last_visible_stem () const
{
  for (int i = stem_count (); i > 0; i--)
    {
      Stem* s = stem (i - 1);
      if (!s->invisible_b ())
        return s;
    }
  return 0;
}
