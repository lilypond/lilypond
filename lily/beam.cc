/*
  beam.cc -- implement Beam

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
    Jan Nieuwenhuizen <janneke@gnu.org>

*/

/*
  [TODO]
    * less hairy code
    * move paper vars to scm

*/

#include "beaming.hh"
#include "dimensions.hh"
#include "beam.hh"
#include "misc.hh"
#include "debug.hh"
#include "leastsquares.hh"
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
}

void
Beam::add_stem (Stem*s)
{
  Group_interface gi (this, "stems");
  gi.add_element (s);
  
  s->add_dependency (this);

  assert (!s->beam_l ());
  s->set_elt_property ("beam", self_scm_);

  if (!spanned_drul_[LEFT])
    set_bounds (LEFT,s);
  else
    set_bounds (RIGHT,s);
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
Beam::do_pre_processing ()
{
  // Why?
  if (visible_stem_count () < 2)
    {
      warning (_ ("beam has less than two stems"));
      set_elt_property ("transparent", SCM_BOOL_T);
    }

  if (!get_direction ())
    set_direction (get_default_dir ());

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
      int current = s->get_direction () 
	? (1 + d * s->get_direction ())/2
	: s->get_center_distance ((Direction)-d);

      if (current)
	{
	  total[d] += current;
	  count[d] ++;
	}

    } while (flip(&d) != DOWN);
  
  /* 
     [Ross] states that the majority of the notes dictates the
     direction (and not the mean of "center distance")

     But is that because it really looks better, or because he wants
     to provide some real simple hands-on rules?
     
     We have our doubts, so we simply provide all sensible alternatives.

     If dir is not determined: up (see stem::get_default_dir ()) */

  Direction beam_dir = CENTER;
  Direction neutral_dir = (Direction)(int)paper_l ()->get_var ("stem_default_neutral_direction");

  SCM a = get_elt_property ("beam-dir-algorithm");
  
  if (a == ly_symbol2scm ("majority")) // should get default from paper.
    beam_dir = (count[UP] == count[DOWN]) ? neutral_dir 
      : (count[UP] > count[DOWN]) ? UP : DOWN;
  else if (a == ly_symbol2scm ("mean"))
    // mean center distance
    beam_dir = (total[UP] == total[DOWN]) ? neutral_dir
      : (total[UP] > total[DOWN]) ? UP : DOWN;
  else if (a == ly_symbol2scm ("median"))
    {
      // median center distance
      if (count[DOWN] && count[UP])
	{
	  beam_dir = (total[UP] / count[UP] == total[DOWN] / count[DOWN]) 
	    ? neutral_dir 
	    : (total[UP] / count[UP] > total[DOWN] / count[DOWN]) ? UP : DOWN;
	}
      else
	{
	  beam_dir = (count[UP] == count[DOWN]) ? neutral_dir 
	    : (count[UP] > count[DOWN]) ? UP : DOWN;
	}
    }
  
  return beam_dir;
}


/*
  Set all stems with non-forced direction to beam direction.
  Urg: non-forced should become `without/with unforced' direction,
       once stem gets cleaned-up.
 */
void
Beam::set_stem_directions ()
{
  Direction d = get_direction ();
  for (int i=0; i <stem_count (); i++)
    {
      Stem *s = stem (i);
      SCM force = s->remove_elt_property ("dir-forced");
      if (!gh_boolean_p (force) || !gh_scm2bool (force))
	s->set_direction (d);
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
  if (gh_number_p (gap))
    {
      int auto_gap_i = gh_scm2int (gap);
      for (int i=1; i < stem_count (); i++)
        {
	  bool is_b = (bool)(calc_interstaff_dist (stem (i), this) 
	    - calc_interstaff_dist (stem (i-1), this));
	  int l_y = (int)(stem (i-1)->chord_start_f ())
	    + (int)calc_interstaff_dist (stem (i-1), this);
	  int r_y = (int)(stem (i)->chord_start_f ())
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
	  int y = (int)(stem (i)->chord_start_f ())
	    + (int)calc_interstaff_dist (stem (i), this);
	  stem (i)->set_direction (y < knee_y ? UP : DOWN);
	  stem (i)->set_elt_property ("dir-forced", SCM_BOOL_T);
	}
    }
  return knee_b;
}

/*
 Set stem's shorten property if unset.
 TODO: take some y-position (nearest?) into account
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
  SCM shorten = scm_eval (scm_listify (
    ly_symbol2scm ("beamed-stem-shorten"),
    gh_int2scm (multiplicity), 
    SCM_UNDEFINED));
  Real shorten_f = gh_scm2double (shorten) 
    * Staff_symbol_referencer_interface (this).staff_space ();

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
Beam::do_post_processing ()
{
  /* first, calculate y, dy */
  Real y, dy;
  calc_position_and_height (&y, &dy);
  if (suspect_slope_b (y, dy))
    dy = 0;

  Real damped_dy = calc_slope_damping_f (dy);
  Real quantised_dy = quantise_dy_f (damped_dy);

  y += (dy - quantised_dy) / 2;
  dy = quantised_dy;
  
  /*
    until here, we used only stem_info, which acts as if dir=up
   */
  y *= get_direction ();
  dy *= get_direction ();

  /* set or read dy as necessary */
  SCM s = get_elt_property ("height");
  if (gh_number_p (s))
    dy = gh_scm2double (s);
  else
    set_elt_property ("height", gh_double2scm (dy));

  /* set or read y as necessary */
  s = get_elt_property ("y-position");
  if (gh_number_p (s))
    {
      y = gh_scm2double (s);
      set_stem_length (y, dy);
    }
  else
    { 
      /* we can modify y, so we should quantise y */
      Real y_shift = check_stem_length_f (y, dy);
      y += y_shift;
      y = quantise_y_f (y, dy, 0);
      set_stem_length (y, dy);
      y_shift = check_stem_length_f (y, dy);

      Real internote_f = paper_l ()->get_var ("interline") / 2;
      if (y_shift > internote_f / 4)
	{
	  y += y_shift;

	  /*
	    for significantly lengthened or shortened stems,
	    request quanting the other way.
	  */
	  int quant_dir = 0;
	  if (abs (y_shift) > internote_f / 2)
	    quant_dir = sign (y_shift) * get_direction ();
	  y = quantise_y_f (y, dy, quant_dir);
	  set_stem_length (y, dy);
	}

      set_elt_property ("y-position", gh_double2scm (y));
    }
}

/*
  See Documentation/tex/fonts.doc
 */
void
Beam::calc_position_and_height (Real* y, Real* dy) const
{
  *y = *dy = 0;
  if (visible_stem_count () <= 1)
    return;

  Real first_ideal = first_visible_stem ()->calc_stem_info ().idealy_f_;
  if (first_ideal == last_visible_stem ()->calc_stem_info ().idealy_f_)
    {
      *dy = 0;
      *y = first_ideal;
      return;
    }

  Least_squares ls;
  Real x0 = first_visible_stem ()->hpos_f ();
  for (int i=0; i < stem_count (); i++)
    {
      Stem* s = stem (i);
      if (s->invisible_b ())
        continue;
      ls.input.push (Offset (s->hpos_f () - x0, 
        s->calc_stem_info ().idealy_f_));
    }
  Real dydx;
  ls.minimise (dydx, *y); // duh, takes references

  Real dx = last_visible_stem ()->hpos_f () - x0;
  *dy = dydx * dx;
}

bool
Beam::suspect_slope_b (Real y, Real dy) const
{
  /*
    steep slope running against lengthened stem is suspect
  */
  Real first_ideal = first_visible_stem ()->calc_stem_info ().idealy_f_;
  Real last_ideal = last_visible_stem ()->calc_stem_info ().idealy_f_;
  Real lengthened = paper_l ()->get_var ("beam_lengthened");
  Real steep = paper_l ()->get_var ("beam_steep_slope");

  Real dx = last_visible_stem ()->hpos_f () - first_visible_stem ()->hpos_f ();
  Real dydx = dy/dx;

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
      Real dx = last_visible_stem ()->hpos_f ()
	- first_visible_stem ()->hpos_f ();
      Real dydx = dy/dx;
      dydx = 0.6 * tanh (dydx) / damping;
      return dydx * dx;
    }
  return dy;
}

Real
Beam::calc_stem_y_f (Stem* s, Real y, Real dy) const
{
  Real beam_f = gh_scm2double (get_elt_property ("beam-thickness"));
  int   multiplicity = get_multiplicity ();


  Real interbeam_f = paper_l ()->interbeam_f (multiplicity);
  Real x0 = first_visible_stem ()->hpos_f ();
  Real dx = last_visible_stem ()->hpos_f () - x0;
  Real stem_y = (s->hpos_f () - x0) / dx * dy + y;

  /* knee */
  if (get_direction () != s->get_direction ())
    {
      stem_y -= get_direction () * (beam_f / 2
	+ (multiplicity - 1) * interbeam_f);

      Staff_symbol_referencer_interface me (s);
      Staff_symbol_referencer_interface last (last_visible_stem ());
      
      if ((s != first_visible_stem ())
	  && me.staff_symbol_l () != last.staff_symbol_l ())
	stem_y += get_direction () 
	          * (multiplicity - (s->flag_i () - 2) >? 0) * interbeam_f;
    }
  return stem_y;
}

Real
Beam::check_stem_length_f (Real y, Real dy) const
{
  Real shorten = 0;
  Real lengthen = 0;
  for (int i=0; i < stem_count (); i++)
    {
      Stem* s = stem (i);
      if (s->invisible_b ())
	continue;

      Real stem_y = calc_stem_y_f (s, y, dy);
	
      stem_y *= get_direction ();
      Stem_info info = s->calc_stem_info ();

      if (stem_y > info.maxy_f_)
	shorten = shorten <? info.maxy_f_ - stem_y;

      if (stem_y < info.miny_f_)
        lengthen = lengthen >? info.miny_f_ - stem_y; 
    }

  if (lengthen && shorten)
    warning (_ ("weird beam vertical offset"));

  /* when all stems are too short, normal stems win */
  if (shorten)
    return shorten * get_direction ();
  else
    return lengthen * get_direction ();
}

void
Beam::set_stem_length (Real y, Real dy)
{
  Real internote_f = paper_l ()->get_var ("interline") / 2;
  for (int i=0; i < stem_count (); i++)
    {
      Stem* s = stem (i);
      if (s->invisible_b ())
	continue;

      Real stem_y = calc_stem_y_f (s, y, dy);

      /* caution: stem measures in staff-positions */
      s->set_stemend ((stem_y - calc_interstaff_dist (s, this)) / internote_f);
    }
}

/*
  [Ross] (simplification of)
  Try to set dy complying with:
    - zero
    - beam_f / 2 + staffline_f / 2
    - beam_f + staffline_f
  + n * interline

  TODO: get allowed-positions as scm list (aarg: from paper block)
*/
Real
Beam::quantise_dy_f (Real dy) const
{
  SCM s = get_elt_property ("slope-quantisation");
  
  if (s == ly_symbol2scm ("none"))
    return dy;

  Staff_symbol_referencer_interface st (this);
  Real interline_f = st.staff_space ();
  
  Real staffline_f = paper_l ()->get_var ("stafflinethickness");
  Real beam_f = gh_scm2double (get_elt_property ("beam-thickness"));;

  Array<Real> allowed_fraction (3);
  allowed_fraction[0] = 0;
  allowed_fraction[1] = (beam_f / 2 + staffline_f / 2);
  allowed_fraction[2] = (beam_f + staffline_f);

  allowed_fraction.push (interline_f);
  Interval iv = quantise_iv (allowed_fraction,  abs (dy));
  Real q = (abs (dy) - iv[SMALLER] <= iv[BIGGER] - abs (dy))
    ? iv[SMALLER]
    : iv[BIGGER];

  return q * sign (dy);
}

/*
  Prevent interference from stafflines and beams.
  See Documentation/tex/fonts.doc

  TODO: get allowed-positions as scm list (aarg: from paper block)
 */
Real
Beam::quantise_y_f (Real y, Real dy, int quant_dir)
{
   /*
    We only need to quantise the (left) y-position of the beam,
    since dy is quantised too.
    if extend_b then stems must *not* get shorter
   */
  SCM s = get_elt_property ("slope-quantisation");
  if (s == ly_symbol2scm ("none"))
    return y;

  /*
    ----------------------------------------------------------
                                                   ########
	                                ########
                             ########
    --------------########------------------------------------
       ########

       hang       straddle   sit        inter      hang
   */

  Staff_symbol_referencer_interface sinf (this);
  Real space = sinf.staff_space ();
  Real staffline_f = paper_l ()->get_var ("stafflinethickness");
  Real beam_f = gh_scm2double (get_elt_property ("beam-thickness"));;

  Real straddle = 0;
  Real sit = beam_f / 2 - staffline_f / 2;
  Real hang = space - beam_f / 2 + staffline_f / 2;

  /*
    Put all allowed positions into an array.
    Whether a position is allowed or not depends on 
    strictness of quantisation, multiplicity and direction.

    For simplicity, we'll assume dir = UP and correct if 
    dir = DOWN afterwards.
   */
  
  int multiplicity = get_multiplicity ();


  Array<Real> allowed_position;
  if (s == ly_symbol2scm ("normal"))
    {
      if ((multiplicity <= 2) || (abs (dy) >= staffline_f / 2))
	allowed_position.push (straddle);
      if ((multiplicity <= 1) || (abs (dy) >= staffline_f / 2))
	allowed_position.push (sit);
      allowed_position.push (hang);
    }
  else if (s == ly_symbol2scm ("traditional"))
    {
      // TODO: check and fix TRADITIONAL
      if ((multiplicity <= 2) || (abs (dy) >= staffline_f / 2))
	allowed_position.push (straddle);
      if ((multiplicity <= 1) && (dy <= staffline_f / 2))
	allowed_position.push (sit);
      if (dy >= -staffline_f / 2)
	allowed_position.push (hang);
    }

  allowed_position.push (space);
  Real up_y = get_direction () * y;
  Interval iv = quantise_iv (allowed_position, up_y);

  Real q = up_y - iv[SMALLER] <= iv[BIGGER] - up_y 
    ? iv[SMALLER] : iv[BIGGER];
  if (quant_dir)
    q = iv[(Direction)quant_dir];

  return q * get_direction ();
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
  if ((next && !(next->hpos_f () > here->hpos_f ())) ||
      (prev && !(prev->hpos_f () < here->hpos_f ())))
      programming_error ("Beams are not left-to-right");

  Real staffline_f = paper_l ()->get_var ("stafflinethickness");
  int   multiplicity = get_multiplicity ();


  Real interbeam_f = paper_l ()->interbeam_f (multiplicity);
  Real beam_f = gh_scm2double (get_elt_property ("beam-thickness"));;

  Real dy = interbeam_f;
  Real stemdx = staffline_f;

  Real dx = last_visible_stem ()->hpos_f () - first_visible_stem ()->hpos_f ();
  Real dydx = get_real ("height")/dx;

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

  /* half beams extending to the left. */
  if (prev)
    {
      int lhalfs= lhalfs = here->beam_count (LEFT) - prev->beam_count (RIGHT);
      int lwholebeams= here->beam_count (LEFT) <? prev->beam_count (RIGHT) ;
      /*
       Half beam should be one note-width, 
       but let's make sure two half-beams never touch
       */
      Real w = here->hpos_f () - prev->hpos_f ();
      w = w/2 <? nw_f;
      Molecule a;
      if (lhalfs)		// generates warnings if not
	a =  lookup_l ()->beam (dydx, w, beam_f);
      a.translate (Offset (-w, -w * dydx));
      for (int j = 0; j  < lhalfs; j++)
	{
	  Molecule b (a);
	  b.translate_axis (-get_direction () * dy * (lwholebeams+j), Y_AXIS);
	  leftbeams.add_molecule (b);
	}
    }

  if (next)
    {
      int rhalfs  = here->beam_count (RIGHT) - next->beam_count (LEFT);
      int rwholebeams= here->beam_count (RIGHT) <? next->beam_count (LEFT) ;

      Real w = next->hpos_f () - here->hpos_f ();
      Molecule a = lookup_l ()->beam (dydx, w + stemdx, beam_f);
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
	      b.translate_axis (-get_direction () * dy * j, Y_AXIS);
	      rightbeams.add_molecule (b);
	    }
	  // TODO: notehead widths differ for different types
	  gap_f = nw_f / 2;
	  w -= 2 * gap_f;
	  a = lookup_l ()->beam (dydx, w + stemdx, beam_f);
	}

      for (; j  < rwholebeams; j++)
	{
	  Molecule b (a);
	  if (!here->invisible_b ())
	    b.translate (Offset (gap_f, -get_direction () * dy * j));
	  else
	    b.translate (Offset (0, -get_direction () * dy * j));
	  rightbeams.add_molecule (b);
	}

      w = w/2 <? nw_f;
      if (rhalfs)
	a = lookup_l ()->beam (dydx, w, beam_f);

      for (; j  < rwholebeams + rhalfs; j++)
	{
	  Molecule b (a);
	  b.translate_axis (-get_direction () * dy * j, Y_AXIS);
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


Molecule*
Beam::do_brew_molecule_p () const
{
  Molecule *mol_p = new Molecule;
  if (!stem_count ())
    return mol_p;
  
  Real x0 = first_visible_stem ()->hpos_f ();
  Real dx = last_visible_stem ()->hpos_f () - x0;
  Real dydx = get_real ("height")/dx;
  Real y = get_real ("y-position");
  for (int j=0; j <stem_count (); j++)
    {
      Stem *i = stem (j);
      Stem * prev = (j > 0)? stem (j-1) : 0;
      Stem * next = (j < stem_count ()-1) ? stem (j+1) :0;

      Molecule sb = stem_beams (i, next, prev);
      Real x = i->hpos_f ()-x0;
      sb.translate (Offset (x, x * dydx + y));
      mol_p->add_molecule (sb);
    }
  mol_p->translate_axis (x0 
    - spanned_drul_[LEFT]->relative_coordinate (0, X_AXIS), X_AXIS);

  return mol_p;
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
    
  //Group_interface__extract_elements ((Beam*) this, (Stem*) 0, "stems")[stem_count () - 1];
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

  assert (0);

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

  assert (0);
  // sigh
  return 0;
}
