/*
  beam.cc -- implement Beam

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
    Jan Nieuwenhuizen <janneke@gnu.org>

*/


/*
  [TODO]
    * center beam symbol
    * less hairy code

    */

#include <math.h>

#include "beaming.hh"
#include "proto.hh"
#include "dimensions.hh"
#include "beam.hh"
#include "misc.hh"
#include "debug.hh"
#include "molecule.hh"
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
  
  slope_f_ = 0;
  left_y_ = 0;
  multiplicity_i_ = 0;
}

/*
  TODO: Fix this class. This is wildly inefficient.
 */
Stem *
Beam::stem (int i)const
{
  return Group_interface__extract_elements ((Beam*) this, (Stem*) 0, "stems")[i];
}

int
Beam::stem_count ()const
{
  Group_interface gi (this, "stems");
  return gi.count ();
}

Stem*
Beam::stem_top ()const
{
  return Group_interface__extract_elements ((Beam*) this, (Stem*) 0, "stems")[stem_count () - 1];
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
  // sigh
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

Molecule*
Beam::do_brew_molecule_p () const
{
  Molecule *mol_p = new Molecule;
  if (!stem_count ())
    return mol_p;
  
  Real x0 = first_visible_stem ()->hpos_f ();
  for (int j=0; j <stem_count (); j++)
    {
      Stem *i = stem (j);
      Stem * prev = (j > 0)? stem (j-1) : 0;
      Stem * next = (j < stem_count ()-1) ? stem (j+1) :0;

      Molecule sb = stem_beams (i, next, prev);
      Real  x = i->hpos_f ()-x0;
      sb.translate (Offset (x, x * slope_f_ + left_y_));
      mol_p->add_molecule (sb);
    }
  mol_p->translate_axis (x0 
    - spanned_drul_[LEFT]->relative_coordinate (0, X_AXIS), X_AXIS);

  return mol_p;
}

Offset
Beam::center () const
{
  Real w = (first_visible_stem ()->note_delta_f () + extent (X_AXIS).length ())/2.0;
  return Offset (w, w * slope_f_);
}

/*
  Simplistic auto-knees; only consider vertical gap between two
  adjacent chords
 */
bool
Beam::auto_knee (SCM gap, bool interstaff_b)
{
  bool knee = false;
  int knee_y = 0;
  if (gap != SCM_UNDEFINED)
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

	  /*
	    Forced stem directions are ignored.  If you don't want auto-knees,
	    don't set, or unset autoKneeGap/autoInterstaffKneeGap.
	   */
	  if ((abs (gap_i) >= auto_gap_i) && (!interstaff_b || is_b))
	    {
	      knee_y = (r_y + l_y) / 2;
	      knee = true;
	      break;
	    }
	}
    }
  if (knee)
    {
      for (int i=0; i < stem_count (); i++)
        {
	  int y = (int)(stem (i)->chord_start_f ())
	    + (int)calc_interstaff_dist (stem (i), this);
	  stem (i)->set_direction ( y < knee_y ? UP : DOWN);
	  stem (i)->set_elt_property ("dir-forced", SCM_BOOL_T);
	}
    }
  return knee;
}

bool
Beam::auto_knees ()
{
  if (auto_knee (get_elt_property ("auto-interstaff-knee-gap"), true))
    return true;
  
  return auto_knee (get_elt_property ("auto-knee-gap"), false);
}


void
Beam::do_pre_processing ()
{
  /*
    urg: it seems that info on whether beam (voice) dir was forced
         is being junked here?
  */
  if (!get_direction ())
    set_direction ( get_default_dir ());
  
  set_direction (get_direction ());
}

void
Beam::do_print () const
{
#ifndef NPRINT
  DEBUG_OUT << "slope_f_ " << slope_f_ << "left ypos " << left_y_;
  Spanner::do_print ();
#endif
}

void
Beam::do_post_processing ()
{
  if (visible_stem_count () < 2)
    {
      warning (_ ("beam with less than two stems"));
      set_elt_property ("transparent", SCM_BOOL_T);
      return;
    }
  set_stem_shorten ();
  if (auto_knees ())
    {
      /*
	if auto-knee did its work, most probably stem directions
	have changed, so we must recalculate all.
       */
      set_direction (get_default_dir ());
      set_direction (get_direction ());

      /* auto-knees used to only work for slope = 0
	 anyway, should be able to set slope per beam
         set_elt_property ("damping", gh_int2scm(1000));
      */

      set_stem_shorten ();
    }
  calculate_slope ();
  set_stemlens ();
}




Direction
Beam::get_default_dir () const
{
  Drul_array<int> total;
  total[UP]  = total[DOWN] = 0;
  Drul_array<int> count; 
  count[UP]  = count[DOWN] = 0;
  Direction d = DOWN;

  for (int i=0; i <stem_count (); i++)
    do {
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

void
Beam::set_direction (Direction d)
{
  Directional_spanner::set_direction (d);
  for (int i=0; i <stem_count (); i++)
    {
      Stem *s = stem (i);
      s->set_elt_property ("beam-dir", gh_int2scm (d));

      SCM force = s->get_elt_property ("dir-forced"); // remove_prop?
      if (force == SCM_UNDEFINED)
	s->set_direction ( d);
    }
}

/*
  See Documentation/tex/fonts.doc
 */

void
Beam::solve_slope ()
{
  assert (visible_stem_count () > 1);

  Least_squares l;
  Real x0 = first_visible_stem ()->hpos_f ();
  for (int i=0; i < stem_count (); i++)
    {
      Stem* s = stem (i);
      if (s->invisible_b ())
        continue;
      l.input.push (Offset (s->hpos_f () - x0, s->calc_stem_info ().idealy_f_));
    }
  l.minimise (slope_f_, left_y_);
}

/*
  ugh. Naming: this doesn't check, but sets as well.
 */
Real
Beam::check_stemlengths_f (bool set_b)
{
  Real interbeam_f = paper_l ()->interbeam_f (multiplicity_i_);

  Real beam_f = gh_scm2double (get_elt_property ("beam-thickness"));
  Real staffline_f = paper_l ()-> get_var ("stafflinethickness");
  Real epsilon_f = staffline_f / 8;
  Real dy_f = 0.0;
  Real x0 = first_visible_stem ()->hpos_f ();
  Real internote_f = paper_l ()->get_var ("interline");
  for (int i=0; i < stem_count (); i++)
    {
      Stem* s = stem (i);
      if (s->invisible_b ())
	continue;
      Real y = (s->hpos_f () - x0) * slope_f_ + left_y_;
      Stem_info info = s->calc_stem_info ();

      // correct for knee
      if (get_direction () != s->get_direction ())
	{
	  y -= get_direction () * (beam_f / 2
	    + (multiplicity_i_ - 1) * interbeam_f);


	  Staff_symbol_referencer_interface s1 (s);
	  Staff_symbol_referencer_interface s2 (stem_top ());
	  
	  if (!i
	    && s1.staff_symbol_l () != s2.staff_symbol_l ())
	    y += get_direction () * (multiplicity_i_ - (s->flag_i () - 2) >? 0)
	      * interbeam_f;
	}

      /* caution: stem measures in staff-positions */
      if (set_b)
	s->set_stemend ((y - calc_interstaff_dist (s, this))
			       / internote_f);
	
      y *= get_direction ();
      if (y > info.maxy_f_)
	dy_f = dy_f <? info.maxy_f_ - y;
      if (y < info.miny_f_)
	{ 
	  // when all too short, normal stems win..
	  if (dy_f < -epsilon_f)
	    warning (_ ("weird beam vertical offset"));
	  dy_f = dy_f >? info.miny_f_ - y; 
	}
    }
  return dy_f;
}

void
Beam::set_stem_shorten ()
{
  if(!stem_count ())
    return;
  
  assert (multiplicity_i_);

  int total_count_i = 0;
  int forced_count_i = 0;
  for (int i=0; i < stem_count (); i++)
    {
      Stem *s = stem (i);

      s->set_default_extents ();
      if (s->invisible_b ())
	continue;
      if (((int)s->chord_start_f ()) && (s->get_direction () != s->get_default_dir ()))
        forced_count_i++;
      total_count_i++;
    }

  Real internote_f = paper_l ()->get_var ("interline");
  bool grace_b = get_elt_property ("grace") == SCM_BOOL_T;
  String type_str = grace_b ? "grace_" : "";
  int stem_max = (int)rint(paper_l ()->get_var ("stem_max"));
  Real shorten_f = paper_l ()->get_var (type_str + "forced_stem_shorten"
    + to_str (multiplicity_i_ <? stem_max)) * internote_f;
    
  for (int i=0; i < stem_count (); i++)
    {
      Stem *s = stem (i);
      /*
	Chord tremolo needs to beam over invisible stems of wholes
      */
      SCM trem = get_elt_property ("chord-tremolo");
      if (gh_boolean_p (trem) && gh_scm2bool (trem))
	{
	  if (s->invisible_b ())
	    continue;
	}

      if (s->get_direction () == get_direction ())
        {
	  if (forced_count_i == total_count_i)
	    s->set_real ("shorten", shorten_f);
	  else if (forced_count_i > total_count_i / 2)
	    s->set_real ("shorten", shorten_f/2);
	}
    }
}

void
Beam::calculate_slope ()
{
  if (!stem_count ())
    slope_f_ = left_y_ = 0;
  else if (first_visible_stem ()->calc_stem_info ().idealy_f_ == last_visible_stem ()->calc_stem_info ().idealy_f_)
    {
      slope_f_ = 0;
      left_y_ = first_visible_stem ()->calc_stem_info ().idealy_f_;
      left_y_ *= get_direction ();
    }
  else
    {
      solve_slope ();
      Real solved_slope_f = slope_f_;

      /*
	steep slope running against lengthened stem is suspect
      */
      Real dx_f = stem (stem_count () -1)->hpos_f () - first_visible_stem ()->hpos_f ();

      Real lengthened = paper_l ()->get_var ("beam_lengthened");
      Real steep = paper_l ()->get_var ("beam_steep_slope");
      if (((left_y_ - first_visible_stem ()->calc_stem_info ().idealy_f_ > lengthened)
	   && (slope_f_ > steep))
	  || ((left_y_ + slope_f_ * dx_f - last_visible_stem ()->calc_stem_info ().idealy_f_ > lengthened)
	      && (slope_f_ < -steep)))
	{
	  slope_f_ = 0;
	}

      /*
	This neat trick is by Werner Lemberg,
	damped = tanh (slope_f_)
	corresponds with some tables in [Wanske]
      */
      SCM damp = remove_elt_property ("damping");
      int damping = 1;		// ugh.
      if (damp!= SCM_UNDEFINED)
	damping = gh_int2scm (damp);

      if (damping)
	slope_f_ = 0.6 * tanh (slope_f_) / damping;
      
      quantise_dy ();

      Real damped_slope_dy_f = (solved_slope_f - slope_f_) * dx_f / 2;
      left_y_ += damped_slope_dy_f;

      left_y_ *= get_direction ();
      slope_f_ *= get_direction ();
    }
}

void
Beam::quantise_dy ()
{
  /*
    [Ross] (simplification of)
    Try to set slope_f_ complying with y-span of:
      - zero
      - beam_f / 2 + staffline_f / 2
      - beam_f + staffline_f
    + n * interline
    */

  SCM q = get_elt_property ("slope-quantisation");
  
  if (q == ly_symbol2scm ("none"))
    return;

  Staff_symbol_referencer_interface st (this);
  Real interline_f = st.staff_line_leading_f ();
  
  Real staffline_f = paper_l ()->get_var ("stafflinethickness");
  Real beam_f = gh_scm2double (get_elt_property ("beam-thickness"));;

  Real dx_f = stem (stem_count () -1 )->hpos_f () - first_visible_stem ()->hpos_f ();

  Real dy_f = dx_f * abs (slope_f_);
  
  Real quanty_f = 0.0;

  Array<Real> allowed_fraction (3);
  allowed_fraction[0] = 0;
  allowed_fraction[1] = (beam_f / 2 + staffline_f / 2);
  allowed_fraction[2] = (beam_f + staffline_f);

  Interval iv = quantise_iv (allowed_fraction, interline_f, dy_f);
  quanty_f = (dy_f - iv[SMALLER] <= iv[BIGGER] - dy_f)
    ? iv[SMALLER]
    : iv[BIGGER];

  slope_f_ = (quanty_f / dx_f) * sign (slope_f_);
}

/*
  
  Prevent interference from stafflines and beams.  See Documentation/tex/fonts.doc
  
 */
void
Beam::quantise_left_y (bool extend_b)
{
   /*
    we only need to quantise the start of the beam as dy is quantised too
   if extend_b then stems must *not* get shorter
   */
  SCM q = get_elt_property ("slope-quantisation");


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
  Real space = sinf.staff_line_leading_f ();
  Real staffline_f = paper_l ()->get_var ("stafflinethickness");
  Real beam_f = gh_scm2double (get_elt_property ("beam-thickness"));;

  /*
    [TODO]
    it would be nice to have all allowed positions in a runtime matrix:
    (multiplicity, minimum_beam_dy, maximum_beam_dy)
   */

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
  // isn't this asymmetric ? --hwn
  
  Real dy_f = get_direction () * left_y_;

  Real beamdx_f = stem (stem_count () -1)->hpos_f () - first_visible_stem ()->hpos_f ();
  Real beamdy_f = beamdx_f * slope_f_;

  Array<Real> allowed_position;
  if (q == ly_symbol2scm ("normal"))
    {
      if ((multiplicity_i_ <= 2) || (abs (beamdy_f) >= staffline_f / 2))
	allowed_position.push (straddle);
      if ((multiplicity_i_ <= 1) || (abs (beamdy_f) >= staffline_f / 2))
	allowed_position.push (sit);
      allowed_position.push (hang);
    }
  else if (q == ly_symbol2scm ("traditional"))
    {
      // TODO: check and fix TRADITIONAL
      if ((multiplicity_i_ <= 2) || (abs (beamdy_f) >= staffline_f / 2))
	allowed_position.push (straddle);
      if ((multiplicity_i_ <= 1) && (beamdy_f <= staffline_f / 2))
	allowed_position.push (sit);
      if (beamdy_f >= -staffline_f / 2)
	allowed_position.push (hang);
    }


  Interval iv = quantise_iv (allowed_position, space, dy_f);

  Real quanty_f = dy_f - iv[SMALLER] <= iv[BIGGER] - dy_f ? iv[SMALLER] : iv[BIGGER];
  if (extend_b)
    quanty_f = iv[BIGGER];

  left_y_ = get_direction () * quanty_f;
}

void
Beam::set_stemlens ()
{
  Real staffline_f = paper_l ()->get_var ("stafflinethickness");
  // enge floots
  Real epsilon_f = staffline_f / 8;


  // je bent zelf eng --hwn.
  Real dy_f = check_stemlengths_f (false);
  for (int i = 0; i < 2; i++)	// 2 ?
    { 
      left_y_ += dy_f * get_direction ();
      quantise_left_y (dy_f);
      dy_f = check_stemlengths_f (true);
      if (abs (dy_f) <= epsilon_f)
        {
	  break;
	}
    }
}

void
Beam::set_beaming (Beaming_info_list *beaming)
{
  Direction d = LEFT;
  for (int i=0; i  < stem_count (); i++)
    {
      do
	{
	  if (stem (i)->beams_i_drul_[d] < 0)
	    stem (i)->beams_i_drul_[d] = beaming->infos_.elem (i).beams_i_drul_[d];
	}
      while (flip (&d) != LEFT);
    }
}


void
Beam::do_add_processing ()
{
  for (int i=0; i < stem_count () ; i++) 
    {
      Direction d = LEFT;
      do {
	multiplicity_i_ = multiplicity_i_ >? stem (i)->beams_i_drul_[d];
      } while ((flip (&d)) != LEFT);
    }

}



/*
  beams to go with one stem.

  clean  me up.
  */
Molecule
Beam::stem_beams (Stem *here, Stem *next, Stem *prev) const
{
  if ((next && !(next->hpos_f () > here->hpos_f ())) ||
      (prev && !(prev->hpos_f () < here->hpos_f ())))
      programming_error ("Beams are not left-to-right");

  Real staffline_f = paper_l ()->get_var ("stafflinethickness");
  Real interbeam_f = paper_l ()->interbeam_f (multiplicity_i_);
  Real beam_f = gh_scm2double (get_elt_property ("beam-thickness"));;

  Real dy = interbeam_f;
  Real stemdx = staffline_f;
  Real sl = slope_f_;

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
      int lhalfs= lhalfs = here->beams_i_drul_[LEFT] - prev->beams_i_drul_[RIGHT] ;
      int lwholebeams= here->beams_i_drul_[LEFT] <? prev->beams_i_drul_[RIGHT] ;
      /*
       Half beam should be one note-width, 
       but let's make sure two half-beams never touch
       */
      Real w = here->hpos_f () - prev->hpos_f ();
      w = w/2 <? nw_f;
      Molecule a;
      if (lhalfs)		// generates warnings if not
	a =  lookup_l ()->beam (sl, w, beam_f);
      a.translate (Offset (-w, -w * sl));
      for (int j = 0; j  < lhalfs; j++)
	{
	  Molecule b (a);
	  b.translate_axis (-get_direction () * dy * (lwholebeams+j), Y_AXIS);
	  leftbeams.add_molecule (b);
	}
    }

  if (next)
    {
      int rhalfs = here->beams_i_drul_[RIGHT] - next->beams_i_drul_[LEFT];
      int rwholebeams = here->beams_i_drul_[RIGHT] <? next->beams_i_drul_[LEFT];

      Real w = next->hpos_f () - here->hpos_f ();
      Molecule a = lookup_l ()->beam (sl, w + stemdx, beam_f);
      a.translate_axis( - stemdx/2, X_AXIS);
      int j = 0;
      Real gap_f = 0;

      SCM gap = get_elt_property ("beam-gap");
      if (gap != SCM_UNDEFINED)
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
	  a = lookup_l ()->beam (sl, w + stemdx, beam_f);
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
	a = lookup_l ()->beam (sl, w, beam_f);

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


