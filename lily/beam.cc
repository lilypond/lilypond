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
    * redo grouping 

TODO:

The relationship Stem <-> Beam is way too hairy.  Let's figure who
needs what, and what information should be available when.

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

Beam::Beam ()
{
  slope_f_ = 0;
  left_y_ = 0;
  multiple_i_ = 0;
}

void
Beam::add_stem (Stem*s)
{
#if 0
  /*
    should figure out why this didn't work.

    --hwn.
   */
  if (!stems_.size ())
    {
      set_parent (s, Y_AXIS);
    }
#endif
  stems_.push (s);
  s->add_dependency (this);

  assert (!s->beam_l_);
  s->beam_l_ = this;

  if (!spanned_drul_[LEFT])
    set_bounds (LEFT,s);
  else
    set_bounds (RIGHT,s);
}

Stem_info
Beam::get_stem_info (Stem *s)
{
  Stem_info i;
  for (int i=0; i < sinfo_.size (); i++)
    {
      if (sinfo_[i].stem_l_ == s)
	return sinfo_[i];
    }
  assert (false);
  return i;
}

Molecule*
Beam::do_brew_molecule_p () const
{
  Molecule *mol_p = new Molecule;
  if (!sinfo_.size ())
    return mol_p;
  
  Real x0 = stems_[0]->hpos_f ();
  for (int j=0; j <stems_.size (); j++)
    {
      Stem *i = stems_[j];
      Stem * prev = (j > 0)? stems_[j-1] : 0;
      Stem * next = (j < stems_.size ()-1) ? stems_[j+1] :0;

      Molecule sb = stem_beams (i, next, prev);
      Real  x = i->hpos_f ()-x0;
      sb.translate (Offset (x, (x * slope_f_ + left_y_) *
			    i->staff_line_leading_f ()/2 ));
      mol_p->add_molecule (sb);
    }
  mol_p->translate_axis (x0 
    - spanned_drul_[LEFT]->relative_coordinate (0, X_AXIS), X_AXIS);

  return mol_p;
}

Offset
Beam::center () const
{
  Stem_info si = sinfo_[0];
  
  Real w= (si.stem_l_->note_delta_f () + extent (X_AXIS).length ())/2.0;
  return Offset (w, ( w* slope_f_) *
		 si.stem_l_->staff_line_leading_f ()/2);
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
  Real internote_f = stems_[0]->staff_line_leading_f ()/2;
  if (gap != SCM_UNDEFINED)
    {
      int auto_gap_i = gh_scm2int (gap);
      for (int i=1; i < stems_.size (); i++)
        {
	  bool is_b = (bool)(sinfo_[i].interstaff_f_ - sinfo_[i-1].interstaff_f_);
	  int l_y = (int)(stems_[i-1]->chord_start_f () / internote_f)
	    + (int)sinfo_[i-1].interstaff_f_;
	  int r_y = (int)(stems_[i]->chord_start_f () / internote_f)
	    + (int)sinfo_[i].interstaff_f_;
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
      for (int i=0; i < stems_.size (); i++)
        {
	  int y = (int)(stems_[i]->chord_start_f () / internote_f)
	    + (int)sinfo_[i].interstaff_f_;
	  stems_[i]->set_direction ( y < knee_y ? UP : DOWN);
	  stems_[i]->set_elt_property ("dir-forced", SCM_BOOL_T);
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
  if (stems_.size () < 2)
    {
      warning (_ ("beam with less than two stems"));
      set_elt_property ("transparent", SCM_BOOL_T);
      return;
    }
  set_steminfo ();
  if (auto_knees ())
    {
      /*
	if auto-knee did its work, most probably stem directions
	have changed, so we must recalculate all.
       */
      set_direction ( get_default_dir ());
      set_direction (get_direction ());

      /* auto-knees used to only work for slope = 0
	 anyway, should be able to set slope per beam
         set_elt_property ("damping", gh_int2scm(1000));
      */

      sinfo_.clear ();
      set_steminfo ();
    }
  calculate_slope ();
  set_stemlens ();
}

void
Beam::do_substitute_element_pointer (Score_element*o,Score_element*n)
{
  if (Stem * os = dynamic_cast<Stem*> (o))
    stems_.substitute (os,
		       dynamic_cast<Stem *> (n));
}

Interval
Beam::do_width () const
{
  return Interval (stems_[0]->hpos_f (),
		   stems_.top ()->hpos_f ());
}

Direction
Beam::get_default_dir () const
{
  Drul_array<int> total;
  total[UP]  = total[DOWN] = 0;
  Drul_array<int> count; 
  count[UP]  = count[DOWN] = 0;
  Direction d = DOWN;

  for (int i=0; i <stems_.size (); i++)
    do {
      Stem *s = stems_[i];
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
  
  if (a == gh_symbol2scm ("majority")) // should get default from paper.
    beam_dir = (count[UP] == count[DOWN]) ? neutral_dir 
      : (count[UP] > count[DOWN]) ? UP : DOWN;
  else if (a == gh_symbol2scm ("mean"))
    // mean center distance
    beam_dir = (total[UP] == total[DOWN]) ? neutral_dir
      : (total[UP] > total[DOWN]) ? UP : DOWN;
  else if (a == gh_symbol2scm ("median"))
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
  for (int i=0; i <stems_.size (); i++)
    {
      Stem *s = stems_[i];
      s->set_elt_property ("beam-dir", gh_int2scm (d));

      SCM force = s->remove_elt_property ("dir-forced");
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
  assert (sinfo_.size () > 1);

  Least_squares l;
  for (int i=0; i < sinfo_.size (); i++)
    {
      l.input.push (Offset (sinfo_[i].x_, sinfo_[i].idealy_f_));
    }
  l.minimise (slope_f_, left_y_);
}

/*
  ugh. Naming: this doesn't check, but sets as well.
 */
  
Real
Beam::check_stemlengths_f (bool set_b)
{
  Real interbeam_f = paper_l ()->interbeam_f (multiple_i_);

  Real beam_f = paper_l ()->get_var ("beam_thickness");;
  Real staffline_f = paper_l ()-> get_var ("stafflinethickness");
  Real epsilon_f = staffline_f / 8;
  Real dy_f = 0.0;
  for (int i=0; i < sinfo_.size (); i++)
    {
      Real y = sinfo_[i].x_ * slope_f_ + left_y_;

      // correct for knee
      if (get_direction () != sinfo_[i].get_direction ())
	{
	  Real internote_f = sinfo_[i].stem_l_->staff_line_leading_f ()/2;
	  y -= get_direction () * (beam_f / 2
		       + (sinfo_[i].mult_i_ - 1) * interbeam_f) / internote_f;
	  if (!i && sinfo_[i].stem_l_->staff_symbol_l () !=
	      sinfo_.top ().stem_l_->staff_symbol_l ())
	    y += get_direction () * (multiple_i_ - (sinfo_[i].stem_l_->flag_i_ - 2) >? 0)
	      * interbeam_f / internote_f;
	}

      if (set_b)
	sinfo_[i].stem_l_->set_stemend (y - sinfo_[i].interstaff_f_);
	
      y *= get_direction ();
      if (y > sinfo_[i].maxy_f_)
	dy_f = dy_f <? sinfo_[i].maxy_f_ - y;
      if (y < sinfo_[i].miny_f_)
	{ 
	  // when all too short, normal stems win..
	  if (dy_f < -epsilon_f)
	    warning (_ ("weird beam vertical offset"));
	  dy_f = dy_f >? sinfo_[i].miny_f_ - y; 
	}
    }
  return dy_f;
}

void
Beam::set_steminfo ()
{
  if(!stems_.size ())
    return;
  
  assert (multiple_i_);

  int total_count_i = 0;
  int forced_count_i = 0;
  for (int i=0; i < stems_.size (); i++)
    {
      Stem *s = stems_[i];

      s->set_default_extents ();
      if (s->invisible_b ())
	continue;
      if (((int)s->chord_start_f ()) && (s->get_direction () != s->get_default_dir ()))
        forced_count_i++;
      total_count_i++;
    }

  bool grace_b = get_elt_property ("grace") == SCM_BOOL_T;
  String type_str = grace_b ? "grace_" : "";
  int stem_max = (int)rint(paper_l ()->get_var ("stem_max"));
  Real shorten_f = paper_l ()->get_var (type_str + "forced_stem_shorten"
					+ to_str (multiple_i_ <? stem_max));
    
  Real leftx = 0;
  for (int i=0; i < stems_.size (); i++)
    {
      Stem *s = stems_[i];
      /*
	Chord tremolo needs to beam over invisible stems of wholes
      */
      SCM trem = get_elt_property ("chord-tremolo");
      if (gh_boolean_p (trem) && gh_scm2bool (trem))
	{
	  if (s->invisible_b ())
	    continue;
	}

      Stem_info info (s, multiple_i_);
      if (leftx == 0)
	leftx = info.x_;
      info.x_ -= leftx;
      if (info.get_direction () == get_direction ())
        {
	  if (forced_count_i == total_count_i)
	    info.idealy_f_ -= shorten_f;
	  else if (forced_count_i > total_count_i / 2)
	    info.idealy_f_ -= shorten_f / 2;
	}
      sinfo_.push (info);
    }
}

void
Beam::calculate_slope ()
{
  if (!sinfo_.size ())
    slope_f_ = left_y_ = 0;
  else if (sinfo_[0].idealy_f_ == sinfo_.top ().idealy_f_)
    {
      slope_f_ = 0;
      left_y_ = sinfo_[0].idealy_f_;
      left_y_ *= get_direction ();
    }
  else
    {
      solve_slope ();
      Real solved_slope_f = slope_f_;

      /*
	steep slope running against lengthened stem is suspect
      */
      Real dx_f = stems_.top ()->hpos_f () - stems_[0]->hpos_f ();

      // urg, these y internote-y-dimensions
      Real internote_f = stems_[0]->staff_line_leading_f ()/2;

      Real lengthened = paper_l ()->get_var ("beam_lengthened") / internote_f;
      Real steep = paper_l ()->get_var ("beam_steep_slope") / internote_f;
      if (((left_y_ - sinfo_[0].idealy_f_ > lengthened)
	   && (slope_f_ > steep))
	  || ((left_y_ + slope_f_ * dx_f - sinfo_.top ().idealy_f_ > lengthened)
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
  
  if (q == gh_symbol2scm ("none"))
    return;

  Real interline_f = stems_[0]->staff_line_leading_f ();
  Real internote_f = interline_f / 2;
  Real staffline_f = paper_l ()->get_var ("stafflinethickness");
  Real beam_f = paper_l ()->get_var ("beam_thickness");;

  Real dx_f = stems_.top ()->hpos_f () - stems_[0]->hpos_f ();

  // dim(y) = internote; so slope = (y/internote)/x
  Real dy_f = dx_f * abs (slope_f_ * internote_f);
  
  Real quanty_f = 0.0;

  Array<Real> allowed_fraction (3);
  allowed_fraction[0] = 0;
  allowed_fraction[1] = (beam_f / 2 + staffline_f / 2);
  allowed_fraction[2] = (beam_f + staffline_f);


  Interval iv = quantise_iv (allowed_fraction, interline_f, dy_f);
  quanty_f = (dy_f - iv[SMALLER] <= iv[BIGGER] - dy_f)
    ? iv[SMALLER]
    : iv[BIGGER];


  slope_f_ = (quanty_f / dx_f) / internote_f * sign (slope_f_);
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

  Real space = stems_[0]->staff_line_leading_f ();
  Real internote_f = space /2;
  Real staffline_f = paper_l ()->get_var ("stafflinethickness");
  Real beam_f = paper_l ()->get_var ("beam_thickness");;

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
  
  // dim(left_y_) = internote
  Real dy_f = get_direction () * left_y_ * internote_f;

  Real beamdx_f = stems_.top ()->hpos_f () - stems_[0]->hpos_f ();
  Real beamdy_f = beamdx_f * slope_f_ * internote_f;

  Array<Real> allowed_position;
  if (q == gh_symbol2scm ("normal"))
    {
      if ((multiple_i_ <= 2) || (abs (beamdy_f) >= staffline_f / 2))
	allowed_position.push (straddle);
      if ((multiple_i_ <= 1) || (abs (beamdy_f) >= staffline_f / 2))
	allowed_position.push (sit);
      allowed_position.push (hang);
    }
  else if (q == gh_symbol2scm ("traditional"))
    {
      // TODO: check and fix TRADITIONAL
      if ((multiple_i_ <= 2) || (abs (beamdy_f) >= staffline_f / 2))
	allowed_position.push (straddle);
      if ((multiple_i_ <= 1) && (beamdy_f <= staffline_f / 2))
	allowed_position.push (sit);
      if (beamdy_f >= -staffline_f / 2)
	allowed_position.push (hang);
    }


  Interval iv = quantise_iv (allowed_position, space, dy_f);

  Real quanty_f = dy_f - iv[SMALLER] <= iv[BIGGER] - dy_f ? iv[SMALLER] : iv[BIGGER];
  if (extend_b)
    quanty_f = iv[BIGGER];

  // dim(left_y_) = internote
  left_y_ = get_direction () * quanty_f / internote_f;
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
  for (int i=0; i  < stems_.size (); i++)
    {
      do
	{
	  if (stems_[i]->beams_i_drul_[d] < 0)
	    stems_[i]->beams_i_drul_[d] = beaming->infos_.elem (i).beams_i_drul_[d];
	}
      while (flip (&d) != LEFT);
    }
}


void
Beam::do_add_processing ()
{
  for (int i=0; i < stems_.size () ; i++) 
    {
      Direction d = LEFT;
      do {
	multiple_i_ = multiple_i_ >? stems_[i]->beams_i_drul_[d];
      } while ((flip (&d)) != LEFT);
    }

  if (stems_.size ())
    {
      stems_[0]->beams_i_drul_[LEFT] =0;
      stems_.top()->beams_i_drul_[RIGHT] =0;
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
  Real interbeam_f = paper_l ()->interbeam_f (multiple_i_);

  Real internote_f = here->staff_line_leading_f ()/2;
  Real beam_f = paper_l ()->get_var ("beam_thickness");;

  Real dy = interbeam_f;
  Real stemdx = staffline_f;
  Real sl = slope_f_* internote_f;

  Molecule leftbeams;
  Molecule rightbeams;

  // UGH
  Real nw_f;
  if (!here->head_l_arr_.size ())
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


