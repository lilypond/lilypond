/*
  beam.cc -- implement Beam

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999, 1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
    Jan Nieuwenhuizen <janneke@gnu.org>

*/


/*
  [TODO]
    * centre beam symbol
    * less hairy code
    * redo grouping 
 */

#include <math.h>

#include "p-col.hh"
#include "array.hh"
#include "proto.hh"
#include "dimensions.hh"
#include "beam.hh"
#include "abbreviation-beam.hh"
#include "misc.hh"
#include "debug.hh"
#include "molecule.hh"
#include "leastsquares.hh"
#include "stem.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "rhythmic-grouping.hh"

Beam::Beam ()
{
  slope_f_ = 0;
  left_y_ = 0;
  quantisation_ = NORMAL;
  multiple_i_ = 0;
  vertical_align_drul_[MIN] = 0;
  vertical_align_drul_[MAX] = -1;
}

void
Beam::add_stem (Stem*s)
{
  stems_.push (s);
  s->add_dependency (this);
  s->beam_l_ = this;

  if (!spanned_drul_[LEFT])
    set_bounds (LEFT,s);
  else
    set_bounds (RIGHT,s);
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
    - spanned_drul_[LEFT]->absolute_coordinate (X_AXIS), X_AXIS);

  // correct if last note (and therefore reference point of beam)
  // is on different staff
  Stem_info si =   sinfo_.top ();
  mol_p->translate_axis (-si.interstaff_f_ * si.stem_l_->staff_line_leading_f ()/2,
			 Y_AXIS);

  return mol_p;
}

Offset
Beam::center () const
{
  Stem_info si = sinfo_[0];
  
  Real w= (si.stem_l_->note_delta_f () + extent (X_AXIS).length ())/2.0;
  return Offset (w, (left_y_ + w* slope_f_) *
		 si.stem_l_->staff_line_leading_f ()/2);
}

void
Beam::do_pre_processing ()
{
  if (!dir_)
    set_default_dir ();
}

void
Beam::do_print () const
{
#ifndef NPRINT
  DOUT << "slope_f_ " << slope_f_ << "left ypos " << left_y_;
  Spanner::do_print ();
#endif
}

void
Beam::do_post_processing ()
{
  if (stems_.size () < 2)
    {
      warning (_ ("beam with less than two stems"));
      set_elt_property (transparent_scm_sym, SCM_BOOL_T);
      return ;
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

void
Beam::set_default_dir ()
{
  Drul_array<int> total;
  total[UP]  = total[DOWN] = 0;
  Drul_array<int> count; 
  count[UP]  = count[DOWN] = 0;
  Direction d = DOWN;
  
  for (int i=0; i <stems_.size (); i++)
    do {
      Stem *s = stems_[i];
      int current = s->dir_ 
	? (1 + d * s->dir_)/2
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

     But is that because it really looks better, or because he
     wants to provide some real simple hands-on rules.
     
     We have our doubts, so we simply provide all sensible alternatives.
  */

  Dir_algorithm a = (Dir_algorithm)rint(paper_l ()->get_var ("beam_dir_algorithm"));
  switch (a)
    {
    case MAJORITY:
      dir_ = (count[UP] > count[DOWN]) ? UP : DOWN;
      break;
    case MEAN:
      // mean centre distance
      dir_ = (total[UP] > total[DOWN]) ? UP : DOWN;
      break;
    default:
    case MEDIAN:
      // median centre distance
      if (!count[UP])
	dir_ = DOWN;
      else if (!count[DOWN])
	dir_ = UP;
      else
	dir_ = (total[UP] / count[UP] > total[DOWN] / count[DOWN]) ? UP : DOWN;
      break;
    }

  for (int i=0; i <stems_.size (); i++)
    {
      Stem *s = stems_[i];
      s->set_elt_property (beam_dir_scm_sym, gh_int2scm (dir_));

      SCM force = s->remove_elt_property (dir_forced_scm_sym);
      if (force == SCM_BOOL_F)
	s->dir_ = dir_;
    }
}

/*
  See Documentation/tex/fonts.doc
 */

void
Beam::solve_slope ()
{
  /*
    should use minimum energy formulation (cf linespacing)
  */
  assert (sinfo_.size () > 1);
  DOUT << "Beam::solve_slope: \n";

  Least_squares l;
  for (int i=0; i < sinfo_.size (); i++)
    {
      l.input.push (Offset (sinfo_[i].x_, sinfo_[i].idealy_f_));
    }
  l.minimise (slope_f_, left_y_);
}

Real
Beam::check_stemlengths_f (bool set_b)
{
  Real interbeam_f = paper_l ()->interbeam_f (multiple_i_);

  Real beam_f = paper_l ()->beam_thickness_f ();
  Real staffline_f = paper_l ()->rule_thickness ();
  Real epsilon_f = staffline_f / 8;
  Real dy_f = 0.0;
  for (int i=0; i < sinfo_.size (); i++)
    {
      Real y = sinfo_[i].x_ * slope_f_ + left_y_;

      // correct for knee
      if (dir_ != sinfo_[i].dir_)
	{
	  Real internote_f = sinfo_[i].stem_l_->staff_line_leading_f ()/2;
	  y -= dir_ * (beam_f / 2
		       + (sinfo_[i].mult_i_ - 1) * interbeam_f) / internote_f;
	  if (!i && sinfo_[i].stem_l_->staff_symbol_l () !=
	      sinfo_.top ().stem_l_->staff_symbol_l ())
	    y += dir_ * (multiple_i_ - (sinfo_[i].stem_l_->flag_i_ - 2) >? 0)
	      * interbeam_f / internote_f;
	}

      if (set_b)
	sinfo_[i].stem_l_->set_stemend (y - sinfo_[i].interstaff_f_);
	
      y *= dir_;
      if (y > sinfo_[i].maxy_f_)
	dy_f = dy_f <? sinfo_[i].maxy_f_ - y;
      if (y < sinfo_[i].miny_f_)
	{ 
	  // when all too short, normal stems win..
	  if (dy_f < -epsilon_f)
	    warning (_ ("weird beam shift, check your knees"));
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
      s->mult_i_ = multiple_i_;
      s->set_default_extents ();
      if (s->invisible_b ())
	continue;
      if (((int)s->chord_start_f ()) && (s->dir_ != s->get_default_dir ()))
        forced_count_i++;
      total_count_i++;
    }

  Real internote_f = stems_[0]->staff_line_leading_f ()/2;
  int stem_max = (int)rint(paper_l ()->get_var ("stem_max"));
  Real shorten_f = paper_l ()->get_var (String ("forced_stem_shorten"
					      + to_str (multiple_i_ <? stem_max)))
    / internote_f;
    
  Real leftx = 0;
  for (int i=0; i < stems_.size (); i++)
    {
      Stem *s = stems_[i];
      if (s->invisible_b ())
	continue;

      Stem_info info (s);
      if (leftx == 0)
	leftx = info.x_;
      info.x_ -= leftx;
      if (info.dir_ == dir_)
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
  set_steminfo ();
  if (!sinfo_.size ())
    slope_f_ = left_y_ = 0;
  else if (sinfo_[0].idealy_f_ == sinfo_.top ().idealy_f_)
    {
      slope_f_ = 0;
      left_y_ = sinfo_[0].idealy_f_;
      left_y_ *= dir_;
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
      SCM damp = remove_elt_property (damping_scm_sym);
      int damping = 1;		// ugh.
      if (damp!= SCM_BOOL_F)
	damping = gh_int2scm (SCM_CDR(damp));

      if (damping)
	slope_f_ = 0.6 * tanh (slope_f_) / damping;
      
      quantise_dy ();

      Real damped_slope_dy_f = (solved_slope_f - slope_f_) * dx_f / 2;
      left_y_ += damped_slope_dy_f;

      left_y_ *= dir_;
      slope_f_ *= dir_;
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

  if (quantisation_ <= NONE)
    return;

  Real interline_f = stems_[0]->staff_line_leading_f ();
  Real internote_f = interline_f / 2;
  Real staffline_f = paper_l ()->rule_thickness ();
  Real beam_f = paper_l ()->beam_thickness_f ();

  Real dx_f = stems_.top ()->hpos_f () - stems_[0]->hpos_f ();

  // dim(y) = internote; so slope = (y/internote)/x
  Real dy_f = dx_f * abs (slope_f_ * internote_f);
  
  Real quanty_f = 0.0;

  /* UGR.   ICE in 2.8.1; bugreport filed. */
  Array<Real> allowed_fraction (3);
  allowed_fraction[0] = 0;
  allowed_fraction[1] = (beam_f / 2 + staffline_f / 2);
  allowed_fraction[2] = (beam_f + staffline_f);


  Interval iv = quantise_iv (allowed_fraction, interline_f, dy_f);
  quanty_f = (dy_f - iv.min () <= iv.max () - dy_f)
    ? iv.min ()
    : iv.max ();


  slope_f_ = (quanty_f / dx_f) / internote_f * sign (slope_f_);
}

static int test_pos = 0;


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

  if (quantisation_ <= NONE)
    return;

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
  Real staffline_f = paper_l ()->rule_thickness ();
  Real beam_f = paper_l ()->beam_thickness_f ();

  /*
    [TODO]
    it would be nice to have all allowed positions in a runtime matrix:
    (multiplicity, minimum_beam_dy, maximum_beam_dy)
   */

  Real straddle = 0;
  Real sit = beam_f / 2 - staffline_f / 2;
  Real inter = space / 2;
  Real hang = space - beam_f / 2 + staffline_f / 2;

  /*
    Put all allowed positions into an array.
    Whether a position is allowed or not depends on 
    strictness of quantisation, multiplicity and direction.

    For simplicity, we'll assume dir = UP and correct if 
    dir = DOWN afterwards.
   */

  // dim(left_y_) = internote
  Real dy_f = dir_ * left_y_ * internote_f;

  Real beamdx_f = stems_.top ()->hpos_f () - stems_[0]->hpos_f ();
  Real beamdy_f = beamdx_f * slope_f_ * internote_f;

  Array<Real> allowed_position;
  if (quantisation_ != TEST)
    {
      if (quantisation_ <= NORMAL) 
	{
	  if ((multiple_i_ <= 2) || (abs (beamdy_f) >= staffline_f / 2))
	    allowed_position.push (straddle);
	  if ((multiple_i_ <= 1) || (abs (beamdy_f) >= staffline_f / 2))
	    allowed_position.push (sit);
	  allowed_position.push (hang);
	}
      else
        // TODO: check and fix TRADITIONAL
	{
	  if ((multiple_i_ <= 2) || (abs (beamdy_f) >= staffline_f / 2))
	    allowed_position.push (straddle);
	  if ((multiple_i_ <= 1) && (beamdy_f <= staffline_f / 2))
	    allowed_position.push (sit);
	  if (beamdy_f >= -staffline_f / 2)
	    allowed_position.push (hang);
	}
    }
  else
    {
      if (test_pos == 0)
        {
	allowed_position.push (hang);
	cout << "hang" << hang << "\n";
	}
      else if (test_pos==1)
        {
	allowed_position.push (straddle);
	cout << "straddle" << straddle << endl;
	}
      else if (test_pos==2)
        {
	allowed_position.push (sit);
	cout << "sit" << sit << endl;
	}
      else if (test_pos==3)
        {
	allowed_position.push (inter);
	cout << "inter" << inter << endl;
	}
    }

  Interval iv = quantise_iv (allowed_position, space, dy_f);

  Real quanty_f = dy_f - iv.min () <= iv.max () - dy_f ? iv.min () : iv.max ();
  if (extend_b)
    quanty_f = iv.max ();

  // dim(left_y_) = internote
  left_y_ = dir_ * quanty_f / internote_f;
}

void
Beam::set_stemlens ()
{
  Real staffline_f = paper_l ()->rule_thickness ();
  // enge floots
  Real epsilon_f = staffline_f / 8;

  DOUT << "Beam::set_stemlens: \n";
  Real dy_f = check_stemlengths_f (false);
  for (int i = 0; i < 2; i++)
    { 
      left_y_ += dy_f * dir_;
      quantise_left_y (dy_f);
      dy_f = check_stemlengths_f (true);
      if (abs (dy_f) <= epsilon_f)
        {
	  DOUT << "Beam::set_stemlens: " << i << " iterations\n";
	  break;
	}
    }

  test_pos++;
  test_pos %= 4;
}

/*
 FIXME
 ugh.  this is broken and should be rewritten.
  - [c8. c32 c32]
 */
void
Beam::set_grouping (Rhythmic_grouping def, Rhythmic_grouping cur)
{
  def.OK ();
  cur.OK ();
  assert (cur.children.size () == stems_.size ());

  cur.split (def);

  Array<int> b;
  {
    Array<int> flags;
    for (int j=0; j <stems_.size (); j++)
      {
	Stem *s = stems_[j];

	int f = s->flag_i_ - 2;
	assert (f>0);
	flags.push (f);
      }
    int fi =0;
    b= cur.generate_beams (flags, fi);
    b.insert (0,0);
    b.push (0);
    assert (stems_.size () == b.size ()/2);
  }

  for (int j=0, i=0; i < b.size () && j <stems_.size (); j++)
    {
      Stem *s = stems_[j];
      Direction d = LEFT;
      do {
	if (s->beams_i_drul_[d] < 0)
	  s->beams_i_drul_[d] = b[i];

	multiple_i_ = multiple_i_ >? s->beams_i_drul_[d];
	i++;
      } while ((flip (&d)) != LEFT);
    }
}

/*
  beams to go with one stem.
  */
Molecule
Beam::stem_beams (Stem *here, Stem *next, Stem *prev) const
{
  assert (!next || next->hpos_f () > here->hpos_f ());
  assert (!prev || prev->hpos_f () < here->hpos_f ());

  Real staffline_f = paper_l ()->rule_thickness ();
  Real interbeam_f = paper_l ()->interbeam_f (multiple_i_);

  Real internote_f = here->staff_line_leading_f ()/2;
  Real beam_f = paper_l ()->beam_thickness_f ();

  Real dy = interbeam_f;
  Real stemdx = staffline_f;
  Real sl = slope_f_* internote_f;
  lookup_l ()->beam (sl, 20 PT, 1 PT);

  Molecule leftbeams;
  Molecule rightbeams;

  // UGH
  Real nw_f = paper_l ()->note_width () * 0.8;

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
	  b.translate_axis (-dir_ * dy * (lwholebeams+j), Y_AXIS);
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
      if (here->beam_gap_i_)
	{
	  int nogap = rwholebeams - here->beam_gap_i_;
	  for (; j  < nogap; j++)
	    {
	      Molecule b (a);
	      b.translate_axis (-dir_ * dy * j, Y_AXIS);
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
	  b.translate (Offset (gap_f, -dir_ * dy * j));
	  rightbeams.add_molecule (b);
	}

      w = w/2 <? nw_f;
      if (rhalfs)
	a = lookup_l ()->beam (sl, w, beam_f);

      for (; j  < rwholebeams + rhalfs; j++)
	{
	  Molecule b (a);
	  b.translate_axis (-dir_ * dy * j, Y_AXIS);
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

