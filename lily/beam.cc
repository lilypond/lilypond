/*
  beam.cc -- implement Beam

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>

  TODO

  Less hairy code.  knee: ([\stem 1; c8 \stem -1; c8]

*/

#include <math.h>

#include "p-col.hh"
#include "varray.hh"
#include "proto.hh"
#include "dimen.hh"
#include "beam.hh"
#include "abbreviation-beam.hh"
#include "misc.hh"
#include "debug.hh"
#include "atom.hh"
#include "molecule.hh"
#include "leastsquares.hh"
#include "stem.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "grouping.hh"
#include "stem-info.hh"
#include "main.hh"  // experimental features


IMPLEMENT_IS_TYPE_B1 (Beam, Spanner);

// ugh, hardcoded
const Real MINIMUM_STEMLEN[] = {
  0, // just in case
  5, 
  4,
  3,
  2,
  2,
};

Beam::Beam ()
{
  slope_f_ = 0;
  left_y_ = 0.0;
  damping_i_ = 1;
  quantisation_ = NORMAL;
  multiple_i_ = 0;
}

void
Beam::add (Stem*s)
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
Beam::brew_molecule_p () const
{
  Molecule *mol_p = new Molecule;
  Real inter_f = paper ()->internote_f ();
  Real x0 = stems_[0]->hpos_f ();
  for (int j=0; j <stems_.size (); j++)
    {
      Stem *i = stems_[j];
      Stem * prev = (j > 0)? stems_[j-1] : 0;
      Stem * next = (j < stems_.size ()-1) ? stems_[j+1] :0;

      Molecule sb = stem_beams (i, next, prev);
      Real  x = i->hpos_f ()-x0;
      sb.translate (Offset (x, (x * slope_f_  + left_y_)* inter_f));
      mol_p->add (sb);
    }
  mol_p->translate_axis (x0 - spanned_drul_[LEFT]->absolute_coordinate (X_AXIS), X_AXIS);
  return mol_p;
}

Offset
Beam::center () const
{
  Real w= (paper ()->note_width () + width ().length ())/2.0;
  return Offset (w, (left_y_ + w* slope_f_)*paper ()->internote_f ());
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
  DOUT << "slope_f_ " <<slope_f_ << "left ypos " << left_y_;
  Spanner::do_print ();
#endif
}

void
Beam::do_post_processing ()
{
  if (stems_.size () < 2)
    {
      warning (_ ("Beam with less than 2 stems"));
      transparent_b_ = true;
      return ;
    }
  solve_slope ();
  set_stemlens ();
}

void
Beam::do_substitute_dependent (Score_elem*o,Score_elem*n)
{
  if (o->is_type_b (Stem::static_name ()))
      stems_.substitute ((Stem*)o->item (),  n? (Stem*) n->item ():0);
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
	: s->get_center_distance (Direction (-d));

      if (current)
	{
	  total[d] += current;
	  count[d] ++;
	}

    } while ((d *= -1) != DOWN);
  
   do {
    if (!total[d])
      count[d] = 1;
  } while ((d *= -1) != DOWN);
  
  /* 

     [Ross] states that the majority of the notes dictates the
     direction (and not the mean of "center distance")
  */
  dir_ = (total[UP] > total[DOWN]) ? UP : DOWN;

  for (int i=0; i <stems_.size (); i++)
    {
      Stem *sl = stems_[i];
      sl->dir_ = dir_;
    }
}

/*
  should use minimum energy formulation (cf linespacing)
*/
void
Beam::solve_slope ()
{
  Array<Stem_info> sinfo;
  for (int j=0; j <stems_.size (); j++)
    {
      Stem *i = stems_[j];

      i->set_default_extents ();
      if (i->invisible_b ())
	continue;

      Stem_info info (i);
      sinfo.push (info);
    }
  if (! sinfo.size ())
    slope_f_ = left_y_ = 0;
  else if (sinfo.size () == 1)
    {
      slope_f_ = 0;
      left_y_ = sinfo[0].idealy_f_;
    }
  else
    {

      Real leftx = sinfo[0].x;
      Least_squares l;
      for (int i=0; i < sinfo.size (); i++)
	{
	  sinfo[i].x -= leftx;
	  l.input.push (Offset (sinfo[i].x, sinfo[i].idealy_f_));
	}

      l.minimise (slope_f_, left_y_);
    }

  Real dy = 0.0;
  for (int i=0; i < sinfo.size (); i++)
    {
      Real y = sinfo[i].x * slope_f_ + left_y_;
      Real my = sinfo[i].miny_f_;

      if (my - y > dy)
	dy = my -y;
    }
  left_y_ += dy;
  left_y_ *= dir_;

  slope_f_ *= dir_;

  /*
    This neat trick is by Werner Lemberg, damped = tanh (slope_f_) corresponds
    with some tables in [Wanske]
    */
  if (damping_i_)
    slope_f_ = 0.6 * tanh (slope_f_) / damping_i_;

  quantise_yspan ();

  // y-values traditionally use internote dimension: therefore slope = (y/in)/x
  // but mf and beam-lookup use PT dimension for y (as used for x-values)
  // ugh --- there goes our simplified but careful quantisation
  Real sl = slope_f_ * paper ()->internote_f ();
  paper ()->lookup_l ()->beam (sl, 20 PT, 1 PT);
  slope_f_ = sl / paper ()->internote_f ();
}

void
Beam::quantise_yspan ()
{
  /*
    [Ross] (simplification of)
    Try to set slope_f_ complying with y-span of:
      - zero
      - beam_thickness / 2 + staffline_thickness / 2
      - beam_thickness + staffline_thickness
    + n * interline
    */

  if (!quantisation_)
    return;

  Real interline_f = paper ()->interline_f ();
  Real internote_f = interline_f / 2;
  Real staffline_thickness = paper ()->rule_thickness ();
  Real beam_thickness = 0.48 * (interline_f - staffline_thickness);

  const int QUANTS = 3;
  Real qdy[QUANTS] = {
    0,
    beam_thickness / 2 + staffline_thickness / 2,
    beam_thickness + staffline_thickness
  };

  Real xspan_f = stems_.top ()->hpos_f () - stems_[0]->hpos_f ();
  // y-values traditionally use internote dimension: therefore slope = (y/in)/x
  Real yspan_f = xspan_f * abs (slope_f_ * internote_f);
  int yspan_i = (int)(yspan_f / interline_f);
  Real q = (yspan_f / interline_f - yspan_i) * interline_f;
  int i = 0;
  for (; i < QUANTS - 1; i++)
    if ((q >= qdy[i]) && (q <= qdy[i + 1]))
      {
	if (q - qdy[i] < qdy[i + 1] - q)
	  break;
	else
	{ 
	  i++;
	  break;
	}
      }
  q = qdy[i];

  yspan_f = (Real)yspan_i * interline_f + q;
  // y-values traditionally use internote dimension: therefore slope = (y/in)/x
  slope_f_ = yspan_f / xspan_f / internote_f * sign (slope_f_);
}

void
Beam::quantise_left_y (Beam::Pos pos, bool extend_b)
{
  /*
   quantising left y should suffice, as slope is quantised too
   if extend then stems must not get shorter
   */

  if (!quantisation_)
    return;

  Real interline_f = paper ()->interline_f ();
  Real internote_f = interline_f / 2;
  Real staffline_thickness = paper ()->rule_thickness ();
  Real beam_thickness = 0.48 * (interline_f - staffline_thickness);

  const int QUANTS = 7;
  Real qy[QUANTS] = 
  {
    0,
    beam_thickness / 2,
    beam_thickness,
    interline_f / 2 + beam_thickness / 2 + staffline_thickness / 2,
    interline_f,
    interline_f + beam_thickness / 2,
    interline_f + beam_thickness
  };
  /* 
   ugh, using i triggers gcc 2.7.2.1 internal compiler error (far down):
   for (int i = 0; i < QUANTS; i++)
   */
  
  // fixme!
  for (int ii = 0; ii < QUANTS; ii++)
    qy[ii] -= 0.5 *beam_thickness;
  Pos qpos[QUANTS] = 
  {
    HANG,
    STRADDLE,
    SIT,
    INTER,
    HANG,
    STRADDLE,
    SIT
  };

  // y-values traditionally use internote dimension
  Real y = left_y_ * internote_f;
  int y_i = (int)floor(y / interline_f);
  y = (y / interline_f - y_i) * interline_f;

  if (y < 0)
    for (int ii = 0; ii < QUANTS; ii++)
      qy[ii] -= interline_f;

  int lower_i = 0;
  int i = 0;
  for (; i < QUANTS; i++)
    {
      if (qy[i] > y)
	break;
      // found if lower_i is allowed, and nearer (from below) y than new pos
      if ((pos & qpos[lower_i]) && (y - qy[lower_i] < y - qy[i]))
        break;
      // if new pos is allowed or old pos isn't: assign new pos
      if ((pos & qpos[i]) || !(pos & qpos[lower_i]))
	lower_i = i;
    }

  int upper_i = QUANTS - 1;
  for (i = QUANTS - 1; i >= 0; i--)
    {
      if (qy[i] < y)
	break;
      // found if upper_i is allowed, and nearer (from above) y than new pos
      if ((pos & qpos[upper_i]) && (qy[upper_i] - y < qy[i] - y))
        break;
      // if new pos is allowed or old pos isn't: assign new pos
      if ((pos & qpos[i]) || !(pos & qpos[upper_i]))
	upper_i = i;
    }

  // y-values traditionally use internote dimension
  Real upper_y = (qy[upper_i] + interline_f * y_i) / internote_f;
  Real lower_y = (qy[lower_i] + interline_f * y_i) / internote_f;

  if (extend_b)
    left_y_ = (dir_ > 0 ? upper_y : lower_y);
  else
    left_y_ = (upper_y - y < y - lower_y ? upper_y : lower_y);
}

void
Beam::set_stemlens ()
{
  Real x0 = stems_[0]->hpos_f ();
  Real dy = 0;

  Real interline_f = paper ()->interline_f ();
  Real internote_f = interline_f / 2;
  Real staffline_thickness = paper ()->rule_thickness ();
  Real beam_thickness = 0.48 * (interline_f - staffline_thickness);
  Real interbeam_f = paper ()->interbeam_f ();
  if (multiple_i_ > 3)
    interbeam_f += 2.0 * staffline_thickness / 4;
  Real xspan_f = stems_.top ()->hpos_f () - stems_[0]->hpos_f ();
  /*
   ugh, y values are in "internote" dimension
   */
  Real yspan_f = xspan_f * abs (slope_f_ * internote_f);
  int yspan_i = (int)(yspan_f / interline_f);

  Pos left_pos = NONE;

  if ((yspan_f < staffline_thickness / 2) || (quantisation_ == NORMAL))
    left_pos = (Pos)(STRADDLE | SIT | HANG);
  else
    left_pos = (Pos) (sign (slope_f_) > 0 ? STRADDLE | HANG 
      : SIT | STRADDLE);

  /* 
   ugh, slope currently mangled by availability mf chars...
   be more generous regarding beam position between stafflines
   */
  Real q = (yspan_f / interline_f - yspan_i) * interline_f;
  if (q < interline_f / 3 - beam_thickness / 2)
    left_pos = (Pos) (left_pos | INTER);

  if (multiple_i_ > 1)
    left_pos = (Pos) (dir_ > 0 ? HANG : SIT);

  // ugh, rounding problems!
  const Real EPSILON = interline_f / 10;
  do
    { 
      left_y_ += dy * dir_;
      quantise_left_y (left_pos, dy);
      dy = 0;
      for (int j=0; j < stems_.size (); j++)
	{
	  Stem *s = stems_[j];
	  if (s->transparent_b_)
	    continue;

	  Real x = s->hpos_f () - x0;
	  s->set_stemend (left_y_ + slope_f_ * x);
	  Real y = s->stem_length_f ();
	  int mult = max (stems_[j]->beams_left_i_, stems_[j]->beams_right_i_);
	  if (mult > 1)
	      // dim(y) = internote
	      y -= (Real)(mult - 1) * interbeam_f / internote_f;
	  if (y < MINIMUM_STEMLEN[mult])
	    dy = dy >? (MINIMUM_STEMLEN[mult] - y);
	}
    } while (abs (dy) > EPSILON);
}

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

  for (int j=0, i=0; i < b.size () && j <stems_.size (); i+= 2, j++)
    {
      Stem *s = stems_[j];
      s->beams_left_i_ = b[i];
      s->beams_right_i_ = b[i+1];
      multiple_i_ = multiple_i_ >? (b[i] >? b[i+1]);
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

  Real staffline_thickness = paper ()->rule_thickness ();
  Real interbeam_f = paper ()->interbeam_f ();
  Real internote_f =paper ()->internote_f (); 
  Real interline_f = 2 * internote_f;
  Real beamheight_f = 0.48 * (interline_f - staffline_thickness);
  if (multiple_i_ > 3)
    interbeam_f += 2.0 * staffline_thickness / 4;
  Real dy = interbeam_f;
  Real stemdx = staffline_thickness;
  Real sl = slope_f_* internote_f;
  paper ()->lookup_l ()->beam (sl, 20 PT, 1 PT);

  Molecule leftbeams;
  Molecule rightbeams;

  /* half beams extending to the left. */
  if (prev)
    {
      int lhalfs= lhalfs = here->beams_left_i_ - prev->beams_right_i_ ;
      int lwholebeams= here->beams_left_i_ <? prev->beams_right_i_ ;
      Real w = (here->hpos_f () - prev->hpos_f ())/4 <? paper ()->note_width ();;
      Atom a;
      if (lhalfs)		// generates warnings if not
	a =  paper ()->lookup_l ()->beam (sl, w, beamheight_f);
      a.translate (Offset (-w, -w * sl));
      for (int j = 0; j  < lhalfs; j++)
	{
	  Atom b (a);
	  b.translate_axis (-dir_ * dy * (lwholebeams+j), Y_AXIS);
	  leftbeams.add (b);
	}
    }

  if (next)
    {
      int rhalfs = here->beams_right_i_ - next->beams_left_i_;
      int rwholebeams = here->beams_right_i_ <? next->beams_left_i_;

      Real w = next->hpos_f () - here->hpos_f ();
      Atom a = paper ()->lookup_l ()->beam (sl, w + stemdx, beamheight_f);
      a.translate_axis( - stemdx/2, X_AXIS);
      int j = 0;
      Real gap_f = 0;
      if (here->beam_gap_i_)
	{
	  int nogap = rwholebeams - here->beam_gap_i_;
	  for (; j  < nogap; j++)
	    {
	      Atom b (a);
	      b.translate_axis (-dir_ * dy * j, Y_AXIS);
	      rightbeams.add (b);
	    }
	  // TODO: notehead widths differ for different types
	  gap_f = paper ()->note_width () / 2;
	  w -= 2 * gap_f;
	  a = paper ()->lookup_l ()->beam (sl, w + stemdx, beamheight_f);
	}

      for (; j  < rwholebeams; j++)
	{
	  Atom b (a);
	  b.translate (Offset (gap_f, -dir_ * dy * j));
	  rightbeams.add (b);
	}

      w = w/4 <? paper ()->note_width ();
      if (rhalfs)
	a = paper ()->lookup_l ()->beam (sl, w, beamheight_f);

      for (; j  < rwholebeams + rhalfs; j++)
	{
	  Atom b (a);
	  b.translate_axis (-dir_ * dy * j, Y_AXIS);
	  rightbeams.add (b);
	}

    }
  leftbeams.add (rightbeams);

  /*
    Does beam quanting think  of the asymetry of beams? 
    Refpoint is on bottom of symbol. (FIXTHAT) --hwn.
   */
  if (experimental_features_global_b && dir_ < 0)
    leftbeams.translate_axis (-beamheight_f, Y_AXIS);
  return leftbeams;
}
