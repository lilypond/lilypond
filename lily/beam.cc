/*
  beam.cc -- implement Beam
  
  source file of the GNU LilyPond music typesetter
  
  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
  
*/

/*
  [TODO]

  * Fix TODO
  
  * Junk stem_info.
  
  * Remove #'direction from beam.  A beam has no direction per se.
    It may only set directions for stems.

  * Rewrite stem_beams.

  * Use Number_pair i.s.o Interval to represent (yl, yr).
  
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
#include "text-item.hh"  // debug output.
#include "font-interface.hh"  // debug output.


#define DEBUG_QUANTING 0


static Real
shrink_extra_weight (Real x)
{
  return fabs (x) * ((x < 0) ? 1.5 : 1.0);
}

void
Beam::add_stem (Grob *me, Grob *s)
{
  Pointer_group_interface::add_grob (me, ly_symbol2scm ("stems"), s);
  
  s->add_dependency (me);

  assert (!Stem::beam_l (s));
  s->set_grob_property ("beam", me->self_scm ());

  add_bound_item (dynamic_cast<Spanner*> (me), dynamic_cast<Item*> (s));
}


/*
  TODO: fix this for grace notes.
 */
Real
Beam::get_interbeam (Grob *me)
{
  Real slt = me->paper_l ()->get_var ("stafflinethickness");
  Real ss = Staff_symbol_referencer::staff_space (me);
  Real thickness = gh_scm2double (me->get_grob_property ("thickness"))
    * ss;

  int multiplicity = get_multiplicity (me);
  Real interbeam = multiplicity < 4
    ? (2*ss + slt - thickness) / 2.0
    : (3*ss + slt - thickness) / 3.0;
  
  return interbeam;
}

int
Beam::get_multiplicity (Grob *me) 
{
  int m = 0;
  for (SCM s = me->get_grob_property ("stems"); gh_pair_p (s); s = ly_cdr (s))
    {
      Grob *sc = unsmob_grob (ly_car (s));

      if (Stem::has_interface (sc))
	m = m >? Stem::beam_count (sc, LEFT) >? Stem::beam_count (sc, RIGHT);
    }
  return m;
}

/* After pre-processing all directions should be set.
   Several post-processing routines (stem, slur, script) need stem/beam
   direction.
   Currenly, this means that beam has set all stem's directions.
   [Alternatively, stems could set its own directions, according to
   their beam, during 'final-pre-processing'.] */
MAKE_SCHEME_CALLBACK (Beam, before_line_breaking, 1);
SCM
Beam::before_line_breaking (SCM smob)
{
  Grob *me =  unsmob_grob (smob);

  /* Beams with less than 2 two stems don't make much sense, but could happen
     when you do
     
     [r8 c8 r8].
     
    For a beam that  only has one stem, we try to do some disappearance magic:
    we revert the flag, and move on to The Eternal Engraving Fields. */

  int count = visible_stem_count (me);
  if (count < 2)
    {
      me->warning (_ ("beam has less than two visible stems"));

      SCM stems = me->get_grob_property ("stems");
      if (scm_ilength (stems) == 1)
	{
	  me->warning (_ ("Beam has less than two stems. Removing beam."));

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
  if (count >= 1)
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
Beam::get_default_dir (Grob *me) 
{
  Drul_array<int> total;
  total[UP]  = total[DOWN] = 0;
  Drul_array<int> count; 
  count[UP]  = count[DOWN] = 0;
  Direction d = DOWN;

  Link_array<Item> stems=
	Pointer_group_interface__extract_grobs (me, (Item*)0, "stems");

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
  
  /* If dir is not determined: get default */
  return to_dir (me->get_grob_property ("neutral-direction"));
}


/* Set all stems with non-forced direction to beam direction.
   Urg: non-forced should become `without/with unforced' direction,
   once stem gets cleaned-up. */
void
Beam::set_stem_directions (Grob *me)
{
  Link_array<Item> stems
    =Pointer_group_interface__extract_grobs (me, (Item*) 0, "stems");
  Direction d = Directional_element_interface::get (me);
  
  for (int i=0; i <stems.size (); i++)
    {
      Grob *s = stems[i];
      SCM force = s->remove_grob_property ("dir-forced");
      if (!gh_boolean_p (force) || !gh_scm2bool (force))
	Directional_element_interface::set (s, d);
    }
} 

/* Simplistic auto-knees; only consider vertical gap between two
   adjacent chords.

  `Forced' stem directions are ignored.  If you don't want auto-knees,
  don't set, or unset auto-knee-gap. */
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
	Pointer_group_interface__extract_grobs (me, (Item*)0, "stems");
      
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

/* Set stem's shorten property if unset.

 TODO:
   take some y-position (chord/beam/nearest?) into account
   scmify forced-fraction */
void
Beam::set_stem_shorten (Grob *m)
{
  Spanner*me = dynamic_cast<Spanner*> (m);

  Real forced_fraction = forced_stem_count (me) / visible_stem_count (me);

  int multiplicity = get_multiplicity (me);

  SCM shorten = me->get_grob_property ("beamed-stem-shorten");
  if (shorten == SCM_EOL)
    return;

  int sz = scm_ilength (shorten);
  
  Real staff_space = Staff_symbol_referencer::staff_space (me);
  SCM shorten_elt = scm_list_ref (shorten,
				  gh_int2scm (multiplicity <? (sz - 1)));
  Real shorten_f = gh_scm2double (shorten_elt) * staff_space;

  /* your similar cute comment here */
  shorten_f *= forced_fraction;
  
  me->set_grob_property ("shorten", gh_double2scm (shorten_f));
}

/*  Call list of y-dy-callbacks, that handle setting of
    grob-properties y, dy.
    
    User may set grob-properties: y-position-hs and height-hs
 (to be fixed) that override the calculated y and dy.
    
    Because y and dy cannot be calculated and quanted separately, we
    always calculate both, then check for user override. */
MAKE_SCHEME_CALLBACK (Beam, after_line_breaking, 1);
SCM
Beam::after_line_breaking (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  
  /* Copy to mutable list. */
  SCM s = ly_deep_copy (me->get_grob_property ("positions"));
  me->set_grob_property ("positions", s);

  if (ly_car (s) != SCM_BOOL_F)
    return SCM_UNSPECIFIED;

  SCM callbacks = me->get_grob_property ("position-callbacks");
  for (SCM i = callbacks; gh_pair_p (i); i = ly_cdr (i))
    gh_call1 (ly_car (i), smob);

  return SCM_UNSPECIFIED;
}

struct Quant_score
{
  Real yl;
  Real yr;
  Real demerits;
};


/*
  TODO:
   - Make all demerits customisable
   - One sensible check per demerit
   - Add demerits for quants per se, as to forbid a specific quant
     entirely
 */
MAKE_SCHEME_CALLBACK (Beam, quanting, 1);
SCM
Beam::quanting (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  SCM s = me->get_grob_property ("positions");
  Real yl = gh_scm2double (gh_car (s));
  Real yr = gh_scm2double (gh_cdr (s));

  Real ss = Staff_symbol_referencer::staff_space (me);
  Real thickness = gh_scm2double (me->get_grob_property ("thickness")) / ss;
  Real slt = me->paper_l ()->get_var ("stafflinethickness") / ss;


  SCM sdy = me->get_grob_property ("least-squares-dy");
  Real dy_mus = gh_number_p (sdy) ? gh_scm2double (sdy) : 0.0;
  
  Real straddle = 0.0;
  Real sit = (thickness - slt) / 2;
  Real inter = 0.5;
  Real hang = 1.0 - (thickness - slt) / 2;
  Real quants [] = {straddle, sit, inter, hang };
  
  int num_quants = int (sizeof (quants)/sizeof (Real));
  Array<Real> quantsl;
  Array<Real> quantsr;

  /*
    going to REGION_SIZE == 2, yields another 0.6 second with
    wtk1-fugue2.


    (result indexes between 70 and 575)  ? --hwn. 

  */

  const int REGION_SIZE = 3;
  for (int i  = -REGION_SIZE ; i < REGION_SIZE; i++)
    for (int j = 0; j < num_quants; j++)
      {
	quantsl.push (i + quants[j] + int (yl));
	quantsr.push (i + quants[j] + int (yr));
      }

  Array<Quant_score> qscores;
  
  for (int l =0; l < quantsl.size (); l++)  
    for (int r =0; r < quantsr.size (); r++)
      {
	Quant_score qs;
	qs.yl = quantsl[l];
	qs.yr = quantsr[r];
	qs.demerits = 0.0;
	
	qscores.push (qs);
      }


  /*
    This is a longish function, but we don't separate this out into
    neat modular separate subfunctions, as the subfunctions would be
    called for many values of YL, YR. By precomputing various
    parameters outside of the loop, we can save a lot of time.

  */
  for (int i = qscores.size (); i--;)
    if (qscores[i].demerits < 100)
      {
	qscores[i].demerits
	  += score_slopes_dy (me, qscores[i].yl, qscores[i].yr,
			      dy_mus, yr- yl); 
      }

  Real rad = Staff_symbol_referencer::staff_radius (me);
  int multiplicity = get_multiplicity (me);
  Real interbeam = multiplicity < 4
    ? (2*ss + slt - thickness) / 2.0
     : (3*ss + slt - thickness) / 3.0;

  for (int i = qscores.size (); i--;)
    if (qscores[i].demerits < 100)
      {
	qscores[i].demerits
	  += score_forbidden_quants (me, qscores[i].yl, qscores[i].yr,
				     rad, slt, thickness, interbeam,
				     multiplicity); 
      }


  /*
    Do stem lengths.  These depend on YL and YR linearly, so we can
    precompute for every stem 2 factors.
   */
  Link_array<Grob> stems=
    Pointer_group_interface__extract_grobs (me, (Grob*)0, "stems");
  Array<Stem_info> stem_infos;
  Array<Real> lbase_lengths;
  Array<Real> rbase_lengths;  

  Array<int> directions;
  for (int i= 0; i < stems.size(); i++)
    {
      Grob*s = stems[i];
      stem_infos.push( Stem::calc_stem_info (s));

      Real b = calc_stem_y (me, s, Interval (1,0));
      lbase_lengths.push (b);

      b = calc_stem_y (me, s, Interval (0,1));
      rbase_lengths.push (b);
      directions.push( Directional_element_interface::get( s));
    }

  for (int i = qscores.size (); i--;)
    if (qscores[i].demerits < 100)
      {
	qscores[i].demerits
	  += score_stem_lengths (stems, stem_infos,
				 lbase_lengths, rbase_lengths,
				 directions,
				 me, qscores[i].yl, qscores[i].yr);
      }


  Real best = 1e6;
  int best_idx = -1;
  for (int i = qscores.size (); i--;)
    {
      if (qscores[i].demerits < best)
	{
	  best = qscores [i].demerits ;
	  best_idx = i;
	}
    }

  
  me->set_grob_property ("positions",
			 gh_cons (gh_double2scm (qscores[best_idx].yl),
				  gh_double2scm (qscores[best_idx].yr))
			 );

  if (DEBUG_QUANTING)
  {
	  // debug quanting
	  me->set_grob_property ("quant-score",
				 gh_double2scm (qscores[best_idx].demerits));
	  me->set_grob_property ("best-idx", gh_int2scm (best_idx));
  }

  return SCM_UNSPECIFIED;
}

Real
Beam::score_stem_lengths (Link_array<Grob>stems,
			  Array<Stem_info> stem_infos,
			  Array<Real> left_factor,
			  Array<Real> right_factor,
			  Array<int> directions,
			  Grob*me, Real yl, Real yr)
{
  Real demerit_score = 0.0 ;
  
  for (int i=0; i < stems.size (); i++)
    {
      Grob* s = stems[i];
      if (Stem::invisible_b (s))
	continue;

      Real current_y =
	yl * left_factor[i] + right_factor[i]* yr;

      Stem_info info = stem_infos[i];
      Direction d = Direction (directions[i]);
      
      demerit_score += 500 * ( 0 >? (info.min_y - d * current_y));
      demerit_score += 500 * ( 0 >? (d * current_y  - info.max_y));

      demerit_score += 5 * shrink_extra_weight (d * current_y  - info.ideal_y);
    }

  demerit_score *= 2.0  /stems.size (); 

  return demerit_score;
}

Real
Beam::score_slopes_dy (Grob *me, Real yl, Real yr,
		       Real dy_mus, Real dy_damp)
{
  Real dy = yr - yl;

  Real dem = 0.0;
  if (sign (dy_damp) != sign (dy))
    {
      dem += 800;
    }
  
   dem += 400* (0 >? (fabs (dy) - fabs (dy_mus)));
  

   dem += shrink_extra_weight (fabs (dy_damp) - fabs (dy))* 10;
   return dem;
}

static Real
my_modf (Real x)
{
  return x - floor (x);
}

Real
Beam::score_forbidden_quants (Grob*me,
			      Real yl, Real yr,
			      Real rad,
			      Real slt,
			      Real thickness, Real interbeam,
			      int multiplicity)
{
  Real dy = yr - yl;

  Real dem = 0.0;
  if (fabs (yl) < rad && fabs ( my_modf (yl) - 0.5) < 1e-3)
    dem += 1000;
  if (fabs (yr) < rad && fabs ( my_modf (yr) - 0.5) < 1e-3)
    dem += 1000;

  // todo: use multiplicity of outer stems.
  if (multiplicity >= 2)
    {
     
      Real straddle = 0.0;
      Real sit = (thickness - slt) / 2;
      Real inter = 0.5;
      Real hang = 1.0 - (thickness - slt) / 2;
      
      Direction dir = Directional_element_interface::get (me);
      if (fabs (yl - dir * interbeam) < rad
	  && fabs (my_modf (yl) - inter) < 1e-3)
	dem += 15;
      if (fabs (yr - dir * interbeam) < rad
	  && fabs (my_modf (yr) - inter) < 1e-3)
	dem += 15;

      Real eps = 1e-3;

      /*
	Can't we simply compute the distance between the nearest
	staffline and the secondary beam? That would get rid of the
	silly case analysis here (which is probably not when we have
	different beam-thicknesses.)

	--hwn
       */
      
      // hmm, without Interval/Drul_array, you get ~ 4x same code...
      if (fabs (yl - dir * interbeam) < rad + inter)
	{
	  if (dir == UP && dy <= eps
	      && fabs (my_modf (yl) - sit) < eps)
	    dem += 15;
	  
	  if (dir == DOWN && dy >= eps
	      && fabs (my_modf (yl) - hang) < eps)
	    dem += 15;
	}

      if (fabs (yr - dir * interbeam) < rad + inter)
	{
	  if (dir == UP && dy >= eps
	      && fabs (my_modf (yr) - sit) < eps)
	    dem += 15;
	  
	  if (dir == DOWN && dy <= eps
	      && fabs (my_modf (yr) - hang) < eps)
	    dem += 15;
	}
      
      if (multiplicity >= 3)
	{
	  if (fabs (yl - 2 * dir * interbeam) < rad + inter)
	    {
	      if (dir == UP && dy <= eps
		  && fabs (my_modf (yl) - straddle) < eps)
		dem += 15;
	      
	      if (dir == DOWN && dy >= eps
		  && fabs (my_modf (yl) - straddle) < eps)
		dem += 15;
	}
	  
	  if (fabs (yr - 2 * dir * interbeam) < rad + inter)
	    {
	      if (dir == UP && dy >= eps
		  && fabs (my_modf (yr) - straddle) < eps)
		dem += 15;
	      
	      if (dir == DOWN && dy <= eps
		  && fabs (my_modf (yr) - straddle) < eps)
		dem += 15;
	    }
	}
    }
  
  return dem;
}

  

MAKE_SCHEME_CALLBACK (Beam, least_squares, 1);
SCM
Beam::least_squares (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  int count = visible_stem_count (me);
  Interval pos (0, 0);
  
  if (count <= 1)
    {
      me->set_grob_property ("positions", ly_interval2scm (pos));
      return SCM_UNSPECIFIED;
    }
  
  Direction dir = Directional_element_interface::get (me);

  Interval ideal (Stem::calc_stem_info (first_visible_stem (me)).ideal_y,
		  Stem::calc_stem_info (last_visible_stem (me)).ideal_y);
  
  if (!ideal.delta ())
    {
      Interval chord (Stem::chord_start_f (first_visible_stem (me)),
		      Stem::chord_start_f (last_visible_stem (me)));


      /*
	TODO  : use scoring for this.

	complicated, because we take stem-info.ideal for determining
	beam slopes.
	
       */
      /* Make simple beam on middle line have small tilt */
      if (!ideal[LEFT] && chord.delta () && count == 2)
	{
	  Direction d = (Direction) (sign (chord.delta ()) * dir);
	  pos[d] = gh_scm2double (me->get_grob_property ("thickness")) / 2
	    * dir;
	  pos[-d] = - pos[d];
	}
      else
	{
	  pos = ideal;
	  pos[LEFT] *= dir ;
	  pos[RIGHT] *= dir ;
	}
    }
  else
    {
      Array<Offset> ideals;

      // ugh -> use commonx
      Real x0 = first_visible_stem (me)->relative_coordinate (0, X_AXIS);
      Link_array<Item> stems=
	Pointer_group_interface__extract_grobs (me, (Item*)0, "stems");

      for (int i=0; i < stems.size (); i++)
	{
	  Item* s = stems[i];
	  if (Stem::invisible_b (s))
	    continue;
	  ideals.push (Offset (s->relative_coordinate (0, X_AXIS) - x0,
			       Stem::calc_stem_info (s).ideal_y));
	}
      Real y; 
      Real dydx;
      minimise_least_squares (&dydx, &y, ideals);

      Real dx = last_visible_stem (me)->relative_coordinate (0, X_AXIS) - x0;
      Real dy = dydx * dx;
      me->set_grob_property ("least-squares-dy", gh_double2scm (dy * dir));

      pos = Interval (y*dir, (y+dy) * dir);
    }

  me->set_grob_property ("positions", ly_interval2scm (pos));
  return SCM_UNSPECIFIED;
}

MAKE_SCHEME_CALLBACK (Beam, check_concave, 1);
SCM
Beam::check_concave (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  Link_array<Item> stems = 
    Pointer_group_interface__extract_grobs (me, (Item*) 0, "stems");

  for (int i = 0; i < stems.size ();)
    {
      if (Stem::invisible_b (stems[i]))
	stems.del (i);
      else
	i++;
    }
  
  if (stems.size () < 3)
    return SCM_UNSPECIFIED;

  /* Concaveness try #2: Sum distances of inner noteheads that
     fall outside the interval of the two outer noteheads */
  Real concave = 0;
  Interval iv (Stem::chord_start_f (stems[0]),
	       Stem::chord_start_f (stems.top ()));
  
  if (iv[MAX] < iv[MIN])
    iv.swap ();
  
  for (int i = 1; i < stems.size () - 1; i++)
    {
      Real c = 0;
      Real f = Stem::chord_start_f (stems[i]);
      if ((c = f - iv[MAX]) > 0)
	concave += c;
      else if ((c = f - iv[MIN]) < 0)
	concave += c;
    }

  Direction dir = Directional_element_interface::get (me);
  concave *= dir;
      
  Real concaveness = concave / (stems.size () - 2);
  /* ugh: this is the a kludge to get input/regression/beam-concave.ly
     to behave as baerenreiter. */
  concaveness /= (stems.size () - 2);
  
  Real r = gh_scm2double (me->get_grob_property ("concaveness-threshold"));

  /* TODO: some sort of damping iso -> plain horizontal */
  if (concaveness > r)
    {
      Interval pos = ly_scm2interval (me->get_grob_property ("positions"));
      Real r = pos.linear_combination (0);
      me->set_grob_property ("positions", ly_interval2scm (Interval (r, r)));
      me->set_grob_property ("least-squares-dy", gh_double2scm (0));
    }

  return SCM_UNSPECIFIED;
}

/* This neat trick is by Werner Lemberg,
   damped = tanh (slope)
   corresponds with some tables in [Wanske] CHECKME */
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
      Interval pos = ly_scm2interval (me->get_grob_property ("positions"));
      Real dy = pos.delta ();
      
      // ugh -> use commonx
      Real dx = last_visible_stem (me)->relative_coordinate (0, X_AXIS)
	- first_visible_stem (me)->relative_coordinate (0, X_AXIS);
      Real dydx = dy && dx ? dy/dx : 0;
      dydx = 0.6 * tanh (dydx) / damping;

      Real damped_dy = dydx * dx;
      pos[LEFT] += (dy - damped_dy) / 2;
      pos[RIGHT] -= (dy - damped_dy) / 2;
      
      me->set_grob_property ("positions", ly_interval2scm (pos));
    }
    return SCM_UNSPECIFIED;
}

MAKE_SCHEME_CALLBACK (Beam, end_after_line_breaking, 1);
SCM
Beam::end_after_line_breaking (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  set_stem_lengths (me);
  
  return SCM_UNSPECIFIED;
}

/*
  Calculate the Y position of the stem-end, given the Y-left, Y-right
  in POS, and for stem S.
 */
Real
Beam::calc_stem_y (Grob *me, Grob* s, Interval pos)
{
  int beam_multiplicity = get_multiplicity (me);
  int stem_multiplicity = (Stem::flag_i (s) - 2) >? 0;

  Real thick = gh_scm2double (me->get_grob_property ("thickness"));
  Real interbeam = get_interbeam (me);

  // ugh -> use commonx
  Real x0 = first_visible_stem (me)->relative_coordinate (0, X_AXIS);
  Real dx = last_visible_stem (me)->relative_coordinate (0, X_AXIS) - x0;
  Real dy = pos.delta ();
  Real stem_y = (dy && dx
		 ? (s->relative_coordinate (0, X_AXIS) - x0) / dx
		 * dy
		 : 0) + pos[LEFT];

  /* knee */
  Direction dir  = Directional_element_interface::get (me);
  Direction sdir = Directional_element_interface::get (s);
  
  /* knee */
  if (dir!= sdir)
    {
      stem_y -= dir * (thick / 2 + (beam_multiplicity - 1) * interbeam);

      // huh, why not for first visible?

      Grob *last_visible = last_visible_stem (me);
      if (last_visible)
	{
	  if ( Staff_symbol_referencer::staff_symbol_l (s)
	       != Staff_symbol_referencer::staff_symbol_l (last_visible))
	    stem_y += Directional_element_interface::get (me)
	      * (beam_multiplicity - stem_multiplicity) * interbeam;
	}
      else
	programming_error ("No last visible stem");
    }

  return stem_y;
}

/*
  Hmm.  At this time, beam position and slope are determined.  Maybe,
  stem directions and length should set to relative to the chord's
  position of the beam.  */
void
Beam::set_stem_lengths (Grob *me)
{
  Link_array<Item> stems=
    Pointer_group_interface__extract_grobs (me, (Item*)0, "stems");

  if (stems.size () <= 1)
    return;
  
  Grob *common = me->common_refpoint (stems[0], Y_AXIS);
  for (int i=1; i < stems.size (); i++)
    if (!Stem::invisible_b (stems[i]))
      common = common->common_refpoint (stems[i], Y_AXIS);

  Direction dir = Directional_element_interface::get (me);
  Interval pos = ly_scm2interval (me->get_grob_property ("positions"));
  Real staff_space = Staff_symbol_referencer::staff_space (me);
  Real thick = gh_scm2double (me->get_grob_property ("thickness"));
  bool ps_testing = to_boolean (ly_symbol2scm ("ps-testing"));
  for (int i=0; i < stems.size (); i++)
    {
      Item* s = stems[i];
      if (Stem::invisible_b (s))
	continue;

      Real stem_y = calc_stem_y (me, s, pos);

      // doesn't play well with dvips
      if (ps_testing)
	if (Stem::get_direction (s) == dir)
	  stem_y += Stem::get_direction (s) * thick / 2;
      
      /* caution: stem measures in staff-positions */
      Real id = me->relative_coordinate (common, Y_AXIS)
	- stems[i]->relative_coordinate (common, Y_AXIS);
      Stem::set_stemend (s, (stem_y + id) / staff_space * 2);
    }
}

void
Beam::set_beaming (Grob *me, Beaming_info_list *beaming)
{
  Link_array<Grob> stems=
    Pointer_group_interface__extract_grobs (me, (Grob *)0, "stems");
  
  Direction d = LEFT;
  for (int i=0; i  < stems.size (); i++)
    {
      do
	{
	  /* Don't overwrite user override (?) */
	  if (Stem::beam_count (stems[i], d) == -1
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
Beam::stem_beams (Grob *me, Item *here, Item *next, Item *prev, Real dydx)
{
  // ugh -> use commonx
  if ((next
       && ! (next->relative_coordinate (0, X_AXIS)
	    > here->relative_coordinate (0, X_AXIS)))
      || (prev
	  && ! (prev->relative_coordinate (0, X_AXIS)
	       < here->relative_coordinate (0, X_AXIS))))
    programming_error ("Beams are not left-to-right");

  Real thick = gh_scm2double (me->get_grob_property ("thickness"));
  Real bdy = get_interbeam (me);
  
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
      int lhalfs= lhalfs = Stem::beam_count (here, LEFT)
	- Stem::beam_count (prev, RIGHT);
      int lwholebeams= Stem::beam_count (here, LEFT)
	<? Stem::beam_count (prev, RIGHT);
      
      /* Half beam should be one note-width,
	 but let's make sure two half-beams never touch */

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
      int rhalfs  = Stem::beam_count (here, RIGHT)
	- Stem::beam_count (next, LEFT);
      int rwholebeams= Stem::beam_count (here, RIGHT)
	<? Stem::beam_count (next, LEFT);

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

  /* Does beam quanting think  of the asymetry of beams? 
     Refpoint is on bottom of symbol. (FIXTHAT) --hwn. */
  return leftbeams;
}


MAKE_SCHEME_CALLBACK (Beam, brew_molecule, 1);
SCM
Beam::brew_molecule (SCM smob)
{
  Grob *me =unsmob_grob (smob);

  Molecule mol;
  if (!gh_pair_p (me->get_grob_property ("stems")))
    return SCM_EOL;
  Real x0, dx;
  Link_array<Item>stems = 
    Pointer_group_interface__extract_grobs (me, (Item*) 0, "stems");  
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

  Interval pos = ly_scm2interval (me->get_grob_property ("positions"));
  Real dy = pos.delta ();
  Real dydx = dy && dx ? dy/dx : 0;

  for (int i=0; i < stems.size (); i++)
    {
      Item *item = stems[i];
      Item *prev = (i > 0)? stems[i-1] : 0;
      Item *next = (i < stems.size ()-1) ? stems[i+1] :0;

      Molecule sb = stem_beams (me, item, next, prev, dydx);
      Real x = item->relative_coordinate (0, X_AXIS) - x0;
      sb.translate (Offset (x, x * dydx + pos[LEFT]));
      mol.add_molecule (sb);
    }
  
  mol.translate_axis (x0 
		      - dynamic_cast<Spanner*> (me)
		      ->get_bound (LEFT)->relative_coordinate (0, X_AXIS),
		      X_AXIS);

  if (DEBUG_QUANTING)
    {
      /*
	This code prints the demerits for each beam. Perhaps this
	should be switchable for those who want to twiddle with the
	parameters.
      */
      String str;
      if (1)
	{
	  str += to_str (gh_scm2int (me->get_grob_property ("best-idx")));
	  str += ":";
	}
      str += to_str (gh_scm2double (me->get_grob_property ("quant-score")),
		     "%.2f");

      SCM properties = Font_interface::font_alist_chain (me);
  
      Molecule tm = Text_item::text2molecule (me, gh_str02scm (str.ch_C ()), properties);
      mol.add_at_edge (Y_AXIS, UP, tm, 5.0);
    }
  
  return mol.smobbed_copy ();
}

int
Beam::forced_stem_count (Grob *me) 
{
  Link_array<Item>stems = 
    Pointer_group_interface__extract_grobs (me, (Item*) 0, "stems");
  int f = 0;
  for (int i=0; i < stems.size (); i++)
    {
      Item *s = stems[i];

      if (Stem::invisible_b (s))
	continue;

      if (((int)Stem::chord_start_f (s)) 
        && (Stem::get_direction (s) != Stem::get_default_dir (s)))
        f++;
    }
  return f;
}




/*

TODO:
use filter and standard list functions.

*/
int
Beam::visible_stem_count (Grob *me) 
{
  Link_array<Item>stems = 
    Pointer_group_interface__extract_grobs (me, (Item*) 0, "stems");
  int c = 0;
  for (int i = stems.size (); i--;)
    {
      if (!Stem::invisible_b (stems[i]))
        c++;
    }
  return c;
}

Item*
Beam::first_visible_stem (Grob *me) 
{
  Link_array<Item>stems = 
    Pointer_group_interface__extract_grobs (me, (Item*) 0, "stems");
  
  for (int i = 0; i < stems.size (); i++)
    {
      if (!Stem::invisible_b (stems[i]))
        return stems[i];
    }
  return 0;
}

Item*
Beam::last_visible_stem (Grob *me) 
{
  Link_array<Item>stems = 
    Pointer_group_interface__extract_grobs (me, (Item*) 0, "stems");
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
MAKE_SCHEME_CALLBACK (Beam, rest_collision_callback, 2);
SCM
Beam::rest_collision_callback (SCM element_smob, SCM axis)
{
  Grob *rest = unsmob_grob (element_smob);
  Axis a = (Axis) gh_scm2int (axis);
  
  assert (a == Y_AXIS);

  Grob *st = unsmob_grob (rest->get_grob_property ("stem"));
  Grob *stem = st;
  if (!stem)
    return gh_double2scm (0.0);
  Grob *beam = unsmob_grob (stem->get_grob_property ("beam"));
  if (!beam
      || !Beam::has_interface (beam)
      || !Beam::visible_stem_count (beam))
    return gh_double2scm (0.0);

  // make callback for rest from this.
  // todo: make sure this calced already.

  //  Interval pos = ly_scm2interval (beam->get_grob_property ("positions"));
  Interval pos (0, 0);
  SCM s = beam->get_grob_property ("positions");
  if (gh_pair_p (s) && gh_number_p (ly_car (s)))
    pos = ly_scm2interval (s);

  Real dy = pos.delta ();
  // ugh -> use commonx
  Real x0 = first_visible_stem (beam)->relative_coordinate (0, X_AXIS);
  Real dx = last_visible_stem (beam)->relative_coordinate (0, X_AXIS) - x0;
  Real dydx = dy && dx ? dy/dx : 0;
  
  Direction d = Stem::get_direction (stem);
  Real beamy = (stem->relative_coordinate (0, X_AXIS) - x0) * dydx + pos[LEFT];

  Real staff_space = Staff_symbol_referencer::staff_space (rest);

  
  Real rest_dim = rest->extent (rest, Y_AXIS)[d]*2.0 / staff_space; // refp??

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
Beam::has_interface (Grob *me)
{
  return me->has_interface (ly_symbol2scm ("beam-interface"));
}

