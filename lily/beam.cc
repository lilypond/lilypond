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



static Real
shrink_extra_weight (Real x)
{
  return fabs(x) * ((x < 0) ? 1.5 : 1.0);
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


MAKE_SCHEME_CALLBACK (Beam, new_quanting, 1);
SCM
Beam::new_quanting (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  SCM s = me->get_grob_property ("positions");
  Real yl = gh_scm2double (gh_car(s));
  Real yr = gh_scm2double (gh_cdr(s));

  Real ss = Staff_symbol_referencer::staff_space (me);
  Real thickness = gh_scm2double (me->get_grob_property ("thickness")) / ss;
  Real slt = me->paper_l()->get_var ("stafflinethickness") / ss;
  
  Real straddle = 0.0;
  Real sit = (thickness - slt) / 2;
  Real inter = 0.5;
  Real hang = 1.0 - (thickness - slt) / 2;
  Real quants [] = {straddle, sit, inter, hang };
  
  int num_quants = int(sizeof(quants)/sizeof (Real));
  Array<Real> quantsl;
  Array<Real> quantsr;

  const int REGION_SIZE = 3;
  // -> result indexes between 70 and 575
  for (int i  = -REGION_SIZE ; i < REGION_SIZE; i++)
    for (int j = 0; j < num_quants; j++)
      {
	quantsl.push (i + quants[j] + int (yl));
	quantsr.push (i + quants[j] + int (yr));
      }

  Array<Quant_score> qscores;
  
  for(int l =0; l < quantsl.size(); l++)  
    for(int r =0; r < quantsr.size(); r++)
      {
	Quant_score qs;
	qs.yl = quantsl[l];
	qs.yr = quantsr[r];
	qs.demerits = 0.0;
	
	qscores.push (qs);
      }


  SCM score_funcs = me->get_grob_property("quant-score-functions");
  for (SCM s = score_funcs; gh_pair_p (s); s = gh_cdr (s))
    {
      SCM f = gh_car (s);
      for (int i = qscores.size(); i--;)
	{
	  // best scores < 30;
	  // if (qscores[i].demerits < 1000)
	  if (qscores[i].demerits < 100)
	    {
	      SCM score = gh_call3 (f,
				    me->self_scm(),
				    gh_double2scm (qscores[i].yl),
				    gh_double2scm (qscores[i].yr));
	      
	      qscores[i].demerits += gh_scm2double (score); 
	    }
	}
    }
  
  Real best = 1e6;
  int best_idx = -1;
  for (int i = qscores.size(); i--;)
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

  if (0)
  {
	  // debug quanting
	  me->set_grob_property ("quant-score",
				 gh_double2scm (qscores[best_idx].demerits));
	  me->set_grob_property ("best-idx", gh_int2scm (best_idx));
  }

  return SCM_UNSPECIFIED;
}

MAKE_SCHEME_CALLBACK (Beam, score_slopes_dy, 3);
SCM
Beam::score_slopes_dy (SCM smob, SCM syl, SCM syr)
{
  Grob*me = unsmob_grob(smob);
  Real yl = gh_scm2double (syl);
  Real yr = gh_scm2double (syr);
  Real dy = yr - yl;

  SCM sdy = me->get_grob_property("least-squares-dy");
  SCM posns = me->get_grob_property ("positions");

  Real dy_mus = gh_number_p (sdy) ? gh_scm2double (sdy) : 0.0;
  Real dy_damp = - gh_scm2double (gh_car(posns)) + gh_scm2double (gh_cdr (posns));

  Real dem = 0.0;
  if (sign (dy_damp) != sign (dy))
    {
      dem += 800;
    }
  
   dem += 400* (0 >? (fabs(dy) - fabs(dy_mus)));
  

   dem += shrink_extra_weight (fabs (dy_damp) - fabs(dy))* 10;
   return gh_double2scm  (dem);
}

MAKE_SCHEME_CALLBACK (Beam, score_stem_lengths, 3);
SCM
Beam::score_stem_lengths (SCM smob, SCM syl, SCM syr)
{
  Grob*me = unsmob_grob(smob);
  Real yl = gh_scm2double (syl);
  Real yr = gh_scm2double (syr);

  Link_array<Item> stems=
    Pointer_group_interface__extract_grobs (me, (Item*)0, "stems");

  Real demerit_score = 0.0 ;
  
  for (int i=0; i < stems.size (); i++)
    {
      Item* s = stems[i];
      if (Stem::invisible_b (s))
	continue;

      Real current_y = calc_stem_y_f (me, s, Interval(yl,yr));
      Stem_info info = Stem::calc_stem_info (s);
      Direction d = Directional_element_interface::get (s);
      
      demerit_score += 500 * ( 0 >? (info.miny_f_ - d*current_y));
      demerit_score += 500 * ( 0 >? (d * current_y  - info.maxy_f_));

      demerit_score += 5 * shrink_extra_weight (d * current_y  - info.idealy_f_);
    }

  demerit_score *= 2.0  /stems.size(); 

  return gh_double2scm (demerit_score);
}

static Real
my_modf (Real x)
{
  return x - floor(x);
}



MAKE_SCHEME_CALLBACK (Beam, score_forbidden_quants, 3);
SCM
Beam::score_forbidden_quants (SCM smob, SCM syl, SCM syr)
{
  Grob*me = unsmob_grob(smob);
  Real yl = gh_scm2double (syl);
  Real yr = gh_scm2double (syr);
  Real dy = yr - yl;
  Real rad = Staff_symbol_referencer::staff_radius (me);
  Real dem = 0.0;
  if (fabs (yl) < rad && fabs( my_modf(yl) - 0.5) < 1e-3)
    dem += 1000;
  if (fabs (yr) < rad && fabs( my_modf(yr) - 0.5) < 1e-3)
    dem += 1000;


  int multiplicity = get_multiplicity (me);
  // todo: use multiplicity of outer stems.
  if (multiplicity >= 2)
    {
      Real slt = me->paper_l()->get_var("stafflinethickness");
      Real ss = Staff_symbol_referencer::staff_space(me);
      Real thickness = gh_scm2double (me->get_grob_property ("thickness"))
	* ss;

      Real beam_space= (2*ss + slt  - 3 *thickness) / 2.0;
      if (multiplicity >= 4)
	{
	  beam_space = (3*ss + slt - 4 * thickness) /3.0;
	}

      Real straddle = 0.0;
      Real sit = (thickness - slt) / 2;
      Real inter = 0.5;
      Real hang = 1.0 - (thickness - slt) / 2;
      
      Direction dir = Directional_element_interface::get (me);
      if (fabs (yl - dir * (beam_space + thickness)) < rad
	  && fabs (my_modf (yl) - inter) < 1e-3)
	dem += 15;
      if (fabs (yr - dir * (beam_space + thickness)) < rad
	  && fabs (my_modf (yr) - inter) < 1e-3)
	dem += 15;

      // hmm, without Interval/Drul_array, you get ~ 4x same code...
      if (fabs (yl - dir * (beam_space + thickness)) < rad + inter)
	{
	  if (dir == UP && dy <= 1e-3
	      && fabs (my_modf (yl) - sit) < 1e-3)
	    dem += 15;
	  
	  if (dir == DOWN && dy >= 1e-3
	      && fabs (my_modf (yl) - hang) < 1e-3)
	    dem += 15;
	}

      if (fabs (yr - dir * (beam_space + thickness)) < rad + inter)
	{
	  if (dir == UP && dy >= 1e-3
	      && fabs (my_modf (yr) - sit) < 1e-3)
	    dem += 15;
	  
	  if (dir == DOWN && dy <= 1e-3
	      && fabs (my_modf (yr) - hang) < 1e-3)
	    dem += 15;
	}

      if (multiplicity >= 3)
	{
	  if (fabs (yl - 2 * dir * (beam_space + thickness)) < rad + inter)
	    {
	      if (dir == UP && dy <= 1e-3
		  && fabs (my_modf (yl) - straddle) < 1e-3)
		dem += 15;
	      
	      if (dir == DOWN && dy >= 1e-3
		  && fabs (my_modf (yl) - straddle) < 1e-3)
		dem += 15;
	}
	  
	  if (fabs (yr - 2 * dir * (beam_space + thickness)) < rad + inter)
	    {
	      if (dir == UP && dy >= 1e-3
		  && fabs (my_modf (yr) - straddle) < 1e-3)
		dem += 15;
	      
	      if (dir == DOWN && dy <= 1e-3
		  && fabs (my_modf (yr) - straddle) < 1e-3)
		dem += 15;
	    }
	}
    }
  
  return gh_double2scm ( dem);
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

  Interval ideal (Stem::calc_stem_info (first_visible_stem (me)).idealy_f_,
		  Stem::calc_stem_info (last_visible_stem (me)).idealy_f_);
  
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
	  Direction d = (Direction)(sign (chord.delta ()) * dir);
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
			       Stem::calc_stem_info (s).idealy_f_));
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

/* Prevent interference from stafflines. */
Interval
Beam::quantise_interval (Grob *me, Interval pos, Direction quant_dir)
{
  int multiplicity = get_multiplicity (me);

  Real staff_space = Staff_symbol_referencer::staff_space (me);
  Real thick = me->paper_l ()->get_var ("stafflinethickness");
  Direction dir = Directional_element_interface::get (me);
  Real dy = pos.delta ();

  Drul_array<Interval> bounds;
  Direction d = LEFT;
  do
    {
      SCM proc = d == LEFT
	? me->get_grob_property ("left-position-quant-function")
	: me->get_grob_property ("right-position-quant-function");
      
      SCM quants = scm_apply (proc,
			      me->self_scm (),
			      scm_list_n (gh_int2scm (multiplicity),
					  gh_double2scm (dir),
					  gh_double2scm (dy),
					  gh_double2scm (thick / staff_space),
					  /* HUH? */
					  SCM_EOL,
					  SCM_UNDEFINED));
      
      Array<Real> a;
      for (SCM i = quants; gh_pair_p (i); i = ly_cdr (i))
	a.push (gh_scm2double (ly_car (i)));
      
      if (a.size () <= 1)
	return pos;
      
      bounds[d] = quantise_iv (a, pos[d]*dir/staff_space) * staff_space;
    }
  while (flip (&d) != LEFT);
  
  Real ady = abs (dy);

  // quant direction hints disabled for now
  int q = 0;//(int)quant_dir;

  /* TODO: make smart choice, find best left/right quants pair.

     Slope should never be steeper than least_squares (before damping)
     (save that value?)
     Slope should never be reduced to zero.
   */
  SCM s = me->get_grob_property ("least-squares-dy");
  Real lsdy = gh_number_p (s) ? gh_scm2double (s) : 0;
    
  //  Interval qpos (0, 1000 * sign (dy));
  Interval qpos;
  Real epsilon = staff_space / 10;
  Direction ldir = LEFT;
  do
    {
      Direction rdir = LEFT;
      do
	{
	  Interval i (bounds[LEFT][ldir]*dir, bounds[RIGHT][rdir]*dir);
	  if ((!lsdy
	       || (abs (i.delta ()) <= abs (lsdy) + epsilon
		   && sign (i.delta ()) == sign (lsdy)))
	      && (abs (abs (i.delta ()) - ady)
		  <= abs (abs (qpos.delta ()) - ady))
	      && sign (i.delta ()) == sign (pos.delta ())
	      && (!q
		  || (i[LEFT]*q >= pos[LEFT]*q && i[RIGHT]*q
		      >= pos[RIGHT]*q)))
	    qpos = i;
	}
      while (flip (&rdir) != LEFT);
    }
  while (flip (&ldir) != LEFT);
  
  return qpos;
}


/* Quantise vertical position (left and right) of beam.
   Generalisation of [Ross]. */
MAKE_SCHEME_CALLBACK (Beam, quantise_position, 1);
SCM
Beam::quantise_position (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  Interval pos = ly_scm2interval (me->get_grob_property ("positions"));
  Real y_shift = check_stem_length_f (me, pos);
  pos += y_shift;
  Real staff_space = Staff_symbol_referencer::staff_space (me);

  Direction dir = Directional_element_interface::get (me);
  for (int i = 0; i < 10; i++)
    {
      Interval qpos = quantise_interval (me, pos, CENTER);
      // how to check for uninitised interval,  (inf, -inf)?
      if (qpos[LEFT] < 1000)
	{
	  y_shift = check_stem_length_f (me, qpos);
	  if (y_shift * dir < staff_space / 2)
	    {
	      pos = qpos;
	      break;
	    }
	}
      pos += ((i + 1) * ((i % 2) * -2 + 1)) *  dir * staff_space / 4;
    }
      
  
  me->set_grob_property ("positions", ly_interval2scm (pos));
  set_stem_lengths (me);

#if 0  
  pos = ly_scm2interval (me->get_grob_property ("positions"));
  
  y_shift = check_stem_length_f (me, pos);

  Real half_space = Staff_symbol_referencer::staff_space (me) / 2;
  /* HMMM */
  if (y_shift > half_space / 4)
    {
      pos += y_shift;
      int quant_dir = 0;
      /* for significantly lengthened or shortened stems,
	 request quanting the other way.
	 HMMM */
      if (abs (y_shift) > half_space / 2)
	quant_dir = sign (y_shift) * Directional_element_interface::get (me);
      pos = quantise_interval (me, pos, (Direction)quant_dir);
    }
  
  me->set_grob_property ("positions", ly_interval2scm (pos));
#endif
  
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
Beam::calc_stem_y_f (Grob *me, Item* s, Interval pos)
{
  int beam_multiplicity = get_multiplicity (me);
  int stem_multiplicity = (Stem::flag_i (s) - 2) >? 0;

  SCM space_proc = me->get_grob_property ("space-function");
  SCM space = gh_call1 (space_proc, gh_int2scm (beam_multiplicity));

  Real thick = gh_scm2double (me->get_grob_property ("thickness"));
  Real interbeam_f = gh_scm2double (space);

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
      stem_y -= dir * (thick / 2 + (beam_multiplicity - 1) * interbeam_f);
      
      // huh, why not for first visible?
      if (Staff_symbol_referencer::staff_symbol_l (s)
	  != Staff_symbol_referencer::staff_symbol_l (last_visible_stem (me)))
	stem_y += Directional_element_interface::get (me)
	  * (beam_multiplicity - stem_multiplicity) * interbeam_f;
    }

  return stem_y;
}

/* Make very sure that we don't have stems that are too short.
   Try our best not to have stems that are too long (think: knees).
   
   Optionally (testing): try to lengthen more, to reach more ideal
   stem lengths */
Real
Beam::check_stem_length_f (Grob *me, Interval pos)
{
  Real shorten = 0;
  Real lengthen = 0;
  Direction dir = Directional_element_interface::get (me);

  Link_array<Item> stems=
    Pointer_group_interface__extract_grobs (me, (Item*)0, "stems");

  bool knee = false;
  int ideal_lengthen_count = 0;
  Real ideal_lengthen = 0;
  int ideal_shorten_count = 0;
  Real ideal_shorten = 0;
  
  for (int i=0; i < stems.size (); i++)
    {
      Item* s = stems[i];
      if (Stem::invisible_b (s))
	continue;

      knee |= dir != Directional_element_interface::get (s);

      Real stem_y = calc_stem_y_f (me, s, pos);
      
      stem_y *= dir;
      Stem_info info = Stem::calc_stem_info (s);

      shorten = shorten <? info.maxy_f_ - stem_y;
      lengthen = lengthen >? info.miny_f_ - stem_y;

      if (info.idealy_f_ - stem_y > 0)
	{
#if 0	  
	  ideal_lengthen += info.idealy_f_ - stem_y;
	  ideal_lengthen_count++;
#else
	  ideal_lengthen = ideal_lengthen >? info.idealy_f_ - stem_y;
	  ideal_lengthen_count = 1;
#endif	  
	}
      else if (info.idealy_f_ - stem_y < 0)
	{
#if 0	  
	  ideal_shorten += info.idealy_f_ - stem_y;
	  ideal_shorten_count++;
#else
	  ideal_shorten = ideal_shorten <? info.idealy_f_ - stem_y;
	  ideal_shorten_count = 1;
#endif	  
	}
    }
  
  if (lengthen && shorten)
    me->warning (_ ("weird beam vertical offset"));

  if (ideal_lengthen_count)
    lengthen = (ideal_lengthen / ideal_lengthen_count) >? lengthen;
  if (knee && ideal_shorten_count)
    shorten = (ideal_shorten / ideal_shorten_count) <? shorten;

  if (lengthen && shorten)
    return dir * (lengthen + shorten);
    
  return dir * (shorten ? shorten : lengthen);
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

      Real stem_y = calc_stem_y_f (me, s, pos);

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
       && !(next->relative_coordinate (0, X_AXIS)
	    > here->relative_coordinate (0, X_AXIS)))
      || (prev
	  && !(prev->relative_coordinate (0, X_AXIS)
	       < here->relative_coordinate (0, X_AXIS))))
    programming_error ("Beams are not left-to-right");

  int multiplicity = get_multiplicity (me);

  SCM space_proc = me->get_grob_property ("space-function");
  SCM space = gh_call1 (space_proc, gh_int2scm (multiplicity));

  Real thick = gh_scm2double (me->get_grob_property ("thickness"));
  Real interbeam_f = gh_scm2double (space);
    
  Real bdy = interbeam_f;
  
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

  if (0)
    {
      /*
	This code prints the demerits for each beam. Perhaps this
	should be switchable for those who want to twiddle with the
	parameters.
      */
      String str;
      if (1)
	{
	  str += to_str (gh_scm2int  (me->get_grob_property ("best-idx")));
	  str += ":";
	}
      str += to_str (gh_scm2double (me->get_grob_property ("quant-score")),
		     "%.2f");

      SCM properties = Font_interface::font_alist_chain (me);
  
      Molecule tm = Text_item::text2molecule (me, gh_str02scm (str.ch_C()), properties);
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




/* TODO:
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

  Real staff_space =   Staff_symbol_referencer::staff_space (rest);

  
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

