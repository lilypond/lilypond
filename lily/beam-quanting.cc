/*
  beam-quanting.cc -- implement Beam quanting functions
  
  source file of the GNU LilyPond music typesetter
  
  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
  
*/



#include <math.h>

#include "grob.hh"
#include "staff-symbol-referencer.hh"
#include "beam.hh"
#include "stem.hh"
#include "paper-def.hh"
#include "group-interface.hh"
#include "align-interface.hh"

const int INTER_QUANT_PENALTY = 1000; 
const int SECONDARY_BEAM_DEMERIT  = 15;
const int STEM_LENGTH_DEMERIT_FACTOR = 5;

// possibly ridiculous, but too short stems just won't do
const int STEM_LENGTH_LIMIT_PENALTY = 5000;
const int DAMPING_DIRECTIION_PENALTY = 800;
const int MUSICAL_DIRECTION_FACTOR = 400;
const int IDEAL_SLOPE_FACTOR = 10;


static Real
shrink_extra_weight (Real x, Real fac)
{
  return fabs (x) * ((x < 0) ? fac : 1.0);
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

   - One sensible check per demerit (what's this --hwn)

   - Add demerits for quants per se, as to forbid a specific quant
     entirely

*/

int best_quant_score_idx (Array<Quant_score>  const & qscores)
{
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

  return best_idx;
}
  
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
  Real slt = me->get_paper ()->get_var ("linethickness") / ss;


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


  
  /*
    Do stem computations.  These depend on YL and YR linearly, so we can
    precompute for every stem 2 factors.
   */
  Link_array<Grob> stems=
    Pointer_group_interface__extract_grobs (me, (Grob*)0, "stems");
  Array<Stem_info> stem_infos;
  Array<Real> base_lengths;
  Array<Real> stem_xposns;  

  Drul_array<bool> dirs_found(0,0);
  Grob *common[2];
  for (int a = 2; a--;)
    common[a] = common_refpoint_of_array (stems, me, Axis(a));

  Grob * fvs = first_visible_stem (me);
  Grob *lvs = last_visible_stem (me);
  Real xl = fvs ? fvs->relative_coordinate (common[X_AXIS], X_AXIS) : 0.0;
  Real xr = fvs ? lvs->relative_coordinate (common[X_AXIS], X_AXIS) : 0.0;

  /*
    We store some info to quickly interpolate.

    Sometimes my head is screwed on backwards.  The stemlength are
    AFFINE linear in YL and YR. If YL == YR == 0, then we might have
    stem_y != 0.0, when we're cross staff.
    
   */
  bool french = to_boolean (me->get_grob_property ("french-beaming"));
  for (int i= 0; i < stems.size(); i++)
    {
      Grob*s = stems[i];
      stem_infos.push (Stem::get_stem_info (s));
      dirs_found[stem_infos.top ().dir_] = true;

      bool f = french && i > 0&& (i < stems.size  () -1);
      base_lengths.push (calc_stem_y (me, s, common, xl, xr,
				      Interval (0,0), f));
      stem_xposns.push (s->relative_coordinate (common[X_AXIS], X_AXIS));
    }

  bool xstaff= false;
  if (lvs && fvs)
    {
      Grob *commony = fvs->common_refpoint (lvs, Y_AXIS);
      xstaff = Align_interface::has_interface (commony);
    }
  
  Direction ldir = Direction (stem_infos[0].dir_);
  Direction rdir = Direction (stem_infos.top ().dir_);
  bool knee_b = dirs_found[LEFT] && dirs_found[RIGHT];


  int region_size = REGION_SIZE;
  /*
    Knees are harder, lets try some more possibilities for knees. 
   */
  if (knee_b)
    region_size += 2;
  
  for (int i = -region_size ; i < region_size; i++)
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

  /* This is a longish function, but we don't separate this out into
     neat modular separate subfunctions, as the subfunctions would be
     called for many values of YL, YR. By precomputing various
     parameters outside of the loop, we can save a lot of time. */
  for (int i = qscores.size (); i--;)
    {
      qscores[i].demerits
	+= score_slopes_dy (qscores[i].yl, qscores[i].yr,
			    dy_mus, yr- yl, xstaff); 
    }

  Real rad = Staff_symbol_referencer::staff_radius (me);
  int beam_count = get_beam_count (me);
  Real beam_translation = get_beam_translation (me);

  Real reasonable_score = (knee_b) ? 200000 : 100;
  for (int i = qscores.size (); i--;)
    if (qscores[i].demerits < reasonable_score)
      {
	qscores[i].demerits
	  += score_forbidden_quants (qscores[i].yl, qscores[i].yr,
				     rad, slt, thickness, beam_translation,
				     beam_count, ldir, rdir); 
      }

  ; /* silly gdb thinks best_idx is inside for loop. */
  for (int i = qscores.size (); i--;)
    if (qscores[i].demerits < reasonable_score)
      {
	qscores[i].demerits
	  += score_stem_lengths (stems, stem_infos,
				 base_lengths, stem_xposns,
				 xl, xr,
				 knee_b,
				 qscores[i].yl, qscores[i].yr);
      }

  ; /* silly gdb thinks best_idx is inside for loop. */
  int best_idx = best_quant_score_idx (qscores);
  me->set_grob_property ("positions",
			 gh_cons (gh_double2scm (qscores[best_idx].yl),
				  gh_double2scm (qscores[best_idx].yr))
			 );

#if DEBUG_QUANTING

  // debug quanting
  me->set_grob_property ("quant-score",
			 gh_double2scm (qscores[best_idx].demerits));
  me->set_grob_property ("best-idx", scm_int2num (best_idx));
#endif

  return SCM_UNSPECIFIED;
}

Real
Beam::score_stem_lengths (Link_array<Grob>stems,
			  Array<Stem_info> stem_infos,
			  Array<Real> base_stem_ys,
			  Array<Real> stem_xs,
			  Real xl, Real xr, 
			  bool knee, 
			  Real yl, Real yr)
{
  Real limit_pen = STEM_LENGTH_LIMIT_PENALTY;
  Drul_array<Real> score (0, 0);
  Drul_array<int> count (0, 0);
  
  for (int i=0; i < stems.size (); i++)
    {
      Grob* s = stems[i];
      if (Stem::invisible_b (s))
	continue;

      Real x = stem_xs[i];
      Real dx = xr-xl;
      Real beam_y = yr *(x - xl)/dx + yl * ( xr - x)/dx;
      Real current_y = beam_y + base_stem_ys[i];
      Real length_pen = STEM_LENGTH_LIMIT_PENALTY;
      
      Stem_info info = stem_infos[i];
      Direction d = info.dir_;

      score[d] += limit_pen * (0 >? (d * (info.shortest_y_ - current_y)));
      
      Real ideal_diff = d * (current_y - info.ideal_y_);
      Real ideal_score = shrink_extra_weight (ideal_diff, 1.5);

      /* We introduce a power, to make the scoring strictly
         convex. Otherwise a symmetric knee beam (up/down/up/down)
         does not have an optimum in the middle. */
      if (knee)
	ideal_score = pow (ideal_score, 1.1);
      
      score[d] += length_pen * ideal_score;

      count[d] ++;
    }
  
  if(count[LEFT])
    score[LEFT] /= count[LEFT];
  if(count[RIGHT])
    score[RIGHT] /= count[RIGHT];

  return score[LEFT]+score[RIGHT];
}

Real
Beam::score_slopes_dy (Real yl, Real yr,
		       Real dy_mus, Real dy_damp,
		       bool xstaff)
{
  Real dy = yr - yl;

  Real dem = 0.0;
  if (sign (dy_damp) != sign (dy))
    {
      dem += DAMPING_DIRECTIION_PENALTY;
    }

   dem += MUSICAL_DIRECTION_FACTOR * (0 >? (fabs (dy) - fabs (dy_mus)));


   Real slope_penalty = IDEAL_SLOPE_FACTOR;

   /* Xstaff beams tend to use extreme slopes to get short stems. We
      put in a penalty here. */
   if (xstaff)
     slope_penalty *= 10;

   /* Huh, why would a too steep beam be better than a too flat one ? */
   dem += shrink_extra_weight (fabs (dy_damp) - fabs (dy), 1.5)
     * slope_penalty;
   return dem;
}

static Real
my_modf (Real x)
{
  return x - floor (x);
}

Real
Beam::score_forbidden_quants (Real yl, Real yr,
			      Real rad,
			      Real slt,
			      Real thickness, Real beam_translation,
			      int beam_count,
			      Direction ldir, Direction rdir)
{
  Real dy = yr - yl;

  Real dem = 0.0;
  if (fabs (yl) < rad && fabs ( my_modf (yl) - 0.5) < 1e-3)
    dem += INTER_QUANT_PENALTY;
  if (fabs (yr) < rad && fabs ( my_modf (yr) - 0.5) < 1e-3)
    dem += INTER_QUANT_PENALTY;

  // todo: use beam_count of outer stems.
  if (beam_count >= 2)
    {
     
      Real straddle = 0.0;
      Real sit = (thickness - slt) / 2;
      Real inter = 0.5;
      Real hang = 1.0 - (thickness - slt) / 2;
      

      if (fabs (yl - ldir * beam_translation) < rad
	  && fabs (my_modf (yl) - inter) < 1e-3)
	dem += SECONDARY_BEAM_DEMERIT;
      if (fabs (yr - rdir * beam_translation) < rad
	  && fabs (my_modf (yr) - inter) < 1e-3)
	dem += SECONDARY_BEAM_DEMERIT;

      Real eps = 1e-3;

      /*
	Can't we simply compute the distance between the nearest
	staffline and the secondary beam? That would get rid of the
	silly case analysis here (which is probably not when we have
	different beam-thicknesses.)

	--hwn
       */


      // hmm, without Interval/Drul_array, you get ~ 4x same code...
      if (fabs (yl - ldir * beam_translation) < rad + inter)
	{
	  if (ldir == UP && dy <= eps
	      && fabs (my_modf (yl) - sit) < eps)
	    dem += SECONDARY_BEAM_DEMERIT;
	  
	  if (ldir == DOWN && dy >= eps
	      && fabs (my_modf (yl) - hang) < eps)
	    dem += SECONDARY_BEAM_DEMERIT;
	}

      if (fabs (yr - rdir * beam_translation) < rad + inter)
	{
	  if (rdir == UP && dy >= eps
	      && fabs (my_modf (yr) - sit) < eps)
	    dem += SECONDARY_BEAM_DEMERIT;
	  
	  if (rdir == DOWN && dy <= eps
	      && fabs (my_modf (yr) - hang) < eps)
	    dem += SECONDARY_BEAM_DEMERIT;
	}
      
      if (beam_count >= 3)
	{
	  if (fabs (yl - 2 * ldir * beam_translation) < rad + inter)
	    {
	      if (ldir == UP && dy <= eps
		  && fabs (my_modf (yl) - straddle) < eps)
		dem += SECONDARY_BEAM_DEMERIT;
	      
	      if (ldir == DOWN && dy >= eps
		  && fabs (my_modf (yl) - straddle) < eps)
		dem += SECONDARY_BEAM_DEMERIT;
	}
	  
	  if (fabs (yr - 2 * rdir * beam_translation) < rad + inter)
	    {
	      if (rdir == UP && dy >= eps
		  && fabs (my_modf (yr) - straddle) < eps)
		dem += SECONDARY_BEAM_DEMERIT;
	      
	      if (rdir == DOWN && dy <= eps
		  && fabs (my_modf (yr) - straddle) < eps)
		dem += SECONDARY_BEAM_DEMERIT;
	    }
	}
    }
  
  return dem;
}

  
