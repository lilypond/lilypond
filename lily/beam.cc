/*
  beam.cc -- implement Beam
  
  source file of the GNU LilyPond music typesetter
  
  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
  
*/

/*
TODO:


  * Junk stem_info.

  * Use Number_pair i.s.o Interval to represent (yl, yr).

  * cross staff 
  
Notes:

 - Stems run to the Y-center of the beam.
  
 - beam_space is the offset between Y centers of the beam.

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


#define DEBUG_QUANTING 0


#if DEBUG_QUANTING
#include "text-item.hh"  // debug output.
#include "font-interface.hh"  // debug output.
#endif


const int INTER_QUANT_PENALTY = 1000; 
const int SECONDARY_BEAM_DEMERIT  = 15;
const int STEM_LENGTH_DEMERIT_FACTOR = 5;
// possibly ridiculous, but too short stems just won't do
const int STEM_LENGTH_LIMIT_PENALTY = 5000;
const int DAMPING_DIRECTIION_PENALTY = 800;
const int MUSICAL_DIRECTION_FACTOR = 400;
const int IDEAL_SLOPE_FACTOR = 10;
const int REGION_SIZE = 2;


static Real
shrink_extra_weight (Real x)
{
  return fabs (x) * ((x < 0) ? 1.5 : 1.0);
}

// move to somewhree?
Slice
int_list_to_slice (SCM l)
{
  Slice s;
  s.set_empty ();
  for (; gh_pair_p (l); l = gh_cdr (l))
    {
      if (gh_number_p (gh_car (l)))
	s.add_point (gh_scm2int (gh_car (l))); 
    }

  return s;
}

// move to stem?
Slice
stem_beam_multiplicity (Grob *stem)
{
  SCM beaming= stem->get_grob_property ("beaming");
  Slice l = int_list_to_slice (gh_car (beaming));
  Slice r = int_list_to_slice (gh_cdr (beaming));
  l.unite (r);

  return l;
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

Real
Beam::get_beam_space (Grob *me)
{
  SCM func = me->get_grob_property ("space-function");
  SCM s = gh_call2 (func, me->self_scm (), gh_int2scm (get_beam_count (me)));
  return gh_scm2double (s);
}

/*
  Maximum beam_count.
 */
int
Beam::get_beam_count (Grob *me) 
{
  int m = 0;
  for (SCM s = me->get_grob_property ("stems"); gh_pair_p (s); s = ly_cdr (s))
    {
      Grob *sc = unsmob_grob (ly_car (s));
      
      m = m >? (stem_beam_multiplicity (sc).length () + 1);
    }
  return m;
}

MAKE_SCHEME_CALLBACK (Beam, space_function, 2);
SCM
Beam::space_function (SCM smob, SCM beam_count)
{
  Grob *me = unsmob_grob (smob);
  
  Real staff_space = Staff_symbol_referencer::staff_space (me);
  Real line = me->paper_l ()->get_var ("linethickness");
  Real thickness = gh_scm2double (me->get_grob_property ("thickness"))
    * staff_space;
  
  Real beam_space = gh_scm2int (beam_count) < 4
    ? (2*staff_space + line - thickness) / 2.0
    : (3*staff_space + line - thickness) / 3.0;
  
  return gh_double2scm (beam_space);
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
      Direction d = get_default_dir (me);

      consider_auto_knees (me, d);
      set_stem_directions (me, d);

      connect_beams (me);

      set_stem_shorten (me);
    }

  return SCM_EOL;
}



void
Beam::connect_beams (Grob *me)
{
  Link_array<Grob> stems=
    Pointer_group_interface__extract_grobs (me, (Grob*)0, "stems");

  Slice last_int;
  last_int.set_empty();
  for (int i = 0; i< stems.size(); i++)
    {
      Grob *this_stem = stems[i];
      SCM this_beaming = this_stem->get_grob_property ("beaming");

      Direction this_dir = Directional_element_interface::get(this_stem);
      if (i > 0)
	{
	  int start_point = last_int [this_dir];
	  
	  Direction d = LEFT;
	  Slice new_slice ; 
	  do
	    {
	      if (d == RIGHT && i == stems.size()-1)
		continue;
	      
	      new_slice.set_empty();
	      SCM s = index_get_cell (this_beaming, d);
	      for (; gh_pair_p (s); s = gh_cdr (s))
		{
		  int new_beam_pos =
		    start_point - this_dir * gh_scm2int (gh_car (s));

		  new_slice.add_point (new_beam_pos);
		  gh_set_car_x (s, gh_int2scm (new_beam_pos));
		}
	    }
	  while (flip (&d) != LEFT);

	  last_int =  new_slice;
	}
      else
	{
	  SCM s = gh_cdr (this_beaming);
	  for (; gh_pair_p (s); s = gh_cdr (s))
	    {
	      int np = - this_dir * gh_scm2int (gh_car(s));
	      gh_set_car_x (s, gh_int2scm (np));
	      last_int.add_point (np);
	    }
	}
    }
}

MAKE_SCHEME_CALLBACK (Beam, brew_molecule, 1);
SCM
Beam::brew_molecule (SCM grob)
{
  Grob *me = unsmob_grob (grob);
  Link_array<Grob> stems=
    Pointer_group_interface__extract_grobs (me, (Grob*)0, "stems");
  Grob* xcommon = common_refpoint_of_array (stems, me, X_AXIS);

  Real x0, dx;
  if (visible_stem_count (me))
    {
      // ugh -> use commonx
      x0 = first_visible_stem (me)->relative_coordinate (xcommon, X_AXIS);
      dx = last_visible_stem (me)->relative_coordinate (xcommon, X_AXIS) - x0;
    }
  else
    {
      x0 = stems[0]->relative_coordinate (xcommon, X_AXIS);
      dx = stems.top ()->relative_coordinate (xcommon, X_AXIS) - x0;
    }

  SCM posns = me->get_grob_property ("positions");
  Interval pos;
  if (!ly_number_pair_p (posns))
    {
      programming_error ("No beam posns");
      pos = Interval (0,0);
    }
  else
    pos= ly_scm2interval (posns);

  Real dy = pos.delta ();
  Real dydx = dy && dx ? dy/dx : 0;
  
  Real thick = gh_scm2double (me->get_grob_property ("thickness"));
  Real bdy = get_beam_space (me);

  SCM last_beaming = SCM_EOL;;
  Real last_xposn = -1;
  Real last_width = -1 ;
  
  Molecule the_beam;
  Real lt = me->paper_l ()->get_var ("linethickness");
  for (int i = 0; i< stems.size(); i++)
    {
      Grob * st =stems[i];
      
      SCM this_beaming = st->get_grob_property ("beaming");
      Real xposn = st->relative_coordinate (xcommon, X_AXIS);
      Real stem_width = gh_scm2double (st->get_grob_property ("thickness")) *lt;

      if (i > 0)
	{
	  SCM left = gh_cdr (last_beaming);
	  SCM right = gh_car (this_beaming);

	  Array<int> fullbeams;
	  Array<int> lfliebertjes;
	  Array<int> rfliebertjes;	  

	  for (SCM s = left;
	       gh_pair_p (s); s =gh_cdr (s))
	    {
	      int b = gh_scm2int (gh_car (s));
	      if (scm_memq (gh_car(s), right) != SCM_BOOL_F)
		{
		  fullbeams.push (b);
		}
	      else
		{
		  lfliebertjes.push (b); 
		}
	    }
	  for (SCM s = right;
	       gh_pair_p (s); s =gh_cdr (s))
	    {
	      int b = gh_scm2int (gh_car (s));
	      if (scm_memq (gh_car(s), left) == SCM_BOOL_F)
		{
		  rfliebertjes.push (b);
		}
	    }

	  
	  Real w = xposn - last_xposn;
	  Real stem_offset = 0.0;
	  Real width_corr = 0.0;
	  if (i == 1)
	    {
	      stem_offset -= last_width/2;
	      width_corr += last_width/2;
	    }
	  
	  if (i == stems.size() -1)
	    {
	      width_corr += stem_width/2;
	    }
	  
	  Molecule whole = Lookup::beam (dydx, w + width_corr, thick);
	  for (int j = fullbeams.size(); j--;)
	    {
	      Molecule b (whole);
	      b.translate_axis (last_xposn -  x0 + stem_offset, X_AXIS);
	      b.translate_axis (dydx * (last_xposn - x0) + bdy * fullbeams[j], Y_AXIS);
	      the_beam.add_molecule (b);	      
	    }

	  if (lfliebertjes.size() || rfliebertjes.size())
	    {

	      Real nw_f;
	      if (!Stem::first_head (st))
		nw_f = 0;
	      else
		{
		  int t = Stem::duration_log (st); 

		  SCM proc = me->get_grob_property ("flag-width-function");
		  SCM result = gh_call1 (proc, gh_int2scm (t));
		  nw_f = gh_scm2double (result);
		}
	      
	      /* Half beam should be one note-width,
		 but let's make sure two half-beams never touch */
	      
	      Real w = xposn - last_xposn;
	      w = w/2 <? nw_f;

	      Molecule half = Lookup::beam (dydx, w, thick);
	      for (int j = lfliebertjes.size(); j--;)
		{
		  Molecule b (half);
		  b.translate_axis (last_xposn -  x0, X_AXIS);
		  b.translate_axis (dydx * (last_xposn-x0) + bdy * lfliebertjes[j], Y_AXIS);
		  the_beam.add_molecule (b);	      
		}
	      for (int j = rfliebertjes.size(); j--;)
		{
		  Molecule b (half);
		  b.translate_axis (xposn -  x0 - w , X_AXIS);
		  b.translate_axis (dydx * (xposn-x0 -w) + bdy * rfliebertjes[j], Y_AXIS);
		  the_beam.add_molecule (b);	      
		}
	    }
      	}

      last_xposn = xposn;
      last_width = stem_width;
      last_beaming = this_beaming;
    }

  the_beam.translate_axis (x0 - me->relative_coordinate (xcommon, X_AXIS), X_AXIS);
  the_beam.translate_axis (pos[LEFT], Y_AXIS);

#if (DEBUG_QUANTING)
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

      
      Molecule tm = Text_item::text2molecule (me, ly_str02scm (str.ch_C ()), properties);
      the_beam.add_at_edge (Y_AXIS, UP, tm, 5.0);
    }
#endif
    
  
  
  return the_beam.smobbed_copy();
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

      int center_distance = int(- d * Stem::head_positions (s) [-d]) >? 0;
      int current = sd	? (1 + d * sd)/2 : center_distance;

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
Beam::set_stem_directions (Grob *me, Direction d)
{
  Link_array<Item> stems
    =Pointer_group_interface__extract_grobs (me, (Item*) 0, "stems");
  
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
Beam::consider_auto_knees (Grob *me, Direction d)
{
  SCM scm = me->get_grob_property ("auto-knee-gap");

  if (gh_number_p (scm))
    {
      bool knee_b = false;
      Real knee_y = 0;
      Real staff_space = Staff_symbol_referencer::staff_space (me);
      Real gap = gh_scm2double (scm) / staff_space;


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
	      Item *s = stems[i];	  
	      if (Stem::invisible_b (s) || 
		  s->get_grob_property ("dir-forced") == SCM_BOOL_T)
		continue;
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
   scmify forced-fraction

   TODO:
   
   why is shorten stored in beam, and not directly in stem?

*/
void
Beam::set_stem_shorten (Grob *m)
{
  Spanner*me = dynamic_cast<Spanner*> (m);

  Real forced_fraction = forced_stem_count (me) / visible_stem_count (me);

  int beam_count = get_beam_count (me);

  SCM shorten = me->get_grob_property ("beamed-stem-shorten");
  if (shorten == SCM_EOL)
    return;

  int sz = scm_ilength (shorten);
  
  Real staff_space = Staff_symbol_referencer::staff_space (me);
  SCM shorten_elt = scm_list_ref (shorten,
				  gh_int2scm (beam_count <? (sz - 1)));
  Real shorten_f = gh_scm2double (shorten_elt) * staff_space;

  /* your similar cute comment here */
  shorten_f *= forced_fraction;

  if (shorten_f)
    me->set_grob_property ("shorten", gh_double2scm (shorten_f));
}

/*  Call list of y-dy-callbacks, that handle setting of
    grob-properties

*/
MAKE_SCHEME_CALLBACK (Beam, after_line_breaking, 1);
SCM
Beam::after_line_breaking (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  
  /* Copy to mutable list. */
  SCM s = ly_deep_copy (me->get_grob_property ("positions"));
  me->set_grob_property ("positions", s);

  if (ly_car (s) == SCM_BOOL_F)
    {

      // one wonders if such genericity is necessary  --hwn.
      SCM callbacks = me->get_grob_property ("position-callbacks");
      for (SCM i = callbacks; gh_pair_p (i); i = ly_cdr (i))
	gh_call1 (ly_car (i), smob);
    }

  set_stem_lengths (me);  
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

   - One sensible check per demerit (what's this --hwn)

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
  Real slt = me->paper_l ()->get_var ("linethickness") / ss;


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
  Array<Real> lbase_lengths;
  Array<Real> rbase_lengths;  

  Drul_array<bool> dirs_found(0,0);

  bool french = to_boolean (me->get_grob_property ("french-beaming"));
  for (int i= 0; i < stems.size(); i++)
    {
      Grob*s = stems[i];
      stem_infos.push (Stem::calc_stem_info (s));
      dirs_found[stem_infos.top ().dir_] = true;

      Real b = calc_stem_y (me, s, Interval (1,0), french && i > 0&& (i < stems.size  () -1));
      lbase_lengths.push (b);

      Real a = calc_stem_y (me, s, Interval (0,1),  french && i > 0&& (i < stems.size  () -1));
      rbase_lengths.push (a);
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
  int beam_count = get_beam_count (me);
  Real beam_space = beam_count < 4
    ? (2*ss + slt - thickness) / 2.0
     : (3*ss + slt - thickness) / 3.0;

  for (int i = qscores.size (); i--;)
    if (qscores[i].demerits < 100)
      {
	qscores[i].demerits
	  += score_forbidden_quants (me, qscores[i].yl, qscores[i].yr,
				     rad, slt, thickness, beam_space,
				     beam_count, ldir, rdir); 
      }


  for (int i = qscores.size (); i--;)
    if (qscores[i].demerits < 100)
      {
	qscores[i].demerits
	  += score_stem_lengths (stems, stem_infos,
				 lbase_lengths, rbase_lengths,
				 knee_b,
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

#if DEBUG_QUANTING

  // debug quanting
  me->set_grob_property ("quant-score",
			 gh_double2scm (qscores[best_idx].demerits));
  me->set_grob_property ("best-idx", gh_int2scm (best_idx));
#endif

  return SCM_UNSPECIFIED;
}

Real
Beam::score_stem_lengths (Link_array<Grob>stems,
			  Array<Stem_info> stem_infos,
			  Array<Real> left_factor,
			  Array<Real> right_factor,
			  bool knee, 
			  Grob*me,
			  Real yl, Real yr)
{
  Real demerit_score = 0.0 ;
  Real pen = STEM_LENGTH_LIMIT_PENALTY;
  
  for (int i=0; i < stems.size (); i++)
    {
      Grob* s = stems[i];
      if (Stem::invisible_b (s))
	continue;

      Real current_y =
	yl * left_factor[i] + right_factor[i]* yr;

      Stem_info info = stem_infos[i];
      Direction d = info.dir_;

      demerit_score += pen
	* ( 0 >? (info.dir_ * (info.shortest_y_ - current_y)));
      
      demerit_score += STEM_LENGTH_DEMERIT_FACTOR
	* shrink_extra_weight (d * current_y  - info.dir_ * info.ideal_y_);
    }

  demerit_score *= 2.0 / stems.size (); 

  return demerit_score;
}

Real
Beam::score_slopes_dy (Grob *me,
		       Real yl, Real yr,
		       Real dy_mus, Real dy_damp)
{
  Real dy = yr - yl;

  Real dem = 0.0;
  if (sign (dy_damp) != sign (dy))
    {
      dem += DAMPING_DIRECTIION_PENALTY;
    }

   dem += MUSICAL_DIRECTION_FACTOR * (0 >? (fabs (dy) - fabs (dy_mus)));
   dem += shrink_extra_weight (fabs (dy_damp) - fabs (dy))* IDEAL_SLOPE_FACTOR;

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
			      Real thickness, Real beam_space,
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
      

      if (fabs (yl - ldir * beam_space) < rad
	  && fabs (my_modf (yl) - inter) < 1e-3)
	dem += SECONDARY_BEAM_DEMERIT;
      if (fabs (yr - rdir * beam_space) < rad
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
      if (fabs (yl - ldir * beam_space) < rad + inter)
	{
	  if (ldir == UP && dy <= eps
	      && fabs (my_modf (yl) - sit) < eps)
	    dem += SECONDARY_BEAM_DEMERIT;
	  
	  if (ldir == DOWN && dy >= eps
	      && fabs (my_modf (yl) - hang) < eps)
	    dem += SECONDARY_BEAM_DEMERIT;
	}

      if (fabs (yr - rdir * beam_space) < rad + inter)
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
	  if (fabs (yl - 2 * ldir * beam_space) < rad + inter)
	    {
	      if (ldir == UP && dy <= eps
		  && fabs (my_modf (yl) - straddle) < eps)
		dem += SECONDARY_BEAM_DEMERIT;
	      
	      if (ldir == DOWN && dy >= eps
		  && fabs (my_modf (yl) - straddle) < eps)
		dem += SECONDARY_BEAM_DEMERIT;
	}
	  
	  if (fabs (yr - 2 * rdir * beam_space) < rad + inter)
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

  Interval ideal (Stem::calc_stem_info (first_visible_stem (me)).ideal_y_,
		  Stem::calc_stem_info (last_visible_stem (me)).ideal_y_);



  Array<Real> x_posns ;
  Link_array<Item> stems=
    Pointer_group_interface__extract_grobs (me, (Item*)0, "stems");
  Grob *common = stems[0];
  for (int i=1; i < stems.size (); i++)
    common = stems[i]->common_refpoint (common, X_AXIS);

  Real x0 = first_visible_stem (me)->relative_coordinate (common, X_AXIS);
  for (int i=0; i < stems.size (); i++)
    {
      Item* s = stems[i];

      Real x = s->relative_coordinate (common, X_AXIS) - x0;
      x_posns.push (x);
    }
  Real dx = last_visible_stem (me)->relative_coordinate (common, X_AXIS) - x0;

  Real y =0;  
  Real dydx = 0;
  Real dy = 0;
  
  if (!ideal.delta ())
    {
      Interval chord (Stem::chord_start_y (first_visible_stem (me)),
		      Stem::chord_start_y (last_visible_stem (me)));


      /*
	TODO -- use scoring for this.

	complicated, because we take stem-info.ideal for determining
	beam slopes.
       */
      /* Make simple beam on middle line have small tilt */
      if (!ideal[LEFT] && chord.delta () && count == 2)
	{

	  /*
	    FIXME. -> UP
	  */
	  Direction d = (Direction) (sign (chord.delta ()) * UP);
	  pos[d] = gh_scm2double (me->get_grob_property ("thickness")) / 2;
	  //	  	    * dir;
	  pos[-d] = - pos[d];
	}
      else
	{
	  pos = ideal;
	}

      y = pos[LEFT];
      dy = pos[RIGHT]- y;
      dydx = dy/dx;
    }
  else
    {
      Array<Offset> ideals;
      for (int i=0; i < stems.size (); i++)
	{
	  Item* s = stems[i];
	  if (Stem::invisible_b (s))
	    continue;
	  ideals.push (Offset (x_posns[i],
			       Stem::calc_stem_info (s).ideal_y_));
	}
      minimise_least_squares (&dydx, &y, ideals);

      dy = dydx * dx;
      me->set_grob_property ("least-squares-dy", gh_double2scm (dy));
      pos = Interval (y, (y+dy));
    }

  me->set_grob_property ("positions", ly_interval2scm (pos));
 
  return SCM_UNSPECIFIED;
}


/*
  We can't combine with previous function, since check concave and
  slope damping comes first.
 */
MAKE_SCHEME_CALLBACK (Beam, shift_region_to_valid, 1);
SCM
Beam::shift_region_to_valid (SCM grob)
{
  Grob *me = unsmob_grob (grob);
  /*
    Code dup.
   */
  Array<Real> x_posns ;
  Link_array<Item> stems=
    Pointer_group_interface__extract_grobs (me, (Item*)0, "stems");
  Grob *common = stems[0];
  for (int i=1; i < stems.size (); i++)
    common = stems[i]->common_refpoint (common, X_AXIS);

  Grob *fvs = first_visible_stem (me);

  if (!fvs)
    return SCM_UNSPECIFIED;
    
  Real x0 =fvs->relative_coordinate (common, X_AXIS);
  for (int i=0; i < stems.size (); i++)
    {
      Item* s = stems[i];

      Real x = s->relative_coordinate (common, X_AXIS) - x0;
      x_posns.push (x);
    }

  Grob *lvs = last_visible_stem (me);
  if (!lvs)
    return SCM_UNSPECIFIED;
  
  Real dx = lvs->relative_coordinate (common, X_AXIS) - x0;

  Interval pos = ly_scm2interval ( me->get_grob_property ("positions"));
  Real dy = pos.delta();
  Real y = pos[LEFT];
  Real dydx =dy/dx;

  
  /*
    Shift the positions so that we have a chance of finding good
    quants (i.e. no short stem failures.)
   */
  Interval feasible_left_point;
  feasible_left_point.set_full ();
  for (int i=0; i < stems.size (); i++)
    {
      Item* s = stems[i];
      if (Stem::invisible_b (s))
	continue;


      Direction d = Stem::get_direction (s);


      Real left_y = Stem::calc_stem_info (s).shortest_y_
	- dydx * x_posns [i];

      Interval flp ;
      flp.set_full ();
      flp[-d] = left_y;

      feasible_left_point.intersect (flp);
    }
      
  if (feasible_left_point.empty_b())
    {
      warning (_("Not sure that we can find a nice beam slope (no viable initial configuration found)."));
    }
  else if (!feasible_left_point.elem_b(y))
    {
      if (isinf (feasible_left_point[DOWN]))
	y = feasible_left_point[UP] - REGION_SIZE;
      else if (isinf (feasible_left_point[UP]))
	y = feasible_left_point[DOWN]+ REGION_SIZE;
      else
	y = feasible_left_point.center ();
    }
  pos = Interval (y, (y+dy));
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


  /* Concaveness #1: If distance of an inner notehead to line between
     two outer noteheads is bigger than CONCAVENESS-GAP (2.0ss),
     beam is concave (Heinz Stolba).

     In the case of knees, the line connecting outer heads is often
     not related to the beam slope (it may even go in the other
     direction). Skip the check when the outer stems point in
     different directions. --hwn
     
  */
  bool concaveness1 = false;
  SCM gap = me->get_grob_property ("concaveness-gap");
  if (gh_number_p (gap)
      && Stem::get_direction(stems.top ())
         == Stem::get_direction(stems[0]))
    {
      Real r1 = gh_scm2double (gap);
      Real dy = Stem::chord_start_y (stems.top ())
	- Stem::chord_start_y (stems[0]);

      
      Real slope = dy / (stems.size () - 1);
      
      Real y0 = Stem::chord_start_y (stems[0]);
      for (int i = 1; i < stems.size () - 1; i++)
	{
	  Real c = (Stem::chord_start_y (stems[i]) - y0) - i * slope;
	  if (c > r1)
	    {
	      concaveness1 = true;
	      break;
	    }
	}
    }

    
  /* Concaveness #2: Sum distances of inner noteheads that fall
     outside the interval of the two outer noteheads.

     We only do this for beams where first and last stem have the same
     direction. --hwn.


     Note that "convex" stems compensate for "concave" stems.
     (is that intentional?) --hwn.
  */
  
  Real concaveness2 = 0;
  SCM thresh = me->get_grob_property ("concaveness-threshold");
  Real r2 = infinity_f;
  if (!concaveness1 && gh_number_p (thresh)
      && Stem::get_direction(stems.top ())
         == Stem::get_direction(stems[0]))
    {
      r2 = gh_scm2double (thresh);

      Direction dir = Stem::get_direction(stems.top ());
      Real concave = 0;
      Interval iv (Stem::chord_start_y (stems[0]),
		   Stem::chord_start_y (stems.top ()));
      
      if (iv[MAX] < iv[MIN])
	iv.swap ();
      
      for (int i = 1; i < stems.size () - 1; i++)
	{
	  Real f = Stem::chord_start_y (stems[i]);
	  concave += ((f - iv[MAX] ) >? 0) +
	    ((f - iv[MIN] ) <? 0);
	}
      concave *= dir;
      concaveness2 = concave / (stems.size () - 2);
      
      /* ugh: this is the a kludge to get
	 input/regression/beam-concave.ly to behave as
	 baerenreiter. */

      /*
	huh? we're dividing twice (which is not scalable) meaning that
	the longer the beam, the more unlikely it will be
	concave. Maybe you would even expect the other way around??

	--hwn.
	
       */
      concaveness2 /= (stems.size () - 2);
    }
  
  /* TODO: some sort of damping iso -> plain horizontal */
  if (concaveness1 || concaveness2 > r2)
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

Slice
where_are_the_whole_beams(SCM beaming)
{
  Slice l; 
  
  for( SCM s = gh_car (beaming); gh_pair_p (s) ; s = gh_cdr (s))
    {
      if (scm_memq (gh_car (s), gh_cdr (beaming)) != SCM_BOOL_F)
	
	l.add_point (gh_scm2int (gh_car (s)));
    }

  return l;
}

/*
  Calculate the Y position of the stem-end, given the Y-left, Y-right
  in POS, and for stem S.
 */
Real
Beam::calc_stem_y (Grob *me, Grob* s, Interval pos, bool french) 
{
  Real thick = gh_scm2double (me->get_grob_property ("thickness"));
  Real beam_space = get_beam_space (me);

  // ugh -> use commonx
  Grob * fvs = first_visible_stem (me);
  Grob *lvs = last_visible_stem (me);
    
  Real x0 = fvs ? fvs->relative_coordinate (0, X_AXIS) : 0.0;
  Real dx = fvs ? lvs->relative_coordinate (0, X_AXIS) - x0 : 0.0;
  Real r = s->relative_coordinate (0, X_AXIS) - x0;
  Real dy = pos.delta ();
  Real stem_y_beam0 = (dy && dx
		       ? r / dx
		       * dy
		       : 0) + pos[LEFT];


  
  Direction my_dir = Directional_element_interface::get (s);
  SCM beaming = s->get_grob_property ("beaming");
 
  Real stem_y = stem_y_beam0;
  if (french)
    {
      stem_y += beam_space * where_are_the_whole_beams (beaming)[-my_dir];
    }
  else
    {
      stem_y += (stem_beam_multiplicity(s)[my_dir]) * beam_space;
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

  Interval pos = ly_scm2interval (me->get_grob_property ("positions"));
  Real staff_space = Staff_symbol_referencer::staff_space (me);

  bool french = to_boolean (me->get_grob_property ("french-beaming"));
 
  for (int i=0; i < stems.size (); i++)
    {
      Item* s = stems[i];
      if (Stem::invisible_b (s))
	continue;

      Real stem_y = calc_stem_y (me, s, pos, french && i > 0&& (i < stems.size  () -1));

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
      /*
	Don't overwrite user settings.
       */
      
      do
	{
	  /* Don't set beaming for outside of outer stems */	  
	  if ((d == LEFT && i == 0)
	      ||(d == RIGHT && i == stems.size () -1))
	    continue;


	  SCM beaming_prop = stems[i]->get_grob_property ("beaming");
	  if (beaming_prop == SCM_EOL ||
	      index_get_cell (beaming_prop, d) == SCM_EOL)
	    {
	      int b = beaming->infos_.elem (i).beams_i_drul_[d];
	      Stem::set_beaming (stems[i], b, d);
	    }
	}
      while (flip (&d) != LEFT);
    }
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

      if (((int)Stem::chord_start_y (s)) 
        && (Stem::get_direction (s) != Stem::get_default_dir (s)))
        f++;
    }
  return f;
}




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




ADD_INTERFACE (Beam, "beam-interface",
  "A beam.

#'thickness= weight of beams, in staffspace


We take the least squares line through the ideal-length stems, and
then damp that using

	damped = tanh (slope)

this gives an unquantized left and right position for the beam end.
Then we take all combinations of quantings near these left and right
positions, and give them a score (according to how close they are to
the ideal slope, how close the result is to the ideal stems, etc.). We
take the best scoring combination.

",
  "french-beaming position-callbacks concaveness-gap concaveness-threshold dir-function quant-score auto-knee-gap gap chord-tremolo beamed-stem-shorten shorten least-squares-dy damping flag-width-function neutral-direction positions space-function thickness");


