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
  
  - Determine auto knees based on positions if it's set by the user.


Notes:


 - Stems run to the Y-center of the beam.
  
 - beam_translation is the offset between Y centers of the beam.

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


void
Beam::add_stem (Grob *me, Grob *s)
{
  Pointer_group_interface::add_grob (me, ly_symbol2scm ("stems"), s);
  
  s->add_dependency (me);

  assert (!Stem::get_beam (s));
  s->set_grob_property ("beam", me->self_scm ());

  add_bound_item (dynamic_cast<Spanner*> (me), dynamic_cast<Item*> (s));
}


/*
  this returns the translation between 2 adjoining beams.
 */
Real
Beam::get_beam_translation (Grob *me)
{
  SCM func = me->get_grob_property ("space-function");
  SCM s = gh_call2 (func, me->self_scm (), scm_int2num (get_beam_count (me)));
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
      
      m = m >? (Stem::beam_multiplicity (sc).length () + 1);
    }
  return m;
}

MAKE_SCHEME_CALLBACK (Beam, space_function, 2);
SCM
Beam::space_function (SCM smob, SCM beam_count)
{
  Grob *me = unsmob_grob (smob);
  
  Real staff_space = Staff_symbol_referencer::staff_space (me);
  Real line = me->get_paper ()->get_var ("linethickness");
  Real thickness = gh_scm2double (me->get_grob_property ("thickness"))
    * staff_space;
  
  Real beam_translation = gh_scm2int (beam_count) < 4
    ? (2*staff_space + line - thickness) / 2.0
    : (3*staff_space + line - thickness) / 3.0;
  
  return gh_double2scm (beam_translation);
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

	  unsmob_grob (gh_car (stems))->set_grob_property ("beam", SCM_EOL);
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

      consider_auto_knees (me);
      set_stem_directions (me, d);

      connect_beams (me);

      set_stem_shorten (me);
    }

  return SCM_EOL;
}


/*
  We want a maximal number of shared beams, but if there is choice, we
  take the one that is closest to the end of the stem. This is for situations like

       x
      |
      |
  |===|
  |=
  |
  x

  
 */
int
position_with_maximal_common_beams (SCM left_beaming, SCM right_beaming,
				    Direction left_dir,
				    Direction right_dir)
{
  Slice lslice = int_list_to_slice (gh_cdr (left_beaming));

  int best_count = 0;
  int best_start = 0;
  for (int i = lslice[-left_dir];
       (i - lslice[left_dir])* left_dir <= 0 ; i+= left_dir) 
    {
      int count =0;
      for ( SCM s = gh_car (right_beaming); gh_pair_p (s); s = gh_cdr (s))
	{
	  int k = - right_dir * gh_scm2int (gh_car (s)) + i;
	  if (scm_memq (scm_int2num (k), left_beaming) != SCM_BOOL_F)
	    count ++;
	}

      if (count >= best_count)
	{
	  best_count = count; 
	  best_start = i;
	}
    }

  return best_start;
}

void
Beam::connect_beams (Grob *me)
{
  Link_array<Grob> stems=
    Pointer_group_interface__extract_grobs (me, (Grob*)0, "stems");

  Slice last_int;
  last_int.set_empty();
  SCM last_beaming = SCM_EOL;
  Direction last_dir = CENTER;
  for (int i = 0; i< stems.size(); i++)
    {
      Grob *this_stem = stems[i];
      SCM this_beaming = this_stem->get_grob_property ("beaming");

      Direction this_dir = Directional_element_interface::get(this_stem);
      if (i > 0)
	{
	  int start_point = position_with_maximal_common_beams
	    (last_beaming, this_beaming,
	     last_dir, this_dir);
	  
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
		  gh_set_car_x (s, scm_int2num (new_beam_pos));
		}


	    }
	  while (flip (&d) != LEFT);

	  if (!new_slice.empty_b())
	    last_int =  new_slice;
	}
      else
	{
	  gh_set_car_x ( this_beaming, SCM_EOL);
	  SCM s = gh_cdr (this_beaming);
	  for (; gh_pair_p (s); s = gh_cdr (s))
	    {
	      int np = - this_dir * gh_scm2int (gh_car(s));
	      gh_set_car_x (s, scm_int2num (np));
	      last_int.add_point (np);
	    }
	}

      if (i == stems.size () -1)
	{
	  gh_set_cdr_x (this_beaming, SCM_EOL);
	}

      if (scm_ilength (gh_cdr (this_beaming)) > 0)
	{
	  last_beaming = this_beaming;
	  last_dir = this_dir;
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
  Real bdy = get_beam_translation (me);

  SCM last_beaming = SCM_EOL;;
  Real last_xposn = -1;
  Real last_width = -1 ;


  SCM gap = me->get_grob_property ("gap");
  Molecule the_beam;
  Real lt = me->get_paper ()->get_var ("linethickness");
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

	  if (gh_number_p (gap))
	    {
	      Real g = gh_scm2double (gap);
	      stem_offset += g;
	      width_corr -= 2*g; 
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
		  SCM result = gh_call1 (proc, scm_int2num (t));
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
	  str += to_string (gh_scm2int (me->get_grob_property ("best-idx")));
	  str += ":";
	}
      str += to_string (gh_scm2double (me->get_grob_property ("quant-score")),
		     "%.2f");

      SCM properties = Font_interface::font_alist_chain (me);

      
      Molecule tm = Text_item::text2molecule (me, scm_makfrom0str (str.to_str0 ()), properties);
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

  Link_array<Grob> stems=
	Pointer_group_interface__extract_grobs (me, (Grob*)0, "stems");

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
		    gh_cons (scm_int2num (count[UP]),
			     scm_int2num (count[DOWN])),
		    gh_cons (scm_int2num (total[UP]),
			     scm_int2num (total[DOWN])));

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
  Link_array<Grob> stems
    =Pointer_group_interface__extract_grobs (me, (Grob*) 0, "stems");
  
  for (int i=0; i <stems.size (); i++)
    {
      Grob *s = stems[i];
  
      SCM forcedir = s->get_grob_property ("direction");
      if (!to_dir (forcedir))
	Directional_element_interface::set (s, d);
    }
}

/*
  A union of intervals in the real line.

  Abysmal performance (quadratic) for large N, hopefully we don't have
  that large N. In any case, this should probably be rewritten to use
  a balanced tree.
 */
struct Int_set
{
  Array<Interval> allowed_regions_;

  Int_set()
  {
    set_full();
  }

  void set_full()
  {
    allowed_regions_.clear();
    Interval s;
    s.set_full ();
    allowed_regions_.push (s);
  }

  void remove_interval (Interval rm)
  {
    for (int i = 0; i < allowed_regions_.size(); )
      {
	Interval s = rm;

	s.intersect (allowed_regions_[i]);

	if (!s.empty_b ())
	  {
	    Interval before = allowed_regions_[i];
	    Interval after = allowed_regions_[i];

	    before[RIGHT] = s[LEFT];
	    after[LEFT] = s[RIGHT];

	    if (!before.empty_b() && before.length () > 0.0)
	      {
		allowed_regions_.insert (before, i);
		i++;
	      }
	    allowed_regions_.del (i);
	    if (!after.empty_b () && after.length () > 0.0)
	      {
		allowed_regions_.insert (after, i);
		i++;
	      }
	  }
	else
	  i++;
      }
  }
};


/*
  Only try horizontal beams for knees.  No reliable detection of
  anything else is possible here, since we don't know funky-beaming
  settings, or X-distances (slopes!)  People that want sloped
  knee-beams, should set the directions manually.
 */
void
Beam::consider_auto_knees (Grob* me)
{
  SCM scm = me->get_grob_property ("auto-knee-gap");
  if (!gh_number_p (scm))
    return ;

  Real threshold = gh_scm2double (scm);
  
  Int_set gaps;

  gaps.set_full ();

  Link_array<Grob> stems=
    Pointer_group_interface__extract_grobs (me, (Grob*)0, "stems");
      
  Grob *common = common_refpoint_of_array (stems, me,  Y_AXIS);
  Real staff_space = Staff_symbol_referencer::staff_space (me);
  
  Array<Interval> hps_array;  
  for (int i=0; i < stems.size (); i++)
    {
      Grob* stem = stems[i];
      if (Stem::invisible_b (stem))
	continue;

      Interval hps = Stem::head_positions (stem);
      if(!hps.empty_b())
	{
	  hps[LEFT] += -1;
	  hps[RIGHT] += 1; 
	  hps *= staff_space * 0.5 ;

	  /*
	    We could subtract beam Y position, but this routine only
	    sets stem directions, a constant shift does not have an
	    influence.
	    
	   */
	  hps += stem->relative_coordinate (common, Y_AXIS);

	  if (to_dir (stem->get_grob_property ("direction")))
	    {
	      Direction stemdir = to_dir (stem->get_grob_property ("direction"));
	      hps[-stemdir] = - stemdir * infinity_f;
	    }
	}
      hps_array.push (hps);

      gaps.remove_interval (hps);
    }

  Interval max_gap;
  Real max_gap_len =0.0;

  for (int i  = gaps.allowed_regions_.size() -1;  i >=  0 ; i--)
    {
      Interval gap = gaps.allowed_regions_[i];

      /*
	the outer gaps are not knees.
       */
      if (isinf (gap[LEFT]) || isinf(gap[RIGHT]))
	continue;
      
      if (gap.length () >= max_gap_len)
	{
	  max_gap_len = gap.length();
	  max_gap = gap;
	}
    }

  if (max_gap_len > threshold)
    {
      int j = 0;
      for (int i = 0; i < stems.size(); i++)
	{
	  Grob* stem = stems[i];
	  if (Stem::invisible_b (stem))
	    continue;

	  Interval hps = hps_array[j++];


	  Direction d =  (hps.center () < max_gap.center()) ?
	    UP : DOWN ;
	  
	  stem->set_grob_property ("direction", scm_int2num (d));
	  
	  hps.intersect (max_gap);
	  assert (hps.empty_b () || hps.length () < 1e-6 );
	}
    }
}



/* Set stem's shorten property if unset.

 TODO:
   take some y-position (chord/beam/nearest?) into account
   scmify forced-fraction
 
  This is done in beam because the shorten has to be uniform over the
  entire beam.

*/
void
Beam::set_stem_shorten (Grob *me)
{
  /*
    shortening looks silly for x staff beams
   */
  if (knee_b(me))
    return ;
  
  Real forced_fraction = forced_stem_count (me) / visible_stem_count (me);

  int beam_count = get_beam_count (me);

  SCM shorten_list = me->get_grob_property ("beamed-stem-shorten");
  if (shorten_list == SCM_EOL)
    return;

  Real staff_space = Staff_symbol_referencer::staff_space (me);
  
  SCM shorten_elt =
    robust_list_ref (beam_count -1, shorten_list);
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


  Array<Real> x_posns ;
  Link_array<Grob> stems=
    Pointer_group_interface__extract_grobs (me, (Grob*)0, "stems");
  Grob *commonx = common_refpoint_of_array (stems, me, X_AXIS);
  Grob *commony = common_refpoint_of_array (stems, me, Y_AXIS);  

  Real my_y = me->relative_coordinate (commony, Y_AXIS);
  
  Grob *fvs  = first_visible_stem (me);
  Grob *lvs  = last_visible_stem (me);
  
  Interval ideal (Stem::calc_stem_info (fvs).ideal_y_
		  + fvs->relative_coordinate (commony, Y_AXIS) -my_y,
		  Stem::calc_stem_info (lvs).ideal_y_
		  + lvs->relative_coordinate (commony, Y_AXIS) - my_y);
  
  Real x0 = first_visible_stem (me)->relative_coordinate (commonx, X_AXIS);
  for (int i=0; i < stems.size (); i++)
    {
      Grob* s = stems[i];

      Real x = s->relative_coordinate (commonx, X_AXIS) - x0;
      x_posns.push (x);
    }
  Real dx = last_visible_stem (me)->relative_coordinate (commonx, X_AXIS) - x0;

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
	  Grob* s = stems[i];
	  if (Stem::invisible_b (s))
	    continue;
	  ideals.push (Offset (x_posns[i],
			       Stem::calc_stem_info (s).ideal_y_
			       + s->relative_coordinate (commony, Y_AXIS)
			       - my_y));
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
  Link_array<Grob> stems=
    Pointer_group_interface__extract_grobs (me, (Grob*)0, "stems");
  Grob *commonx = common_refpoint_of_array (stems, me, X_AXIS);
  Grob *commony = common_refpoint_of_array (stems, me, Y_AXIS);  

  Grob *fvs = first_visible_stem (me);

  if (!fvs)
    return SCM_UNSPECIFIED;
    
  Real x0 =fvs->relative_coordinate (commonx, X_AXIS);
  for (int i=0; i < stems.size (); i++)
    {
      Grob* s = stems[i];

      Real x = s->relative_coordinate (commonx, X_AXIS) - x0;
      x_posns.push (x);
    }

  Grob *lvs = last_visible_stem (me);
  if (!lvs)
    return SCM_UNSPECIFIED;
  
  Real dx = lvs->relative_coordinate (commonx, X_AXIS) - x0;

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
      Grob* s = stems[i];
      if (Stem::invisible_b (s))
	continue;

      Direction d = Stem::get_direction (s);

      Real left_y =
	Stem::calc_stem_info (s).shortest_y_
	- dydx * x_posns [i];

      /*
	left_y is now relative to the stem S. We want relative to
	ourselves, so translate:
       */
      left_y += 
	+ s->relative_coordinate (commony, Y_AXIS)
	- me->relative_coordinate (commony, Y_AXIS);

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

  Link_array<Grob> stems = 
    Pointer_group_interface__extract_grobs (me, (Grob*) 0, "stems");

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

      Grob *fvs  = first_visible_stem (me);
      Grob *lvs  = last_visible_stem (me);

      Grob *commonx = fvs->common_refpoint (lvs, X_AXIS);


      Real dx = last_visible_stem (me)->relative_coordinate (commonx, X_AXIS)
	- first_visible_stem (me)->relative_coordinate (commonx, X_AXIS);
      Real dydx = dy && dx ? dy/dx : 0;
      dydx = 0.6 * tanh (dydx) / damping;

      Real damped_dy = dydx * dx;
      pos[LEFT] += (dy - damped_dy) / 2;
      pos[RIGHT] -= (dy - damped_dy) / 2;
      
      me->set_grob_property ("positions", ly_interval2scm (pos));
    }
  return SCM_UNSPECIFIED;
}

/*
  Report slice containing the numbers that are both in (car BEAMING)
  and (cdr BEAMING)
 */
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
  in POS for stem S. This Y position is relative to S.
 */
Real
Beam::calc_stem_y (Grob *me, Grob* s, Grob ** common,
		   Real xl, Real xr,
		   Interval pos, bool french) 
{
  Real beam_translation = get_beam_translation (me);

    
  Real r = s->relative_coordinate (common[X_AXIS], X_AXIS) - xl;
  Real dy = pos.delta ();
  Real dx = xr - xl;
  Real stem_y_beam0 = (dy && dx
		       ? r / dx
		       * dy
		       : 0) + pos[LEFT];
  
  Direction my_dir = Directional_element_interface::get (s);
  SCM beaming = s->get_grob_property ("beaming");
 
  Real stem_y = stem_y_beam0;
  if (french)
    {
      Slice bm = where_are_the_whole_beams (beaming);
      if (!bm.empty_b())
	stem_y += beam_translation * bm[-my_dir];
    }
  else
    {
      Slice bm = Stem::beam_multiplicity(s);
      if (!bm.empty_b())
	stem_y +=bm[my_dir] * beam_translation;
    }
  
  Real id = me->relative_coordinate (common[Y_AXIS], Y_AXIS)
    - s->relative_coordinate (common[Y_AXIS], Y_AXIS);
  
  return stem_y + id;
}

/*
  Hmm.  At this time, beam position and slope are determined.  Maybe,
  stem directions and length should set to relative to the chord's
  position of the beam.  */
void
Beam::set_stem_lengths (Grob *me)
{
  Link_array<Grob> stems=
    Pointer_group_interface__extract_grobs (me, (Grob*)0, "stems");

  if (stems.size () <= 1)
    return;
  
  Grob *common[2];
  for (int a = 2; a--;)
    common[a] = common_refpoint_of_array (stems, me, Axis(a));
  
  Interval pos = ly_scm2interval (me->get_grob_property ("positions"));
  Real staff_space = Staff_symbol_referencer::staff_space (me);

  bool french = to_boolean (me->get_grob_property ("french-beaming"));

  
  bool gap = false;
  Real thick =0.0;
  if (gh_number_p (me->get_grob_property ("gap"))
      &&gh_scm2double (me->get_grob_property ("gap")))
  {
    gap = true;
    thick = gh_scm2double (me->get_grob_property ("thickness"))
      * Staff_symbol_referencer::staff_space(me);
  }
      
  // ugh -> use commonx
  Grob * fvs = first_visible_stem (me);
  Grob *lvs = last_visible_stem (me);
    
  Real xl = fvs ? fvs->relative_coordinate (common[X_AXIS], X_AXIS) : 0.0;
  Real xr = lvs ? lvs->relative_coordinate (common[X_AXIS], X_AXIS) : 0.0;
  
  for (int i=0; i < stems.size (); i++)
    {
      Grob* s = stems[i];
      if (Stem::invisible_b (s))
	continue;

      Real stem_y = calc_stem_y (me, s, common,
				 xl, xr,
				 pos, french && i > 0&& (i < stems.size  () -1));

      /*
	Make the stems go up to the end of the beam. This doesn't matter
	for normal beams, but for tremolo beams it looks silly otherwise.
       */
      if (gap)
	stem_y += thick * 0.5 * Directional_element_interface::get(s);
      
      Stem::set_stemend (s, 2* stem_y / staff_space);
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
  Link_array<Grob>stems = 
    Pointer_group_interface__extract_grobs (me, (Grob*) 0, "stems");
  int f = 0;
  for (int i=0; i < stems.size (); i++)
    {
      Grob *s = stems[i];

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
  Link_array<Grob>stems = 
    Pointer_group_interface__extract_grobs (me, (Grob*) 0, "stems");
  int c = 0;
  for (int i = stems.size (); i--;)
    {
      if (!Stem::invisible_b (stems[i]))
        c++;
    }
  return c;
}

Grob*
Beam::first_visible_stem (Grob *me) 
{
  Link_array<Grob>stems = 
    Pointer_group_interface__extract_grobs (me, (Grob*) 0, "stems");
  
  for (int i = 0; i < stems.size (); i++)
    {
      if (!Stem::invisible_b (stems[i]))
        return stems[i];
    }
  return 0;
}

Grob*
Beam::last_visible_stem (Grob *me) 
{
  Link_array<Grob>stems = 
    Pointer_group_interface__extract_grobs (me, (Grob*) 0, "stems");
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
Beam::knee_b (Grob* me)
{
  SCM k = me->get_grob_property ("knee");
  if (gh_boolean_p (k))
    return gh_scm2bool (k);

  bool knee = false;
  int d = 0;
  for (SCM s = me->get_grob_property ("stems"); gh_pair_p (s); s = ly_cdr (s))
    {
      Direction dir = Directional_element_interface::get
	(unsmob_grob (ly_car (s)));
      if (d && d != dir)
	{
	  knee = true;
	  break;
	}
      d = dir;
    }
  
  me->set_grob_property ("knee", gh_bool2scm (knee));

  return knee;
}

int
Beam::get_direction_beam_count (Grob *me, Direction d )
{
  Link_array<Grob>stems = 
    Pointer_group_interface__extract_grobs (me, (Grob*) 0, "stems");
  int bc = 0;
  
  for (int i = stems.size (); i--;)
    {
      /*
	Should we take invisible stems into account?
       */
      if (Stem::get_direction (stems[i]) == d)
        bc = bc >? (Stem::beam_multiplicity (stems[i]).length () + 1);
    }

  return bc;
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


