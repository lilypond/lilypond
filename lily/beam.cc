/*
  beam.cc -- implement Beam
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

/*
TODO:

  - Determine auto knees based on positions if it's set by the user.

  - the code is littered with * and / staff_space calls for
    #'positions. Consider moving to real-world coordinates?

    Problematic issue is user tweaks (user tweaks are in staff-coordinates.) 
  
Notes:

 - Stems run to the Y-center of the beam.
  
 - beam_translation is the offset between Y centers of the beam.

*/


#include <math.h> // tanh.

#include "stencil.hh" 
#include "directional-element-interface.hh"
#include "beaming.hh"
#include "beam.hh"
#include "misc.hh"
#include "least-squares.hh"
#include "stem.hh"
#include "output-def.hh"
#include "lookup.hh"
#include "group-interface.hh"
#include "staff-symbol-referencer.hh"
#include "item.hh"
#include "spanner.hh"
#include "warn.hh"

bool debug_beam_quanting_flag;


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
  s->set_property ("beam", me->self_scm ());

  add_bound_item (dynamic_cast<Spanner*> (me), dynamic_cast<Item*> (s));
}


Real
Beam::get_thickness (Grob * me)
{
  return robust_scm2double (me->get_property ("thickness"), 0)
    * Staff_symbol_referencer::staff_space (me);
}

/* Return the translation between 2 adjoining beams. */
Real
Beam::get_beam_translation (Grob *me)
{
  SCM func = me->get_property ("space-function");

  if (ly_c_procedure_p (func))
    {
      SCM s = scm_call_2 (func, me->self_scm (), scm_int2num (get_beam_count (me)));
      return ly_scm2double (s);
    }
  else
    {
      return 0.81;
    }
}

/* Maximum beam_count. */
int
Beam::get_beam_count (Grob *me) 
{
  int m = 0;
  for (SCM s = me->get_property ("stems"); ly_c_pair_p (s); s = ly_cdr (s))
    {
      Grob *stem = unsmob_grob (ly_car (s));
      m = m >? (Stem::beam_multiplicity (stem).length () + 1);
    }
  return m;
}


/*
  Space return space between beams.
 */
MAKE_SCHEME_CALLBACK (Beam, space_function, 2);
SCM
Beam::space_function (SCM smob, SCM beam_count)
{
  Grob *me = unsmob_grob (smob);
  
  Real staff_space = Staff_symbol_referencer::staff_space (me);
  Real line = Staff_symbol_referencer::line_thickness (me);
  Real thickness = get_thickness (me);
  
  Real beam_translation = ly_scm2int (beam_count) < 4
    ? (2*staff_space + line - thickness) / 2.0
    : (3*staff_space + line - thickness) / 3.0;
  
  return scm_make_real (beam_translation);
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

      SCM stems = me->get_property ("stems");
      if (scm_ilength (stems) == 1)
	{
	  me->warning (_ ("removing beam with less than two stems"));

	  unsmob_grob (ly_car (stems))->set_property ("beam", SCM_EOL);
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
  Slice lslice = int_list_to_slice (ly_cdr (left_beaming));

  int best_count = 0;
  int best_start = 0;
  for (int i = lslice[-left_dir];
       (i - lslice[left_dir])* left_dir <= 0 ; i+= left_dir) 
    {
      int count =0;
      for ( SCM s = ly_car (right_beaming); ly_c_pair_p (s); s = ly_cdr (s))
	{
	  int k = - right_dir * ly_scm2int (ly_car (s)) + i;
	  if (scm_c_memq (scm_int2num (k), left_beaming) != SCM_BOOL_F)
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
  last_int.set_empty ();
  SCM last_beaming = SCM_EOL;
  Direction last_dir = CENTER;
  for (int i = 0; i< stems.size (); i++)
    {
      Grob *this_stem = stems[i];
      SCM this_beaming = this_stem->get_property ("beaming");

      Direction this_dir = get_grob_direction (this_stem);
      if (ly_c_pair_p (last_beaming) && ly_c_pair_p (this_beaming))
	{
	  int start_point = position_with_maximal_common_beams
	    (last_beaming, this_beaming,
	     last_dir, this_dir);
	  
	  Direction d = LEFT;
	  Slice new_slice ; 
	  do
	    {
	      if (d == RIGHT && i == stems.size ()-1)
		continue;
	      
	      new_slice.set_empty ();
	      SCM s = index_get_cell (this_beaming, d);
	      for (; ly_c_pair_p (s); s = ly_cdr (s))
		{
		  int new_beam_pos =
		    start_point - this_dir * ly_scm2int (ly_car (s));

		  new_slice.add_point (new_beam_pos);
		  scm_set_car_x (s, scm_int2num (new_beam_pos));
		}


	    }
	  while (flip (&d) != LEFT);

	  if (!new_slice.is_empty ())
	    last_int =  new_slice;
	}
      else
	{
	  scm_set_car_x ( this_beaming, SCM_EOL);
	  SCM s = ly_cdr (this_beaming);
	  for (; ly_c_pair_p (s); s = ly_cdr (s))
	    {
	      int np = - this_dir * ly_scm2int (ly_car (s));
	      scm_set_car_x (s, scm_int2num (np));
	      last_int.add_point (np);
	    }
	}

      if (i == stems.size () -1)
	{
	  scm_set_cdr_x (this_beaming, SCM_EOL);
	}

      if (scm_ilength (ly_cdr (this_beaming)) > 0)
	{
	  last_beaming = this_beaming;
	  last_dir = this_dir;
	}
    }
 }


/*
  TODO: should not make beams per stem, but per Y-level.
 */
MAKE_SCHEME_CALLBACK (Beam, print, 1);
SCM
Beam::print (SCM grob)
{
  Spanner *me = unsmob_spanner (grob);
  position_beam (me);
  
  Link_array<Grob> stems=
    Pointer_group_interface__extract_grobs (me, (Grob*)0, "stems");
  Grob* xcommon = common_refpoint_of_array (stems, me, X_AXIS);

  xcommon = me->get_bound (LEFT)->common_refpoint (xcommon, X_AXIS);
  xcommon = me->get_bound (RIGHT)->common_refpoint (xcommon, X_AXIS);

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

  SCM posns = me->get_property ("positions");
  Drul_array<Real> pos;
  if (!is_number_pair (posns))
    {
      programming_error ("No beam posns");
      pos = Interval (0,0);
    }
  else
    pos= ly_scm2realdrul (posns);

  scale_drul (  &pos,  Staff_symbol_referencer::staff_space (me));
  
  Real dy = pos[RIGHT] - pos[LEFT];
  Real dydx = (dy && dx) ? dy/dx : 0;
  
  Real thick = get_thickness (me);
  Real bdy = get_beam_translation (me);

  SCM last_beaming = SCM_EOL;
  Real last_xposn = -1;
  Real last_stem_width = -1 ;

  Real gap_length =robust_scm2double ( me->get_property ("gap"), 0.0);
  
  Stencil the_beam;
  Real lt = me->get_paper ()->get_dimension (ly_symbol2scm ("linethickness"));
  
  for (int i = 0; i<= stems.size (); i++)
    {
      Grob * st = (i < stems.size ()) ? stems[i] : 0;
      
      SCM this_beaming = st ? st->get_property ("beaming") : SCM_EOL;
      Real xposn = st ? st->relative_coordinate (xcommon, X_AXIS) : 0.0;
      Real stem_width = st ? robust_scm2double (st->get_property ("thickness"), 1.0) *lt : 0 ;
      Direction stem_dir = st ? to_dir (st->get_property ("direction")) : CENTER;
      /*
	We do the space left of ST, with lfliebertjes pointing to the
	right from the left stem, and rfliebertjes pointing left from
	right stem.
       */
      SCM left = (i>0) ? ly_cdr (last_beaming) : SCM_EOL;
      SCM right = st ? ly_car (this_beaming) : SCM_EOL;

      Array<int> full_beams;
      Array<int> lfliebertjes;
      Array<int> rfliebertjes;	  

      for (SCM s = left;
	   ly_c_pair_p (s); s =ly_cdr (s))
	{
	  int b = ly_scm2int (ly_car (s));
	  if (scm_c_memq (ly_car (s), right) != SCM_BOOL_F)
	    {
	      full_beams.push (b);
	    }
	  else
	    {
	      lfliebertjes.push (b); 
	    }
	}
      for (SCM s = right;
	   ly_c_pair_p (s); s =ly_cdr (s))
	{
	  int b = ly_scm2int (ly_car (s));
	  if (scm_c_memq (ly_car (s), left) == SCM_BOOL_F)
	    {
	      rfliebertjes.push (b);
	    }
	}

      /*
	how much to stick out for beams across linebreaks
       */
      Real break_overshoot = 3.0;
      Real w = (i > 0 && st) ? (xposn - last_xposn) : break_overshoot;

      Real stem_offset =0.0;
      if (i > 0)
	{
	  w += last_stem_width / 2;
	  stem_offset = -last_stem_width / 2;
	}

      if (st)
	w += stem_width/ 2 ;
      

      Real blot = me->get_paper ()->get_dimension (ly_symbol2scm ("blotdiameter"));
      Stencil whole = Lookup::beam (dydx, w, thick, blot);
      Stencil gapped;

      int gap_count = 0;
      if (ly_c_number_p (me->get_property ("gap-count")))
	{
	  gap_count = ly_scm2int (me->get_property ("gap-count"));
	  gapped = Lookup::beam (dydx, w - 2 * gap_length, thick, blot);

	  full_beams.sort (default_compare);
	  if (stem_dir == UP)
	    full_beams.reverse ();
	}

      int k = 0;
      for (int j = full_beams.size (); j--;)
	{
	  Stencil b (whole);
	  
	  if (k++ < gap_count)
	    {
	      b = gapped;
	      b.translate_axis (gap_length, X_AXIS);
	    }
	  b.translate_axis (last_xposn -  x0 + stem_offset, X_AXIS);
	  b.translate_axis (dydx * (last_xposn - x0) + bdy * full_beams[j], Y_AXIS);

	  the_beam.add_stencil (b);	      
	}
	  
      if (lfliebertjes.size () || rfliebertjes.size ())
	{
	  Real nw_f;

	  if (st)
	    {
	      int t = Stem::duration_log (st); 

	      SCM proc = me->get_property ("flag-width-function");
	      SCM result = scm_call_1 (proc, scm_int2num (t));
	      nw_f = ly_scm2double (result);
	    }
	  else
	    nw_f = break_overshoot / 2;
	      
	  /* Half beam should be one note-width,
	     but let's make sure two half-beams never touch */
	  Real lw = nw_f;
	  Real rw = nw_f;
	  if (i > 0)
	    rw = nw_f <? ( (xposn - last_xposn) / 2);
	  else
	    /*
	      TODO: 0.5 is a guess.
	    */
	    rw = xposn - me->get_bound (LEFT)->extent (xcommon, X_AXIS)[RIGHT]
	      - 0.5;
	  
	  if (st)
	    lw = nw_f <? ( (xposn - last_xposn) / 2);
	  else
	    lw = me->get_bound (RIGHT)->relative_coordinate (xcommon, X_AXIS)
	      - last_xposn;

	  Stencil rhalf = Lookup::beam (dydx, rw, thick, blot);
	  Stencil lhalf = Lookup::beam (dydx, lw, thick, blot);
	  for (int j = lfliebertjes.size (); j--;)
	    {
	      Stencil b (lhalf);
	      b.translate_axis (last_xposn -  x0, X_AXIS);
	      b.translate_axis (dydx * (last_xposn-x0) + bdy * lfliebertjes[j], Y_AXIS);
	      the_beam.add_stencil (b);	      
	    }
	  for (int j = rfliebertjes.size (); j--;)
	    {
	      Stencil b (rhalf);
	      b.translate_axis (xposn -  x0 - rw , X_AXIS);
	      b.translate_axis (dydx * (xposn-x0 -rw) + bdy * rfliebertjes[j], Y_AXIS);
	      the_beam.add_stencil (b);	      
	    }
	}


      last_xposn = xposn;
      last_stem_width = stem_width;
      last_beaming = this_beaming;
    }

  the_beam.translate_axis (x0 - me->relative_coordinate (xcommon, X_AXIS), X_AXIS);
  the_beam.translate_axis (pos[LEFT], Y_AXIS);

#if (DEBUG_QUANTING)
  SCM quant_score = me->get_property ("quant-score");
  if (debug_beam_quanting_flag
      && ly_c_string_p (quant_score))
    {
      
      /*
	This code prints the demerits for each beam. Perhaps this
	should be switchable for those who want to twiddle with the
	parameters.
      */
      String str;
      SCM properties = Font_interface::text_font_alist_chain (me);

      Stencil tm = *unsmob_stencil (Text_item::interpret_markup
	 (me->get_paper ()->self_scm (), properties, quant_score));
      the_beam.add_at_edge (Y_AXIS, UP, tm, 5.0, 0);
    }
#endif
    
  
  
  return the_beam.smobbed_copy ();
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
      Direction sd = get_grob_direction (s);

      int center_distance = int (- d * Stem::head_positions (s) [-d]) >? 0;
      int current = sd	? (1 + d * sd)/2 : center_distance;

      if (current)
	{
	  total[d] += current;
	  count[d] ++;
	}
    } while (flip (&d) != DOWN);
  
  SCM func = me->get_property ("dir-function");
  SCM s = scm_call_2 (func,
		    scm_cons (scm_int2num (count[UP]),
			     scm_int2num (count[DOWN])),
		    scm_cons (scm_int2num (total[UP]),
			     scm_int2num (total[DOWN])));

  if (ly_c_number_p (s) && ly_scm2int (s))
    return to_dir (s);
  
  /* If dir is not determined: get default */
  return to_dir (me->get_property ("neutral-direction"));
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
  
      SCM forcedir = s->get_property ("direction");
      if (!to_dir (forcedir))
	set_grob_direction (s,  d);
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

  Int_set ()
  {
    set_full ();
  }

  void set_full ()
  {
    allowed_regions_.clear ();
    Interval s;
    s.set_full ();
    allowed_regions_.push (s);
  }

  void remove_interval (Interval rm)
  {
    for (int i = 0; i < allowed_regions_.size (); )
      {
	Interval s = rm;

	s.intersect (allowed_regions_[i]);

	if (!s.is_empty ())
	  {
	    Interval before = allowed_regions_[i];
	    Interval after = allowed_regions_[i];

	    before[RIGHT] = s[LEFT];
	    after[LEFT] = s[RIGHT];

	    if (!before.is_empty () && before.length () > 0.0)
	      {
		allowed_regions_.insert (before, i);
		i++;
	      }
	    allowed_regions_.del (i);
	    if (!after.is_empty () && after.length () > 0.0)
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
  SCM scm = me->get_property ("auto-knee-gap");
  if (!ly_c_number_p (scm))
    return ;

  Real threshold = ly_scm2double (scm);
  
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
      if (Stem::is_invisible (stem))
	continue;

      Interval hps = Stem::head_positions (stem);
      if (!hps.is_empty ())
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

	  if (to_dir (stem->get_property ("direction")))
	    {
	      Direction stemdir = to_dir (stem->get_property ("direction"));
	      hps[-stemdir] = - stemdir * infinity_f;
	    }
	}
      hps_array.push (hps);

      gaps.remove_interval (hps);
    }

  Interval max_gap;
  Real max_gap_len =0.0;

  for (int i  = gaps.allowed_regions_.size () -1;  i >=  0 ; i--)
    {
      Interval gap = gaps.allowed_regions_[i];

      /*
	the outer gaps are not knees.
       */
      if (isinf (gap[LEFT]) || isinf (gap[RIGHT]))
	continue;
      
      if (gap.length () >= max_gap_len)
	{
	  max_gap_len = gap.length ();
	  max_gap = gap;
	}
    }

  if (max_gap_len > threshold)
    {
      int j = 0;
      for (int i = 0; i < stems.size (); i++)
	{
	  Grob* stem = stems[i];
	  if (Stem::is_invisible (stem))
	    continue;

	  Interval hps = hps_array[j++];


	  Direction d =  (hps.center () < max_gap.center ()) ?
	    UP : DOWN ;
	  
	  stem->set_property ("direction", scm_int2num (d));
	  
	  hps.intersect (max_gap);
	  assert (hps.is_empty () || hps.length () < 1e-6 );
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
  if (is_knee (me))
    return ;
  
  Real forced_fraction = 1.0 * forced_stem_count (me)
    / visible_stem_count (me);

  int beam_count = get_beam_count (me);

  SCM shorten_list = me->get_property ("beamed-stem-shorten");
  if (shorten_list == SCM_EOL)
    return;

  Real staff_space = Staff_symbol_referencer::staff_space (me);
  
  SCM shorten_elt =
    robust_list_ref (beam_count -1, shorten_list);
  Real shorten_f = ly_scm2double (shorten_elt) * staff_space;

  /* your similar cute comment here */
  shorten_f *= forced_fraction;

  if (shorten_f)
    me->set_property ("shorten", scm_make_real (shorten_f));
}

/*  Call list of y-dy-callbacks, that handle setting of
    grob-properties

*/
MAKE_SCHEME_CALLBACK (Beam, after_line_breaking, 1);
SCM
Beam::after_line_breaking (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  position_beam (me);
  return SCM_UNSPECIFIED;
}

void
Beam::position_beam (Grob *me)
{
  if (to_boolean (me->get_property ("positioning-done")))
    return ;

  me->set_property ("positioning-done", SCM_BOOL_T);

  /* Copy to mutable list. */
  SCM s = ly_deep_copy (me->get_property ("positions"));
  me->set_property ("positions", s);

  if (ly_car (s) == SCM_BOOL_F)
    {
      // one wonders if such genericity is necessary  --hwn.
      SCM callbacks = me->get_property ("position-callbacks");
      for (SCM i = callbacks; ly_c_pair_p (i); i = ly_cdr (i))
	scm_call_1 (ly_car (i), me->self_scm ());
    }

  set_stem_lengths (me);  
}


/*
  Compute  a first approximation to the beam slope.
 */
MAKE_SCHEME_CALLBACK (Beam, least_squares, 1);
SCM
Beam::least_squares (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  int count = visible_stem_count (me);
  Interval pos (0, 0);
  
  if (count < 1)
    {
      me->set_property ("positions", ly_interval2scm (pos));
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
  
  Interval ideal (Stem::get_stem_info (fvs).ideal_y_
		  + fvs->relative_coordinate (commony, Y_AXIS) -my_y,
		  Stem::get_stem_info (lvs).ideal_y_
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

      /* Simple beams (2 stems) on middle line should be allowed to be
	 slightly sloped.
	 
	 However, if both stems reach middle line,
	 ideal[LEFT] == ideal[RIGHT] and ideal.delta () == 0.

	 For that case, we apply artificial slope */
      if (!ideal[LEFT] && chord.delta () && count == 2)
	{
	  /* FIXME. -> UP */
	  Direction d = (Direction) (sign (chord.delta ()) * UP);
	  pos[d] = get_thickness (me) / 2;
	  pos[-d] = - pos[d];
	}
      else
	{
	  pos = ideal;
	}

      /*
	For broken beams this doesn't work well. In this case, the
	 slope esp. of the first part of a broken beam should predict
	 where the second part goes.
       */
      me->set_property ("least-squares-dy",
			     scm_make_real (pos[RIGHT] - pos[LEFT]));
    }
  else
    {
      Array<Offset> ideals;
      for (int i=0; i < stems.size (); i++)
	{
	  Grob* s = stems[i];
	  if (Stem::is_invisible (s))
	    continue;
	  ideals.push (Offset (x_posns[i],
			       Stem::get_stem_info (s).ideal_y_
			       + s->relative_coordinate (commony, Y_AXIS)
			       - my_y));
	}
      
      minimise_least_squares (&dydx, &y, ideals);

      dy = dydx * dx;
      me->set_property ("least-squares-dy", scm_make_real (dy));
      pos = Interval (y, (y+dy));
    }

  /*
    "position" is relative to the staff.
   */
  scale_drul (&pos,  1/ Staff_symbol_referencer::staff_space (me)); 
  
  me->set_property ("positions", ly_interval2scm (pos));
 
  return SCM_UNSPECIFIED;
}


/*
  We can't combine with previous function, since check concave and
  slope damping comes first.

TODO: we should use the concaveness to control the amount of damping
applied.
  
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

  Drul_array<Real> pos = ly_scm2interval ( me->get_property ("positions"));

  scale_drul (&pos,  Staff_symbol_referencer::staff_space (me));
  
  Real dy = pos[RIGHT] - pos[LEFT];
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
      if (Stem::is_invisible (s))
	continue;

      Direction d = Stem::get_direction (s);

      Real left_y =
	Stem::get_stem_info (s).shortest_y_
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
      
  if (feasible_left_point.is_empty ())
    warning (_ ("no viable initial configuration found: may not find good beam slope"));
  else if (!feasible_left_point.contains (y))
    {
      if (isinf (feasible_left_point[DOWN]))
	y = feasible_left_point[UP] - REGION_SIZE;
      else if (isinf (feasible_left_point[UP]))
	y = feasible_left_point[DOWN]+ REGION_SIZE;
      else
	y = feasible_left_point.center ();
    }
  
  pos = Drul_array<Real> (y, (y+dy));
  scale_drul (&pos, 1/ Staff_symbol_referencer::staff_space (me));
  
  me->set_property ("positions", ly_interval2scm (pos));
  return SCM_UNSPECIFIED;
}

MAKE_SCHEME_CALLBACK (Beam, check_concave, 1);
SCM
Beam::check_concave (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  Link_array<Grob> stems = 
    Pointer_group_interface__extract_grobs (me, (Grob*) 0, "stems");

  Direction beam_dir = CENTER;
  for (int i = 0; i < stems.size ();)
    {
      if (Stem::is_invisible (stems[i]))
	stems.del (i);
      else
	{
	  if (Direction sd = Stem::get_direction (stems[i]))
	    {
	      /*
		Don't do knee beams.
	       */
	      if (beam_dir && sd && sd != beam_dir)
		return SCM_UNSPECIFIED;
	      
	      beam_dir = sd;
	    }
	  i++;
	}
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
  bool is_concave1 = false;
  SCM gap = me->get_property ("concaveness-gap");
  if (ly_c_number_p (gap))
    {
      Real r1 = ly_scm2double (gap);
      Real dy = Stem::chord_start_y (stems.top ())
	- Stem::chord_start_y (stems[0]);

      
      Real slope = dy / (stems.size () - 1);
      
      Real y0 = Stem::chord_start_y (stems[0]);
      for (int i = 1; i < stems.size () - 1; i++)
	{
	  Real c =
	    beam_dir *((Stem::chord_start_y (stems[i]) - y0) - i * slope);
	  if (c - r1 > 0)
	    {
	      is_concave1 = true;
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
  SCM thresh = me->get_property ("concaveness-threshold");
  Real r2 = infinity_f;
  if (!is_concave1 && ly_c_number_p (thresh))
    {
      r2 = ly_scm2double (thresh);
      
      Interval iv;
      iv.add_point (Stem::chord_start_y (stems[0]));
      iv.add_point (Stem::chord_start_y (stems.top ()));
      
      for (int i = 1; i < stems.size () - 1; i++)
	{
	  Real f = Stem::chord_start_y (stems[i]);
	  concaveness2 += ( (f - iv[MAX] ) >? 0) +
	    ( (f - iv[MIN] ) <? 0);
	}
      
      concaveness2 *= beam_dir / (stems.size () - 2);
    }
  
  /* TODO: some sort of damping iso -> plain horizontal */
  if (is_concave1 || concaveness2 > r2)
    {
      Drul_array<Real> pos = ly_scm2interval (me->get_property ("positions"));
      Real r = linear_combination (pos, 0);

      r /= Staff_symbol_referencer::staff_space (me);
      me->set_property ("positions", ly_interval2scm (Drul_array<Real> (r, r)));
      me->set_property ("least-squares-dy", scm_make_real (0));
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

  SCM s = me->get_property ("damping"); 
  int damping = ly_scm2int (s);

  if (damping)
    {
      Drul_array<Real>  pos = ly_scm2interval (me->get_property ("positions"));
      scale_drul (&pos,  Staff_symbol_referencer::staff_space (me));
      
      Real dy = pos[RIGHT] - pos[LEFT];

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

      scale_drul (&pos, 1/Staff_symbol_referencer::staff_space (me));
      
      me->set_property ("positions", ly_interval2scm (pos));
    }
  return SCM_UNSPECIFIED;
}

/*
  Report slice containing the numbers that are both in (car BEAMING)
  and (cdr BEAMING)
 */
Slice
where_are_the_whole_beams (SCM beaming)
{
  Slice l; 
  
  for ( SCM s = ly_car (beaming); ly_c_pair_p (s) ; s = ly_cdr (s))
    {
      if (scm_c_memq (ly_car (s), ly_cdr (beaming)) != SCM_BOOL_F)
	
	l.add_point (ly_scm2int (ly_car (s)));
    }

  return l;
}

/* Return the Y position of the stem-end, given the Y-left, Y-right
   in POS for stem S.  This Y position is relative to S. */
Real
Beam::calc_stem_y (Grob *me, Grob* s, Grob ** common,
		   Real xl, Real xr,
		   Drul_array<Real> pos, bool french) 
{
  Real beam_translation = get_beam_translation (me);

    
  Real r = s->relative_coordinate (common[X_AXIS], X_AXIS) - xl;
  Real dy = pos[RIGHT] - pos[LEFT];
  Real dx = xr - xl;
  Real stem_y_beam0 = (dy && dx
		       ? r / dx
		       * dy
		       : 0) + pos[LEFT];
  
  Direction my_dir = get_grob_direction (s);
  SCM beaming = s->get_property ("beaming");
 
  Real stem_y = stem_y_beam0;
  if (french)
    {
      Slice bm = where_are_the_whole_beams (beaming);
      if (!bm.is_empty ())
	stem_y += beam_translation * bm[-my_dir];
    }
  else
    {
      Slice bm = Stem::beam_multiplicity (s);
      if (!bm.is_empty ())
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

  if (!stems.size ())
    return;
  
  Grob *common[2];
  for (int a = 2; a--;)
    common[a] = common_refpoint_of_array (stems, me, Axis (a));
  
  Drul_array<Real> pos = ly_scm2realdrul (me->get_property ("positions"));
  Real staff_space = Staff_symbol_referencer::staff_space (me);
  scale_drul (&pos,  staff_space);

  bool gap = false;
  Real thick =0.0;
  if (ly_c_number_p (me->get_property ("gap-count"))
      &&ly_scm2int (me->get_property ("gap-count")))
    {
      gap = true;
      thick = get_thickness (me);
    }
      
  // ugh -> use commonx
  Grob * fvs = first_visible_stem (me);
  Grob *lvs = last_visible_stem (me);
    
  Real xl = fvs ? fvs->relative_coordinate (common[X_AXIS], X_AXIS) : 0.0;
  Real xr = lvs ? lvs->relative_coordinate (common[X_AXIS], X_AXIS) : 0.0;
  
  for (int i=0; i < stems.size (); i++)
    {
      Grob* s = stems[i];
      if (Stem::is_invisible (s))
	continue;

      bool french = to_boolean (s->get_property ("french-beaming"));
      Real stem_y = calc_stem_y (me, s, common,
				 xl, xr,
				 pos, french && s != lvs && s!= fvs);

      /*
	Make the stems go up to the end of the beam. This doesn't matter
	for normal beams, but for tremolo beams it looks silly otherwise.
       */
      if (gap)
	stem_y += thick * 0.5 * get_grob_direction (s);

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
	  if ( (d == LEFT && i == 0)
	      || (d == RIGHT && i == stems.size () -1))
	    continue;

	  Grob *st =  stems[i];
	  SCM beaming_prop = st->get_property ("beaming");
	  if (beaming_prop == SCM_EOL ||
	      index_get_cell (beaming_prop, d) == SCM_EOL)
	    {
	      int b = beaming->infos_.elem (i).beams_i_drul_[d];
	      if (i>0
		  && i < stems.size () -1
		  && Stem::is_invisible (st))
		b = b <? beaming->infos_.elem (i).beams_i_drul_[-d];
	      
	      Stem::set_beaming (st, b, d);
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

      if (Stem::is_invisible (s))
	continue;

      /* I can imagine counting those boundaries as a half forced stem,
	 but let's count them full for now. */
      if (abs (Stem::chord_start_y (s)) > 0.1
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
      if (!Stem::is_invisible (stems[i]))
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
      if (!Stem::is_invisible (stems[i]))
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
      if (!Stem::is_invisible (stems[i]))
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
  Axis a = (Axis) ly_scm2int (axis);

  if (ly_c_number_p (rest->get_property ("staff-position")))
    return scm_int2num (0);
  
  assert (a == Y_AXIS);

  Grob *st = unsmob_grob (rest->get_property ("stem"));
  Grob *stem = st;
  if (!stem)
    return scm_make_real (0.0);
  Grob *beam = unsmob_grob (stem->get_property ("beam"));
  if (!beam
      || !Beam::has_interface (beam)
      || !Beam::visible_stem_count (beam))
    return scm_make_real (0.0);

  Drul_array<Real> pos (0, 0);
  SCM s = beam->get_property ("positions");
  if (ly_c_pair_p (s) && ly_c_number_p (ly_car (s)))
    pos = ly_scm2interval (s);
  Real staff_space = Staff_symbol_referencer::staff_space (rest);

  scale_drul (&pos, staff_space);
  

  Real dy = pos[RIGHT] - pos[LEFT];
  
  // ugh -> use commonx
  Real x0 = first_visible_stem (beam)->relative_coordinate (0, X_AXIS);
  Real dx = last_visible_stem (beam)->relative_coordinate (0, X_AXIS) - x0;
  Real dydx = dy && dx ? dy/dx : 0;
  
  Direction d = Stem::get_direction (stem);
  Real stem_y = pos[LEFT] + (stem->relative_coordinate (0, X_AXIS) - x0) * dydx;
  
  Real beam_translation = get_beam_translation (beam);
  Real beam_thickness = Beam::get_thickness (beam);
  
  int beam_count = get_direction_beam_count (beam, d);
  Real height_of_my_beams = beam_thickness / 2
    + (beam_count - 1) * beam_translation;
  Real beam_y = stem_y - d * height_of_my_beams;

  Grob *common_y = rest->common_refpoint (beam, Y_AXIS);

  Real rest_dim = rest->extent (common_y, Y_AXIS)[d];
  Real minimum_distance =
    staff_space * robust_scm2double (rest->get_property ("minimum-distance"), 0.0);

  Real shift = d * ( ((beam_y - d * minimum_distance) - rest_dim) * d  <? 0.0);

  shift /= staff_space;
  Real rad = Staff_symbol_referencer::line_count (rest) * staff_space / 2;

  /* Always move discretely by half spaces */
  shift = ceil (fabs (shift * 2.0)) / 2.0 * sign (shift);

  /* Inside staff, move by whole spaces*/
  if ( (rest->extent (common_y, Y_AXIS)[d] + staff_space * shift) * d
      < rad
      || (rest->extent (common_y, Y_AXIS)[-d] + staff_space * shift) * -d
      < rad)
    shift = ceil (fabs (shift)) *sign (shift);

  return scm_make_real (staff_space * shift);
}

bool
Beam::is_knee (Grob* me)
{
  SCM k = me->get_property ("knee");
  if (ly_c_boolean_p (k))
    return ly_scm2bool (k);

  bool knee = false;
  int d = 0;
  for (SCM s = me->get_property ("stems"); ly_c_pair_p (s); s = ly_cdr (s))
    {
      Direction dir = get_grob_direction (unsmob_grob (ly_car (s)));
      if (d && d != dir)
	{
	  knee = true;
	  break;
	}
      d = dir;
    }
  
  me->set_property ("knee", ly_bool2scm (knee));

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
	       "A beam. \n\n"
	       "The @code{thickness} property is the weight of beams, and is measured "
	       "in  staffspace"
	       ,
	       "knee positioning-done position-callbacks concaveness-gap "
	       "concaveness-threshold dir-function quant-score auto-knee-gap gap "
	       "gap-count chord-tremolo beamed-stem-shorten shorten least-squares-dy "
	       "damping flag-width-function neutral-direction positions space-function "
	       "thickness");


