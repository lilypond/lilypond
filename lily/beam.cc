/*
  beam.cc -- implement Beam

  source file of the GNU LilyPond music typesetter

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

#include "beam.hh"

#include "beaming.hh"
#include "directional-element-interface.hh"
#include "international.hh"
#include "interval-set.hh"
#include "item.hh"
#include "least-squares.hh"
#include "lookup.hh"
#include "misc.hh"
#include "output-def.hh"
#include "pointer-group-interface.hh"
#include "spanner.hh"
#include "staff-symbol-referencer.hh"
#include "stem.hh"
#include "warn.hh"

#if DEBUG_QUANTING
#include "text-interface.hh" // debug output.
#include "font-interface.hh" // debug output.
#endif

void
Beam::add_stem (Grob *me, Grob *s)
{
  if (Stem::get_beam (s))
    {
      programming_error ("Stem already has beam");
      return ;
    }

  Pointer_group_interface::add_grob (me, ly_symbol2scm ("stems"), s);
  s->set_object ("beam", me->self_scm ());
  add_bound_item (dynamic_cast<Spanner *> (me), dynamic_cast<Item *> (s));
}

Real
Beam::get_thickness (Grob *me)
{
  return robust_scm2double (me->get_property ("thickness"), 0)
    * Staff_symbol_referencer::staff_space (me);
}

/* Return the translation between 2 adjoining beams. */
Real
Beam::get_beam_translation (Grob *me)
{
  int beam_count = get_beam_count (me);
  Real staff_space = Staff_symbol_referencer::staff_space (me);
  Real line = Staff_symbol_referencer::line_thickness (me);
  Real thickness = get_thickness (me);
  Real fract = robust_scm2double (me->get_property ("length-fraction"), 1.0);
  
  Real beam_translation = beam_count < 4
    ? (2 * staff_space + line - thickness) / 2.0
    : (3 * staff_space + line - thickness) / 3.0;

  return fract * beam_translation;
}

/* Maximum beam_count. */
int
Beam::get_beam_count (Grob *me)
{
  int m = 0;

  extract_grob_set (me, "stems", stems);
  for (vsize i = 0; i < stems.size (); i++)
    {
      Grob *stem = stems[i];
      m = max (m, (Stem::beam_multiplicity (stem).length () + 1));
    }
  return m;
}


/* After pre-processing all directions should be set.
   Several post-processing routines (stem, slur, script) need stem/beam
   direction.
   Currenly, this means that beam has set all stem's directions.
   [Alternatively, stems could set its own directions, according to
   their beam, during 'final-pre-processing'.] */
MAKE_SCHEME_CALLBACK (Beam, calc_direction, 1);
SCM
Beam::calc_direction (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  /* Beams with less than 2 two stems don't make much sense, but could happen
     when you do

     [r8 c8 r8].

     For a beam that  only has one stem, we try to do some disappearance magic:
     we revert the flag, and move on to The Eternal Engraving Fields. */

  Direction d = CENTER;

  int count = visible_stem_count (me);
  if (count < 2)
    {
      extract_grob_set (me, "stems", stems);
      if (stems.size () == 1)
	{
	  me->warning (_ ("removing beam with less than two stems"));

	  stems[0]->set_object ("beam", SCM_EOL);
	  me->suicide ();

	  return SCM_UNSPECIFIED;
	}
      else if (stems.size () == 0)
	{
	  me->suicide ();
	  return SCM_UNSPECIFIED;
	}
      else 
	{
	  Grob *stem = first_visible_stem (me);

	  /*
	    ugh: stems[0] case happens for chord tremolo.
	  */
	  d = to_dir ((stem ? stem : stems[0])->get_property ("default-direction"));
	}
    }

  if (count >= 1)
    {
      if (!d)
	d = get_default_dir (me);
      
      consider_auto_knees (me);
    }

  if (d)
    {
      set_stem_directions (me, d);
    }
  
  return scm_from_int (d);
}



/* We want a maximal number of shared beams, but if there is choice, we
 * take the one that is closest to the end of the stem. This is for
 * situations like
 *
 *        x
 *       |
 *       |
 *   |===|
 *   |=
 *   |
 *  x
 */
int
position_with_maximal_common_beams (SCM left_beaming, SCM right_beaming,
				    Direction left_dir,
				    Direction right_dir)
{
  Slice lslice = int_list_to_slice (scm_cdr (left_beaming));

  int best_count = 0;
  int best_start = 0;
  for (int i = lslice[-left_dir];
       (i - lslice[left_dir]) * left_dir <= 0; i += left_dir)
    {
      int count = 0;
      for (SCM s = scm_car (right_beaming); scm_is_pair (s); s = scm_cdr (s))
	{
	  int k = -right_dir * scm_to_int (scm_car (s)) + i;
	  if (scm_c_memq (scm_from_int (k), left_beaming) != SCM_BOOL_F)
	    count++;
	}

      if (count >= best_count)
	{
	  best_count = count;
	  best_start = i;
	}
    }

  return best_start;
}

MAKE_SCHEME_CALLBACK(Beam, calc_beaming, 1)
SCM
Beam::calc_beaming (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  
  extract_grob_set (me, "stems", stems);

  Slice last_int;
  last_int.set_empty ();
  
  SCM last_beaming = scm_cons (SCM_EOL, scm_list_1 (scm_from_int (0)));
  Direction last_dir = CENTER;
  for (vsize i = 0; i < stems.size (); i++)
    {
      Grob *this_stem = stems[i];
      SCM this_beaming = this_stem->get_property ("beaming");

      Direction this_dir = get_grob_direction (this_stem);
      if (scm_is_pair (last_beaming) && scm_is_pair (this_beaming))
	{
	  int start_point = position_with_maximal_common_beams
	    (last_beaming, this_beaming,
	     last_dir ? last_dir : this_dir,
	     this_dir);

	  Direction d = LEFT;
	  Slice new_slice;
	  do
	    {
	      new_slice.set_empty ();
	      SCM s = index_get_cell (this_beaming, d);
	      for (; scm_is_pair (s); s = scm_cdr (s))
		{
		  int new_beam_pos
		    = start_point - this_dir * scm_to_int (scm_car (s));

		  new_slice.add_point (new_beam_pos);
		  scm_set_car_x (s, scm_from_int (new_beam_pos));
		}
	    }
	  while (flip (&d) != LEFT);

	  if (!new_slice.is_empty ())
	    last_int = new_slice;
	}
      else
	{
	  SCM s = scm_cdr (this_beaming);
	  for (; scm_is_pair (s); s = scm_cdr (s))
	    {
	      int np = -this_dir * scm_to_int (scm_car (s));
	      scm_set_car_x (s, scm_from_int (np));
	      last_int.add_point (np);
	    }
	}
      
      if (scm_ilength (scm_cdr (this_beaming)) > 0)
	{
	  last_beaming = this_beaming;
	  last_dir = this_dir;
	}
    }

  return SCM_EOL;
}

/*
  I really enjoy spaghetti, but spaghetti should be kept on a plate
  with a little garlic and olive oil. This is too much.

  rewrite-me
*/
MAKE_SCHEME_CALLBACK (Beam, print, 1);
SCM
Beam::print (SCM grob)
{
  Spanner *me = unsmob_spanner (grob);

  extract_grob_set (me, "stems", stems);
  Grob *xcommon = common_refpoint_of_array (stems, me, X_AXIS);

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
      dx = stems.back ()->relative_coordinate (xcommon, X_AXIS) - x0;
    }

  SCM posns = me->get_property ("quantized-positions");
  Drul_array<Real> pos;
  if (!is_number_pair (posns))
    {
      programming_error ("no beam positions?");
      pos = Interval (0, 0);
    }
  else
    pos = ly_scm2realdrul (posns);

  scale_drul (&pos, Staff_symbol_referencer::staff_space (me));

  Real dy = pos[RIGHT] - pos[LEFT];
  Real slope = (dy && dx) ? dy / dx : 0;

  Real thick = get_thickness (me);
  Real bdy = get_beam_translation (me);

  SCM last_beaming = SCM_EOL;
  Real last_xposn = -1;
  Real last_stem_width = -1;

  Real gap_length = robust_scm2double (me->get_property ("gap"), 0.0);

  Stencil the_beam;
  Real lt = me->layout ()->get_dimension (ly_symbol2scm ("linethickness"));

  for (vsize i = 0; i <= stems.size (); i++)
    {
      Grob *stem = (i < stems.size ()) ? stems[i] : 0;

      SCM this_beaming = stem ? stem->get_property ("beaming") : SCM_EOL;
      Real xposn = stem ? stem->relative_coordinate (xcommon, X_AXIS) : 0.0;
      Real stem_width = stem ? robust_scm2double (stem->get_property ("thickness"), 1.0) * lt : 0;
      Direction stem_dir = stem ? to_dir (stem->get_property ("direction")) : CENTER;
      /*
	We do the space left of ST, with lfliebertjes pointing to the
	right from the left stem, and rfliebertjes pointing left from
	right stem.
      */
      SCM left = (i > 0) ? scm_cdr (last_beaming) : SCM_EOL;
      SCM right = stem ? scm_car (this_beaming) : SCM_EOL;

      std::vector<int> full_beams;
      std::vector<int> lfliebertjes;
      std::vector<int> rfliebertjes;

      for (SCM s = left;
	   scm_is_pair (s); s = scm_cdr (s))
	{
	  int b = scm_to_int (scm_car (s));
	  if (scm_c_memq (scm_car (s), right) != SCM_BOOL_F)
	    full_beams.push_back (b);
	  else
	    lfliebertjes.push_back (b);
	}
      for (SCM s = right;
	   scm_is_pair (s); s = scm_cdr (s))
	{
	  int b = scm_to_int (scm_car (s));
	  if (scm_c_memq (scm_car (s), left) == SCM_BOOL_F)
	    rfliebertjes.push_back (b);
	}

      Drul_array<Real> break_overshoot
	= robust_scm2drul (me->get_property ("break-overshoot"),
			   Drul_array<Real> (-0.5, 0.0));

      Real w = (i > 0 && stem)
	? (xposn - last_xposn)
	: break_overshoot[ (i == 0) ? LEFT : RIGHT];

      Real stem_offset = 0.0;
      if (i > 0)
	{
	  w += last_stem_width / 2;
	  stem_offset = -last_stem_width / 2;
	}

      if (stem)
	w += stem_width / 2;

      Real blot = me->layout ()->get_dimension (ly_symbol2scm ("blotdiameter"));
      Stencil whole = Lookup::beam (slope, w, thick, blot);
      Stencil gapped;

      int gap_count = 0;
      if (scm_is_number (me->get_property ("gap-count")))
	{
	  gap_count = scm_to_int (me->get_property ("gap-count"));
	  gapped = Lookup::beam (slope, w - 2 * gap_length, thick, blot);

	  vector_sort (full_beams, default_compare);
	  if (stem_dir == UP)
	    full_beams.reverse ();
	}

      int k = 0;
      for (vsize j = full_beams.size (); j--;)
	{
	  Stencil b (whole);

	  if (k++ < gap_count)
	    {
	      b = gapped;
	      b.translate_axis (gap_length, X_AXIS);
	    }
	  b.translate_axis (last_xposn - x0 + stem_offset, X_AXIS);
	  b.translate_axis (slope * (last_xposn - x0) + bdy * full_beams[j], Y_AXIS);

	  the_beam.add_stencil (b);
	}

      if (lfliebertjes.size () || rfliebertjes.size ())
	{
	  Real nw_f;

	  if (stem)
	    {
	      int t = Stem::duration_log (stem);
	      // ugh. hardcoded.
	      if (t == 1)
		nw_f = 1.98;
	      else
		nw_f = 1.32;
	    }
	  else
	    nw_f = break_overshoot[RIGHT] / 2;

	  /* Half beam should be one note-width,
	     but let's make sure two half-beams never touch */
	  Real lw = nw_f;
	  Real rw = nw_f;
	  if (i > 0)
	    rw = min (nw_f, ((xposn - last_xposn) / 2));
	  else
	    {
	      if (me->get_bound (LEFT)->break_status_dir ())
		rw = xposn - me->get_bound (LEFT)->extent (xcommon, X_AXIS)[RIGHT]
		  + break_overshoot[LEFT];
	      else
		rw = 1.0; 	// ugh.
	    }
	  
	  if (stem)
	    lw = min (nw_f, ((xposn - last_xposn) / 2));
	  else
	    {
	      lw = me->get_bound (RIGHT)->relative_coordinate (xcommon, X_AXIS)
		- last_xposn
		+ break_overshoot[RIGHT];
	    }
	  rw += stem_width / 2;
	  lw += last_stem_width / 2;

	  Stencil rhalf = Lookup::beam (slope, rw, thick, blot);
	  Stencil lhalf = Lookup::beam (slope, lw, thick, blot);
	  for (vsize j = lfliebertjes.size (); j--;)
	    {
	      Stencil b (lhalf);
	      b.translate_axis (last_xposn - x0 - last_stem_width /2,
				X_AXIS);
	      b.translate_axis (slope * (last_xposn - x0)
				+ bdy * lfliebertjes[j],
				Y_AXIS);
	      the_beam.add_stencil (b);
	    }
	  for (vsize j = rfliebertjes.size (); j--;)
	    {
	      Stencil b (rhalf);
	      b.translate_axis (xposn - x0 - rw + stem_width / 2, X_AXIS);
	      b.translate_axis (slope * (xposn - x0 - rw)
				+ bdy * rfliebertjes[j], Y_AXIS);
	      the_beam.add_stencil (b);
	    }
	}

      last_xposn = xposn;
      last_stem_width = stem_width;
      last_beaming = this_beaming;
    }

  the_beam.translate_axis (x0 - me->relative_coordinate (xcommon, X_AXIS),
			   X_AXIS);
  the_beam.translate_axis (pos[LEFT], Y_AXIS);

#if (DEBUG_QUANTING)
  SCM quant_score = me->get_property ("quant-score");
  SCM debug = me->layout ()->lookup_variable (ly_symbol2scm ("debug-beam-quanting"));
  if (to_boolean (debug) && scm_is_string (quant_score))
    {

      /*
	This code prints the demerits for each beam. Perhaps this
	should be switchable for those who want to twiddle with the
	parameters.
      */
      std::string str;
      SCM properties = Font_interface::text_font_alist_chain (me);

      Direction stem_dir = stems.size () ? to_dir (stems[0]->get_property ("direction")) : UP;

      Stencil score = *unsmob_stencil (Text_interface::interpret_markup
				    (me->layout ()->self_scm (), properties, quant_score));

      if (!score.is_empty ())
	the_beam.add_at_edge (Y_AXIS, stem_dir, score, 1.0, 0);
    }
#endif

  return the_beam.smobbed_copy ();
}

Direction
Beam::get_default_dir (Grob *me)
{
  Drul_array<int> total;
  total[UP] = total[DOWN] = 0;
  Drul_array<int> count;
  count[UP] = count[DOWN] = 0;

  extract_grob_set (me, "stems", stems);

  for (vsize i = 0; i < stems.size (); i++)
    {
      Grob *s = stems[i];
      Direction stem_dir = CENTER;
      SCM stem_dir_scm = s->get_property_data (ly_symbol2scm ("direction"));
      if (is_direction (stem_dir_scm))
	stem_dir = to_dir (stem_dir_scm);
      else
	stem_dir = to_dir (s->get_property ("default-direction"));

      if (!stem_dir)
	stem_dir = to_dir (s->get_property ("neutral-direction"));

      if (stem_dir)
	{
	  count[stem_dir] ++;
	  total[stem_dir] += max (int (- stem_dir * Stem::head_positions (s) [-stem_dir]), 0);
	}
    }

  Direction dir = CENTER;
  Direction d = CENTER;
  if ((d = (Direction) sign (count[UP] - count[DOWN])))
    dir = d;
  else if (count[UP]
	   && count[DOWN]
	   && (d = (Direction)  sign (total[UP] / count[UP] - total[DOWN]/count[DOWN])))
    dir = d;
  else if ((d = (Direction)  sign (total[UP] - total[DOWN])))
    dir = d;
  else
    dir = to_dir (me->get_property ("neutral-direction"));
  
  return dir;
}

/* Set all stems with non-forced direction to beam direction.
   Urg: non-forced should become `without/with unforced' direction,
   once stem gets cleaned-up. */
void
Beam::set_stem_directions (Grob *me, Direction d)
{
  extract_grob_set (me, "stems", stems);

  for (vsize i = 0; i < stems.size (); i++)
    {
      Grob *s = stems[i];

      SCM forcedir = s->get_property_data (ly_symbol2scm ("direction"));
      if (!to_dir (forcedir))
	set_grob_direction (s, d);
    }
}

/*
  Only try horizontal beams for knees.  No reliable detection of
  anything else is possible here, since we don't know funky-beaming
  settings, or X-distances (slopes!)  People that want sloped
  knee-beams, should set the directions manually.


  TODO:

  this routine should take into account the stemlength scoring
  of a possible knee/nonknee beam.
*/
void
Beam::consider_auto_knees (Grob *me)
{
  SCM scm = me->get_property ("auto-knee-gap");
  if (!scm_is_number (scm))
    return;

  Interval_set gaps;

  gaps.set_full ();

  extract_grob_set (me, "stems", stems);

  Grob *common = common_refpoint_of_array (stems, me, Y_AXIS);
  Real staff_space = Staff_symbol_referencer::staff_space (me);

  std::vector<Interval> head_extents_array;
  for (vsize i = 0; i < stems.size (); i++)
    {
      Grob *stem = stems[i];
      if (Stem::is_invisible (stem))
	continue;

      Interval head_extents = Stem::head_positions (stem);
      if (!head_extents.is_empty ())
	{
	  head_extents[LEFT] += -1;
	  head_extents[RIGHT] += 1;
	  head_extents *= staff_space * 0.5;

	  /*
	    We could subtract beam Y position, but this routine only
	    sets stem directions, a constant shift does not have an
	    influence.
	  */
	  head_extents += stem->relative_coordinate (common, Y_AXIS);

	  if (to_dir (stem->get_property_data (ly_symbol2scm ("direction"))))
	    {
	      Direction stemdir = to_dir (stem->get_property ("direction"));
	      head_extents[-stemdir] = -stemdir * infinity_f;
	    }
	}
      head_extents_array.push_back (head_extents);

      gaps.remove_interval (head_extents);
    }

  Interval max_gap;
  Real max_gap_len = 0.0;

  for (vsize i = gaps.allowed_regions_.size () -1; i != VPOS ;i--)
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

  Real beam_translation = get_beam_translation (me);
  Real beam_thickness = Beam::get_thickness (me);
  int beam_count = Beam::get_beam_count (me);
  Real height_of_beams = beam_thickness / 2
    + (beam_count - 1) * beam_translation;
  Real threshold = scm_to_double (scm) + height_of_beams;

  if (max_gap_len > threshold)
    {
      int j = 0;
      for (vsize i = 0; i < stems.size (); i++)
	{
	  Grob *stem = stems[i];
	  if (Stem::is_invisible (stem))
	    continue;

	  Interval head_extents = head_extents_array[j++];

	  Direction d = (head_extents.center () < max_gap.center ())
	    ? UP : DOWN;

	  stem->set_property ("direction", scm_from_int (d));

	  head_extents.intersect (max_gap);
	  assert (head_extents.is_empty () || head_extents.length () < 1e-6);
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
set_minimum_dy (Grob *me, Real *dy)
{
  if (*dy)
    {
      /*
	If dy is smaller than the smallest quant, we
	get absurd direction-sign penalties.
      */

      Real ss = Staff_symbol_referencer::staff_space (me);
      Real thickness = Beam::get_thickness (me) / ss;
      Real slt = Staff_symbol_referencer::line_thickness (me) / ss;
      Real sit = (thickness - slt) / 2;
      Real inter = 0.5;
      Real hang = 1.0 - (thickness - slt) / 2;

      *dy = sign (*dy) * max (fabs (*dy),
			      min (min (sit, inter), hang));
    }
}

  

MAKE_SCHEME_CALLBACK(Beam, calc_stem_shorten, 1)
SCM
Beam::calc_stem_shorten (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  
  /*
    shortening looks silly for x staff beams
  */
  if (is_knee (me))
    return scm_from_int (0);

  Real forced_fraction = 1.0 * forced_stem_count (me)
    / visible_stem_count (me);

  int beam_count = get_beam_count (me);

  SCM shorten_list = me->get_property ("beamed-stem-shorten");
  if (shorten_list == SCM_EOL)
    return scm_from_int (0);

  Real staff_space = Staff_symbol_referencer::staff_space (me);

  SCM shorten_elt
    = robust_list_ref (beam_count -1, shorten_list);
  Real shorten = scm_to_double (shorten_elt) * staff_space;

  shorten *= forced_fraction;

  
  if (shorten)
    return scm_from_double (shorten);

  return scm_from_double (0.0);
}



/*
  Compute a first approximation to the beam slope.
*/
MAKE_SCHEME_CALLBACK (Beam, calc_least_squares_positions, 2);
SCM
Beam::calc_least_squares_positions (SCM smob, SCM posns)
{
  (void) posns;
  
  Grob *me = unsmob_grob (smob);

  int count = visible_stem_count (me);
  Interval pos (0,0);
  if (count < 1)
    return ly_interval2scm (pos);
  
  std::vector<Real> x_posns;
  extract_grob_set (me, "stems", stems);
  Grob *commonx = common_refpoint_of_array (stems, me, X_AXIS);
  Grob *commony = common_refpoint_of_array (stems, me, Y_AXIS);

  Real my_y = me->relative_coordinate (commony, Y_AXIS);

  Grob *fvs = first_visible_stem (me);
  Grob *lvs = last_visible_stem (me);

  Interval ideal (Stem::get_stem_info (fvs).ideal_y_
		  + fvs->relative_coordinate (commony, Y_AXIS) -my_y,
		  Stem::get_stem_info (lvs).ideal_y_
		  + lvs->relative_coordinate (commony, Y_AXIS) - my_y);

  Real x0 = first_visible_stem (me)->relative_coordinate (commonx, X_AXIS);
  for (vsize i = 0; i < stems.size (); i++)
    {
      Grob *s = stems[i];

      Real x = s->relative_coordinate (commonx, X_AXIS) - x0;
      x_posns.push_back (x);
    }
  Real dx = last_visible_stem (me)->relative_coordinate (commonx, X_AXIS) - x0;

  Real y = 0;
  Real slope = 0;
  Real dy = 0;
  Real ldy = 0.0;
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
	  pos[-d] = -pos[d];
	}
      else
	pos = ideal;

      /*
	For broken beams this doesn't work well. In this case, the
	slope esp. of the first part of a broken beam should predict
	where the second part goes.
      */
      ldy = pos[RIGHT] - pos[LEFT];
    }
  else
    {
      std::vector<Offset> ideals;
      for (vsize i = 0; i < stems.size (); i++)
	{
	  Grob *s = stems[i];
	  if (Stem::is_invisible (s))
	    continue;
	  ideals.push_back (Offset (x_posns[i],
			       Stem::get_stem_info (s).ideal_y_
			       + s->relative_coordinate (commony, Y_AXIS)
			       - my_y));
	}

      minimise_least_squares (&slope, &y, ideals);

      dy = slope * dx;

      set_minimum_dy (me, &dy);

      ldy = dy;
      pos = Interval (y, (y + dy));
    }

  /*
    "position" is relative to the staff.
  */
  scale_drul (&pos, 1 / Staff_symbol_referencer::staff_space (me));

  me->set_property ("least-squares-dy",  scm_from_double (ldy));
  return ly_interval2scm (pos);
}

/*
  We can't combine with previous function, since check concave and
  slope damping comes first.

  TODO: we should use the concaveness to control the amount of damping
  applied.
*/
MAKE_SCHEME_CALLBACK (Beam, shift_region_to_valid, 2);
SCM
Beam::shift_region_to_valid (SCM grob, SCM posns)
{
  Grob *me = unsmob_grob (grob);
  /*
    Code dup.
  */
  std::vector<Real> x_posns;
  extract_grob_set (me, "stems", stems);
  Grob *commonx = common_refpoint_of_array (stems, me, X_AXIS);
  Grob *commony = common_refpoint_of_array (stems, me, Y_AXIS);

  Grob *fvs = first_visible_stem (me);

  if (!fvs)
    return posns;

  Real x0 = fvs->relative_coordinate (commonx, X_AXIS);
  for (vsize i = 0; i < stems.size (); i++)
    {
      Grob *s = stems[i];

      Real x = s->relative_coordinate (commonx, X_AXIS) - x0;
      x_posns.push_back (x);
    }

  Grob *lvs = last_visible_stem (me);
  if (!lvs)
    return posns;

  Real dx = lvs->relative_coordinate (commonx, X_AXIS) - x0;

  Drul_array<Real> pos = ly_scm2interval (posns);
  

  scale_drul (&pos, Staff_symbol_referencer::staff_space (me));

  Real dy = pos[RIGHT] - pos[LEFT];
  Real y = pos[LEFT];
  Real slope = dx ? (dy / dx) : 0.0;

  /*
    Shift the positions so that we have a chance of finding good
    quants (i.e. no short stem failures.)
  */
  Interval feasible_left_point;
  feasible_left_point.set_full ();
  for (vsize i = 0; i < stems.size (); i++)
    {
      Grob *s = stems[i];
      if (Stem::is_invisible (s))
	continue;

      Direction d = get_grob_direction (s);

      Real left_y
	= Stem::get_stem_info (s).shortest_y_
	- slope * x_posns [i];

      /*
	left_y is now relative to the stem S. We want relative to
	ourselves, so translate:
      */
      left_y
	+= + s->relative_coordinate (commony, Y_AXIS)
	- me->relative_coordinate (commony, Y_AXIS);

      Interval flp;
      flp.set_full ();
      flp[-d] = left_y;

      feasible_left_point.intersect (flp);
    }

  if (feasible_left_point.is_empty ())
    warning (_ ("no viable initial configuration found: may not find good beam slope"));
  else if (!feasible_left_point.contains (y))
    {
      const int REGION_SIZE = 2; // UGH UGH
      if (isinf (feasible_left_point[DOWN]))
	y = feasible_left_point[UP] - REGION_SIZE;
      else if (isinf (feasible_left_point[UP]))
	y = feasible_left_point[DOWN]+ REGION_SIZE;
      else
	y = feasible_left_point.center ();
    }

  pos = Drul_array<Real> (y, (y + dy));
  scale_drul (&pos, 1 / Staff_symbol_referencer::staff_space (me));

  return ly_interval2scm (pos);
}

/* This neat trick is by Werner Lemberg,
   damped = tanh (slope)
   corresponds with some tables in [Wanske] CHECKME */
MAKE_SCHEME_CALLBACK (Beam, slope_damping, 2);
SCM
Beam::slope_damping (SCM smob, SCM posns)
{
  Grob *me = unsmob_grob (smob);
  Drul_array<Real> pos = ly_scm2interval (posns);

  if (visible_stem_count (me) <= 1)
    return posns;

  
  SCM s = me->get_property ("damping");
  Real damping = scm_to_double (s);
  Real concaveness = robust_scm2double (me->get_property ("concaveness"), 0.0);
  if (concaveness >= 10000)
    {
      pos[LEFT] = pos[RIGHT];
      me->set_property ("least-squares-dy", scm_from_double (0));
      damping = 0;
    }
  
  if (damping)
    {
      scale_drul (&pos, Staff_symbol_referencer::staff_space (me));

      Real dy = pos[RIGHT] - pos[LEFT];

      Grob *fvs = first_visible_stem (me);
      Grob *lvs = last_visible_stem (me);

      Grob *commonx = fvs->common_refpoint (lvs, X_AXIS);

      Real dx = last_visible_stem (me)->relative_coordinate (commonx, X_AXIS)
	- first_visible_stem (me)->relative_coordinate (commonx, X_AXIS);

      Real slope = dy && dx ? dy / dx : 0;

      slope = 0.6 * tanh (slope) / (damping + concaveness);

      Real damped_dy = slope * dx;

      set_minimum_dy (me, &damped_dy);

      pos[LEFT] += (dy - damped_dy) / 2;
      pos[RIGHT] -= (dy - damped_dy) / 2;

      scale_drul (&pos, 1 / Staff_symbol_referencer::staff_space (me));
    }

  return ly_interval2scm (pos);
}

/*
  Report slice containing the numbers that are both in (car BEAMING)
  and (cdr BEAMING)
*/
Slice
where_are_the_whole_beams (SCM beaming)
{
  Slice l;

  for (SCM s = scm_car (beaming); scm_is_pair (s); s = scm_cdr (s))
    {
      if (scm_c_memq (scm_car (s), scm_cdr (beaming)) != SCM_BOOL_F)

	l.add_point (scm_to_int (scm_car (s)));
    }

  return l;
}

/* Return the Y position of the stem-end, given the Y-left, Y-right
   in POS for stem S.  This Y position is relative to S. */
Real
Beam::calc_stem_y (Grob *me, Grob *s, Grob ** common,
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
	stem_y += bm[my_dir] * beam_translation;
    }

  Real id = me->relative_coordinate (common[Y_AXIS], Y_AXIS)
    - s->relative_coordinate (common[Y_AXIS], Y_AXIS);

  return stem_y + id;
}

/*
  Hmm.  At this time, beam position and slope are determined.  Maybe,
  stem directions and length should set to relative to the chord's
  position of the beam.  */
MAKE_SCHEME_CALLBACK(Beam, set_stem_lengths, 1); 
SCM
Beam::set_stem_lengths (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  /* trigger callback. */
  (void) me->get_property ("direction");

  SCM posns = me->get_property ("positions");
  
  extract_grob_set (me, "stems", stems);
  if (!stems.size ())
    return posns;

  Grob *common[2];
  for (int a = 2; a--;)
    common[a] = common_refpoint_of_array (stems, me, Axis (a));

  Drul_array<Real> pos = ly_scm2realdrul (posns);
  Real staff_space = Staff_symbol_referencer::staff_space (me);
  scale_drul (&pos, staff_space);

  bool gap = false;
  Real thick = 0.0;
  if (scm_is_number (me->get_property ("gap-count"))
      && scm_to_int (me->get_property ("gap-count")))
    {
      gap = true;
      thick = get_thickness (me);
    }

  Grob *fvs = first_visible_stem (me);
  Grob *lvs = last_visible_stem (me);

  Real xl = fvs ? fvs->relative_coordinate (common[X_AXIS], X_AXIS) : 0.0;
  Real xr = lvs ? lvs->relative_coordinate (common[X_AXIS], X_AXIS) : 0.0;

  for (vsize i = 0; i < stems.size (); i++)
    {
      Grob *s = stems[i];
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

      Stem::set_stemend (s, 2 * stem_y / staff_space);
    }

  return posns;
}

void
Beam::set_beaming (Grob *me, Beaming_info_list const *beaming)
{
  extract_grob_set (me, "stems", stems);

  Direction d = LEFT;
  for (vsize i = 0; i < stems.size (); i++)
    {
      /*
	Don't overwrite user settings.
      */
      do
	{
	  Grob *stem = stems[i];
	  SCM beaming_prop = stem->get_property ("beaming");
	  if (beaming_prop == SCM_EOL
	      || index_get_cell (beaming_prop, d) == SCM_EOL)
	    {
	      int b = beaming->infos_.elem (i).beams_i_drul_[d];
	      if (i > 0
		  && i < stems.size () -1
		  && Stem::is_invisible (stem))
		b = min (b, beaming->infos_.elem (i).beams_i_drul_[-d]);

	      Stem::set_beaming (stem, b, d);
	    }
	}
      while (flip (&d) != LEFT);
    }
}

int
Beam::forced_stem_count (Grob *me)
{
  extract_grob_set (me, "stems", stems);

  int f = 0;
  for (vsize i = 0; i < stems.size (); i++)
    {
      Grob *s = stems[i];

      if (Stem::is_invisible (s))
	continue;

      /* I can imagine counting those boundaries as a half forced stem,
	 but let's count them full for now. */
      Direction defdir = to_dir (s->get_property ("default-direction"));
      
      if (abs (Stem::chord_start_y (s)) > 0.1
	  && defdir
	  && get_grob_direction (s) != defdir)
	f++;
    }
  return f;
}

int
Beam::visible_stem_count (Grob *me)
{
  extract_grob_set (me, "stems", stems);
  int c = 0;
  for (vsize i = stems.size (); i--;)
    {
      if (!Stem::is_invisible (stems[i]))
	c++;
    }
  return c;
}

Grob *
Beam::first_visible_stem (Grob *me)
{
  extract_grob_set (me, "stems", stems);

  for (vsize i = 0; i < stems.size (); i++)
    {
      if (!Stem::is_invisible (stems[i]))
	return stems[i];
    }
  return 0;
}

Grob *
Beam::last_visible_stem (Grob *me)
{
  extract_grob_set (me, "stems", stems);

  for (vsize i = stems.size (); i--;)
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
Beam::rest_collision_callback (SCM smob, SCM prev_offset)
{
  Grob *rest = unsmob_grob (smob);
  if (scm_is_number (rest->get_property ("staff-position")))
    return scm_from_int (0);

  Real offset = robust_scm2double (prev_offset, 0.0);
  
  Grob *st = unsmob_grob (rest->get_object ("stem"));
  Grob *stem = st;
  if (!stem)
    return scm_from_double (0.0);
  Grob *beam = unsmob_grob (stem->get_object ("beam"));
  if (!beam
      || !Beam::has_interface (beam)
      || !Beam::visible_stem_count (beam))
    return scm_from_double (0.0);

  Drul_array<Real> pos (0, 0);
  SCM s = beam->get_property ("positions");
  if (scm_is_pair (s) && scm_is_number (scm_car (s)))
    pos = ly_scm2interval (s);
  else
    programming_error ("positions property should always be pair of numbers.");

  Real staff_space = Staff_symbol_referencer::staff_space (rest);

  scale_drul (&pos, staff_space);

  Real dy = pos[RIGHT] - pos[LEFT];

  Drul_array<Grob*> visible_stems (first_visible_stem (beam),
				   last_visible_stem (beam));
  extract_grob_set (beam, "stems", stems);
  
  Grob *common = common_refpoint_of_array (stems, beam, X_AXIS);
  
  Real x0 = visible_stems[LEFT]->relative_coordinate (common, X_AXIS);
  Real dx = visible_stems[RIGHT]->relative_coordinate (common, X_AXIS) - x0;
  Real slope = dy && dx ? dy / dx : 0;

  Direction d = get_grob_direction (stem);
  Real stem_y = pos[LEFT]
    + (stem->relative_coordinate (common, X_AXIS) - x0) * slope;

  Real beam_translation = get_beam_translation (beam);
  Real beam_thickness = Beam::get_thickness (beam);

  /*
    TODO: this is not strictly correct for 16th knee beams.
  */
  int beam_count
    = Stem::beam_multiplicity (stem).length () + 1;

  Real height_of_my_beams = beam_thickness / 2
    + (beam_count - 1) * beam_translation;
  Real beam_y = stem_y - d * height_of_my_beams;

  Grob *common_y = rest->common_refpoint (beam, Y_AXIS);
  Interval rest_extent = rest->extent (common_y, Y_AXIS);
  rest_extent.translate (offset);
  
  Real rest_dim = rest_extent[d];
  Real minimum_distance
    = staff_space * (robust_scm2double (stem->get_property ("stemlet-length"), 0.0)
		     + robust_scm2double (rest->get_property ("minimum-distance"), 0.0));

  Real shift = d * min (((beam_y - d * minimum_distance) - rest_dim) * d, 0.0);

  shift /= staff_space;
  Real rad = Staff_symbol_referencer::line_count (rest) * staff_space / 2;

  /* Always move discretely by half spaces */
  shift = ceil (fabs (shift * 2.0)) / 2.0 * sign (shift);

  /* Inside staff, move by whole spaces*/
  if ((rest_extent[d] + staff_space * shift) * d
      < rad
      || (rest_extent[-d] + staff_space * shift) * -d
      < rad)
    shift = ceil (fabs (shift)) * sign (shift);

  return scm_from_double (staff_space * shift);
}

bool
Beam::is_knee (Grob *me)
{
  SCM k = me->get_property ("knee");
  if (scm_is_bool (k))
    return ly_scm2bool (k);

  bool knee = false;
  int d = 0;
  extract_grob_set (me, "stems", stems);
  for (vsize i = stems.size (); i--;)
    {
      Direction dir = get_grob_direction (stems[i]);
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
Beam::get_direction_beam_count (Grob *me, Direction d)
{
  extract_grob_set (me, "stems", stems);
  int bc = 0;

  for (vsize i = stems.size (); i--;)
    {
      /*
	Should we take invisible stems into account?
      */
      if (get_grob_direction (stems[i]) == d)
	bc = max (bc, (Stem::beam_multiplicity (stems[i]).length () + 1));
    }

  return bc;
}

ADD_INTERFACE (Beam,
	       "beam-interface",

	       "A beam. \n\n"
	       "The @code{thickness} property is the weight of beams, "
	       "measured in staffspace.  The @code{direction} "
	       "property is not user-serviceable. Use "
	       "the @code{direction} property of @code{Stem} instead. "

	       ,
	       
	       /* properties */
	       "auto-knee-gap "
	       "beamed-stem-shorten "
	       "beaming "
	       "break-overshoot "
	       "chord-tremolo "
	       "concaveness "
	       "damping "
	       "details "
	       "direction " 
	       "gap "
	       "gap-count "
	       "inspect-quants "
	       "knee "
	       "length-fraction "
	       "least-squares-dy "
	       "neutral-direction "
	       "positions "
	       "quant-score "
	       "quantized-positions "
	       "shorten "
	       "stems "
	       "thickness "
	       );
