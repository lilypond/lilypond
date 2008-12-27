/*
  beam.cc -- implement Beam

  source file of the GNU LilyPond music typesetter

  (c) 1997--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

#include "beaming-pattern.hh"
#include "directional-element-interface.hh"
#include "main.hh"
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
#include "grob-array.hh"

#if DEBUG_BEAM_SCORING
#include "text-interface.hh" // debug output.
#include "font-interface.hh" // debug output.
#endif

#include <map>


Beam_stem_segment::Beam_stem_segment ()
{
  max_connect_ = 1000;		// infinity
  stem_ = 0;
  width_ = 0.0;
  stem_x_ = 0.0;
  rank_ = 0;
  stem_index_ = 0;
  dir_ = CENTER;
}

Beam_segment::Beam_segment ()
{
  vertical_count_ = 0;
}

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

MAKE_SCHEME_CALLBACK (Beam, calc_normal_stems, 1);
SCM
Beam::calc_normal_stems (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  
  extract_grob_set (me, "stems", stems);
  SCM val = Grob_array::make_array ();
  Grob_array *ga = unsmob_grob_array (val);
  for (vsize i = 0; i < stems.size ();  i++)
    if (Stem::is_normal_stem (stems[i]))
      ga->add (stems[i]);
  
  return val;  
}

MAKE_SCHEME_CALLBACK (Beam, calc_direction, 1);
SCM
Beam::calc_direction (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  /* Beams with less than 2 two stems don't make much sense, but could happen
     when you do

     r8[ c8 r8]

  */

  Direction dir = CENTER;

  int count = normal_stem_count (me);
  if (count < 2)
    {
      extract_grob_set (me, "stems", stems);
      if (stems.size () == 0)
	{
	  me->warning (_ ("removing beam with no stems"));
	  me->suicide ();

	  return SCM_UNSPECIFIED;
	}
      else 
	{
	  Grob *stem = first_normal_stem (me);

	  /*
	    This happens for chord tremolos.
	  */
	  if (!stem)
	    stem = stems[0];
	  
	  if (is_direction (stem->get_property_data ("direction"))) 
	    dir = to_dir (stem->get_property_data ("direction"));
	  else
	    dir = to_dir (stem->get_property ("default-direction"));
	}
    }

  if (count >= 1)
    {
      if (!dir)
	dir = get_default_dir (me);
      
      consider_auto_knees (me);
    }

  if (dir)
    {
      set_stem_directions (me, dir);
    }
  
  return scm_from_int (dir);
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

MAKE_SCHEME_CALLBACK (Beam, calc_beaming, 1)
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
	  /*
	    FIXME: what's this for? 
	   */
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

bool
operator <(Beam_stem_segment const &a,
	   Beam_stem_segment const &b)
{
  return a.rank_ < b.rank_;
}

typedef map<int, vector<Beam_stem_segment> >  Position_stem_segments_map; 

vector<Beam_segment>
Beam::get_beam_segments (Grob *me_grob, Grob **common)
{
  /* ugh, this has a side-effect that we need to ensure that
     Stem #'beaming is correct */
  (void) me_grob->get_property ("beaming");

  Spanner *me = dynamic_cast<Spanner*> (me_grob);

  extract_grob_set (me, "stems", stems);
  Grob *commonx = common_refpoint_of_array (stems, me, X_AXIS);

  commonx = me->get_bound (LEFT)->common_refpoint (commonx, X_AXIS);
  commonx = me->get_bound (RIGHT)->common_refpoint (commonx, X_AXIS);

  *common = commonx;
  
  int gap_count = robust_scm2int (me->get_property ("gap-count"), 0);
  Real gap_length = robust_scm2double (me->get_property ("gap"), 0.0);

  Position_stem_segments_map stem_segments;
  Real lt = me->layout ()->get_dimension (ly_symbol2scm ("line-thickness"));

  Slice ranks;
  for (vsize i = 0; i < stems.size (); i++)
    {
      Grob *stem = stems[i];
      Real stem_width = robust_scm2double (stem->get_property ("thickness"), 1.0) * lt;
      Real stem_x = stem->relative_coordinate (commonx, X_AXIS);
      SCM beaming = stem->get_property ("beaming");
      Direction d = LEFT;
      do
	{
	  for (SCM s = index_get_cell (beaming, d);
	       scm_is_pair (s); s = scm_cdr (s))
	    {
	      if (!scm_is_integer (scm_car (s)))
		continue;

	      int beam_rank = scm_to_int (scm_car (s));
	      ranks.add_point (beam_rank);
	    }
	  
	  for (SCM s = index_get_cell (beaming, d);
	       scm_is_pair (s); s = scm_cdr (s))
	    {
	      if (!scm_is_integer (scm_car (s)))
		continue;
	  
	      int beam_rank = scm_to_int (scm_car (s));
	      Beam_stem_segment seg;
	      seg.stem_ = stem;
	      seg.stem_x_ = stem_x;
	      seg.rank_ = 2 * i + (d+1)/2;
	      seg.width_ = stem_width;
	      seg.stem_index_ = i;
	      seg.dir_ = d;
	      seg.max_connect_ = robust_scm2int (stem->get_property ("max-beam-connect"), 1000);
	      
	      Direction stem_dir = get_grob_direction (stem);
	      
	      seg.gapped_
		= (stem_dir * beam_rank < (stem_dir * ranks[-stem_dir] + gap_count));
	      stem_segments[beam_rank].push_back (seg);
	    }
	}
      while (flip (&d) != LEFT);
    }

  Drul_array<Real> break_overshoot
    = robust_scm2drul (me->get_property ("break-overshoot"),
		       Drul_array<Real> (-0.5, 0.0));

  vector<Beam_segment> segments;
  for (Position_stem_segments_map::const_iterator i (stem_segments.begin ());
       i != stem_segments.end (); i++)
    {
      vector<Beam_stem_segment> segs = (*i).second;
      vector_sort (segs, less<Beam_stem_segment> ());

      Beam_segment current;

      int vertical_count =  (*i).first;
      for (vsize j = 0; j < segs.size (); j++)
	{
	  /*
	    event_dir == LEFT: left edge of a beamsegment.
	   */
	  Direction event_dir = LEFT;
	  do
	    {
	      bool on_line_bound = (segs[j].dir_ == LEFT) ? segs[j].stem_index_ == 0
		: segs[j].stem_index_ == stems.size() - 1;
	      bool on_beam_bound = (event_dir == LEFT) ? j == 0 :
		j == segs.size () - 1;
	      bool inside_stem = (event_dir == LEFT)
		? segs[j].stem_index_ > 0
		: segs[j].stem_index_ + 1 < stems.size () ;
		      
	      bool event = on_beam_bound
		|| abs (segs[j].rank_ - segs[j+event_dir].rank_) > 1
		|| (abs (vertical_count) >= segs[j].max_connect_
		    || abs (vertical_count) >= segs[j + event_dir].max_connect_);
	      
	      if (!event)
		continue;

	      current.vertical_count_ = vertical_count;
	      current.horizontal_[event_dir] = segs[j].stem_x_;
	      if (segs[j].dir_ == event_dir)
		{
		  if (on_line_bound
		      && me->get_bound (event_dir)->break_status_dir ())
		    {
		      current.horizontal_[event_dir]
			= (robust_relative_extent (me->get_bound (event_dir),
						   commonx, X_AXIS)[RIGHT]
			   + event_dir * break_overshoot[event_dir]);
		    }
		  else
		    {
		      Real notehead_width = 
			Stem::duration_log (segs[j].stem_) == 1
			? 1.98
			: 1.32; // URG.


		      if (inside_stem)
			{
			  Grob *neighbor_stem = stems[segs[j].stem_index_ + event_dir];
			  Real neighbor_stem_x
			    = neighbor_stem->relative_coordinate (commonx, X_AXIS);

			  notehead_width = min (notehead_width,
						fabs (neighbor_stem_x - segs[j].stem_x_)/2.5);
			}
		      current.horizontal_[event_dir] += event_dir * notehead_width;
		    }
		}
	      else
		{
		  current.horizontal_[event_dir] += event_dir * segs[j].width_/2;
		  if (segs[j].gapped_)
		    {
		      current.horizontal_[event_dir] -= event_dir * gap_length;

		      if (Stem::is_invisible (segs[j].stem_))
			{
			  /*
			    Need to do this in case of whole notes. We don't want the
			    heads to collide with the beams.
			   */
			  extract_grob_set (segs[j].stem_, "note-heads", heads);

			  for (vsize k = 0; k < heads.size (); k ++)
			    current.horizontal_[event_dir]
			      = event_dir * min  (event_dir * current.horizontal_[event_dir],
						  - gap_length/2
						  + event_dir
						    * heads[k]->extent (commonx,
									X_AXIS)[-event_dir]);
			}
		    }
		}

	      if (event_dir == RIGHT)
		{
		  segments.push_back (current);
		  current = Beam_segment ();
		}
	    }
	  while (flip (&event_dir) != LEFT);
	}
      
    }

  return segments;
}

MAKE_SCHEME_CALLBACK (Beam, print, 1);
SCM
Beam::print (SCM grob)
{
  Spanner *me = unsmob_spanner (grob);
  Grob *commonx = 0;
  vector<Beam_segment> segments = get_beam_segments (me, &commonx);

  Interval span;
  if (normal_stem_count (me))
    {
      span[LEFT] = first_normal_stem (me)->relative_coordinate (commonx, X_AXIS);
      span[RIGHT] = last_normal_stem (me)->relative_coordinate (commonx, X_AXIS);
    }
  else
    {
      extract_grob_set (me, "stems", stems);      
      span[LEFT] = stems[0]->relative_coordinate (commonx, X_AXIS);
      span[RIGHT] = stems.back ()->relative_coordinate (commonx, X_AXIS);
    }

  Real blot = me->layout ()->get_dimension (ly_symbol2scm ("blot-diameter"));

  SCM posns = me->get_property ("quantized-positions");
  Interval pos;
  if (!is_number_pair (posns))
    {
      programming_error ("no beam positions?");
      pos = Interval (0, 0);
    }
  else
    pos = ly_scm2realdrul (posns);

  scale_drul (&pos, Staff_symbol_referencer::staff_space (me));

  Real dy = pos[RIGHT] - pos[LEFT];
  Real slope = (dy && span.length ()) ? dy / span.length ()  : 0;

  Real thick = get_thickness (me);
  Real beam_dy = get_beam_translation (me);

  Direction feather_dir = to_dir (me->get_property ("grow-direction"));
  
  Stencil the_beam;
  for (vsize i = 0; i < segments.size (); i ++)
    {
      Real local_slope = slope;
      if (feather_dir)
	{
	  local_slope += feather_dir * segments[i].vertical_count_ * beam_dy / span.length ();
	}
      
      Stencil b = Lookup::beam (local_slope, segments[i].horizontal_.length (), thick, blot);

      b.translate_axis (segments[i].horizontal_[LEFT], X_AXIS);
      
      b.translate_axis (local_slope
			* (segments[i].horizontal_[LEFT] - span.linear_combination (feather_dir))
			+ pos.linear_combination (feather_dir)
			+ beam_dy * segments[i].vertical_count_, Y_AXIS);
      the_beam.add_stencil (b);      
    }
	 
#if (DEBUG_BEAM_SCORING)
  SCM annotation = me->get_property ("annotation");
  if (!scm_is_string (annotation))
    {
      SCM debug = me->layout ()->lookup_variable (ly_symbol2scm ("debug-beam-scoring"));
      if (to_boolean (debug))
	annotation = me->get_property ("quant-score");
    }
  
  if (scm_is_string (annotation))
    {
      extract_grob_set (me, "stems", stems);      

      /*
	This code prints the demerits for each beam. Perhaps this
	should be switchable for those who want to twiddle with the
	parameters.
      */
      string str;
      SCM properties = Font_interface::text_font_alist_chain (me);

      Direction stem_dir = stems.size () ? to_dir (stems[0]->get_property ("direction")) : UP;

      Stencil score = *unsmob_stencil (Text_interface::interpret_markup
				    (me->layout ()->self_scm (), properties, annotation));

      if (!score.is_empty ())
	{
	  score.translate_axis (me->relative_coordinate(commonx, X_AXIS), X_AXIS);
	  the_beam.add_at_edge (Y_AXIS, stem_dir, score, 1.0);
	}
    }
#endif

  the_beam.translate_axis (-me->relative_coordinate (commonx, X_AXIS), X_AXIS);
  return the_beam.smobbed_copy ();
}
 
Direction
Beam::get_default_dir (Grob *me)
{
  extract_grob_set (me, "stems", stems);

  Drul_array<Real> extremes (0.0, 0.0);
  for (iterof (s, stems); s != stems.end (); s++)
    {
      Interval positions = Stem::head_positions (*s);
      Direction d = DOWN;
      do
	{
	  if (sign (positions[d]) == d)
	    extremes[d] = d * max (d * positions[d], d * extremes[d]);
	}
      while (flip (&d) != DOWN);
    }

  Drul_array<int> total (0, 0);
  Drul_array<int> count (0, 0);

  bool force_dir = false;
  for (vsize i = 0; i < stems.size (); i++)
    {
      Grob *s = stems[i];
      Direction stem_dir = CENTER;
      SCM stem_dir_scm = s->get_property_data ("direction");
      if (is_direction (stem_dir_scm))
	{
	  stem_dir = to_dir (stem_dir_scm);
	  force_dir = true;
	}
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


  if (!force_dir)
    {
      if (abs (extremes[UP]) > -extremes[DOWN])
	return DOWN;
      else if (extremes[UP] < -extremes[DOWN])
	return UP;
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

      SCM forcedir = s->get_property_data ("direction");
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

  extract_grob_set (me, "normal-stems", stems);

  Grob *common = common_refpoint_of_array (stems, me, Y_AXIS);
  Real staff_space = Staff_symbol_referencer::staff_space (me);

  vector<Interval> head_extents_array;
  for (vsize i = 0; i < stems.size (); i++)
    {
      Grob *stem = stems[i];

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
	  head_extents += stem->pure_relative_y_coordinate (common, 0, INT_MAX);

	  if (to_dir (stem->get_property_data ("direction")))
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

  

MAKE_SCHEME_CALLBACK (Beam, calc_stem_shorten, 1)
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
    / normal_stem_count (me);

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


Interval
Beam::no_visible_stem_positions (Grob *me, Interval default_value)
{
  extract_grob_set (me, "stems", stems);
  if (stems.empty ())
    return default_value;
  
  Interval head_positions;
  Slice multiplicity;
  for (vsize i = 0; i < stems.size(); i++)
    {
      head_positions.unite (Stem::head_positions (stems[i]));
      multiplicity.unite (Stem::beam_multiplicity (stems[i]));
    }

  Direction dir = get_grob_direction (me);
  Real y = head_positions[dir]
    * 0.5 * Staff_symbol_referencer::staff_space (me)
    + dir * get_beam_translation (me) * (multiplicity.length () + 1);

  y /= Staff_symbol_referencer::staff_space (me);
  return Interval (y,y);
}


/*
  Compute a first approximation to the beam slope.
*/
MAKE_SCHEME_CALLBACK (Beam, calc_least_squares_positions, 2);
SCM
Beam::calc_least_squares_positions (SCM smob, SCM /* posns */)
{
  Grob *me = unsmob_grob (smob);

  int count = normal_stem_count (me);
  Interval pos (0,0);
  if (count < 1)
    return ly_interval2scm (no_visible_stem_positions (me, pos));

  vector<Real> x_posns;
  extract_grob_set (me, "normal-stems", stems);
  Grob *commonx = common_refpoint_of_array (stems, me, X_AXIS);
  Grob *commony = common_refpoint_of_array (stems, me, Y_AXIS);

  Real my_y = me->relative_coordinate (commony, Y_AXIS);

  Grob *fvs = first_normal_stem (me);
  Grob *lvs = last_normal_stem (me);

  Interval ideal (Stem::get_stem_info (fvs).ideal_y_
		  + fvs->relative_coordinate (commony, Y_AXIS) - my_y,
		  Stem::get_stem_info (lvs).ideal_y_
		  + lvs->relative_coordinate (commony, Y_AXIS) - my_y);

  Real x0 = first_normal_stem (me)->relative_coordinate (commonx, X_AXIS);
  for (vsize i = 0; i < stems.size (); i++)
    {
      Grob *s = stems[i];

      Real x = s->relative_coordinate (commonx, X_AXIS) - x0;
      x_posns.push_back (x);
    }
  Real dx = last_normal_stem (me)->relative_coordinate (commonx, X_AXIS) - x0;

  Real y = 0;
  Real slope = 0;
  Real dy = 0;
  Real ldy = 0.0;
  if (!ideal.delta ())
    {
      Interval chord (Stem::chord_start_y (stems[0]),
		      Stem::chord_start_y (stems.back ()));

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
      vector<Offset> ideals;
      for (vsize i = 0; i < stems.size (); i++)
	{
	  Grob *s = stems[i];
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
  vector<Real> x_posns;
  extract_grob_set (me, "stems", stems);
  Grob *commonx = common_refpoint_of_array (stems, me, X_AXIS);
  Grob *commony = common_refpoint_of_array (stems, me, Y_AXIS);

  Grob *fvs = first_normal_stem (me);

  if (!fvs)
    return posns;

  Real x0 = fvs->relative_coordinate (commonx, X_AXIS);
  for (vsize i = 0; i < stems.size (); i++)
    {
      Grob *s = stems[i];

      Real x = s->relative_coordinate (commonx, X_AXIS) - x0;
      x_posns.push_back (x);
    }

  Grob *lvs = last_normal_stem (me);
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

  if (normal_stem_count (me) <= 1)
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

      Grob *fvs = first_normal_stem (me);
      Grob *lvs = last_normal_stem (me);

      Grob *commonx = fvs->common_refpoint (lvs, X_AXIS);

      Real dx = last_normal_stem (me)->relative_coordinate (commonx, X_AXIS)
	- first_normal_stem (me)->relative_coordinate (commonx, X_AXIS);

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
Beam::calc_stem_y (Grob *me, Grob *stem, Grob **common,
		   Real xl, Real xr, Direction feather_dir, 
		   Drul_array<Real> pos, bool french)
{
  Real beam_translation = get_beam_translation (me);
  Direction stem_dir = get_grob_direction (stem);

  Real dx = xr - xl;
  Real relx = dx ? (stem->relative_coordinate (common[X_AXIS], X_AXIS) - xl)/dx : 0;
  Real xdir = 2*relx-1;

  Real stem_y = linear_combination(pos, xdir);

  SCM beaming = stem->get_property ("beaming");

  Slice beam_slice (french
		    ? where_are_the_whole_beams (beaming)
		    : Stem::beam_multiplicity (stem));
  if (beam_slice.is_empty ())
    beam_slice = Slice (0,0);
  Interval beam_multiplicity(beam_slice[LEFT],
			     beam_slice[RIGHT]);

  /*
    feather dir = 1 , relx 0->1 : factor 0 -> 1
    feather dir = 0 , relx 0->1 : factor 1 -> 1    
    feather dir = -1, relx 0->1 : factor 1 -> 0    
   */
  Real feather_factor = 1;
  if (feather_dir > 0)
    feather_factor = relx;
  else if (feather_dir < 0)
    feather_factor = 1 - relx;
  
  stem_y += feather_factor * beam_translation
    * beam_multiplicity[Direction(((french) ? DOWN : UP)*stem_dir)];
  Real id = me->relative_coordinate (common[Y_AXIS], Y_AXIS)
    - stem->relative_coordinate (common[Y_AXIS], Y_AXIS);

  return stem_y + id;
}

/*
  Hmm.  At this time, beam position and slope are determined.  Maybe,
  stem directions and length should set to relative to the chord's
  position of the beam.  */
MAKE_SCHEME_CALLBACK (Beam, set_stem_lengths, 1); 
SCM
Beam::set_stem_lengths (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  /* trigger callbacks. */
  (void) me->get_property ("direction");
  (void) me->get_property ("beaming");

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
  if (robust_scm2int (me->get_property ("gap-count"), 0))
    {
      gap = true;
      thick = get_thickness (me);
    }

  Grob *fvs = first_normal_stem (me);
  Grob *lvs = last_normal_stem (me);

  Real xl = fvs ? fvs->relative_coordinate (common[X_AXIS], X_AXIS) : 0.0;
  Real xr = lvs ? lvs->relative_coordinate (common[X_AXIS], X_AXIS) : 0.0;
  Direction feather_dir = to_dir (me->get_property ("grow-direction"));

  for (vsize i = 0; i < stems.size (); i++)
    {
      Grob *s = stems[i];

      bool french = to_boolean (s->get_property ("french-beaming"));
      Real stem_y = calc_stem_y (me, s, common,
				 xl, xr, feather_dir,
				 pos, french && s != lvs && s!= fvs);

      /*
	Make the stems go up to the end of the beam. This doesn't matter
	for normal beams, but for tremolo beams it looks silly otherwise.
      */
      if (gap
	  && !Stem::is_invisible (s))
	stem_y += thick * 0.5 * get_grob_direction (s);

      /*
	Do set_stemend for invisible stems too, so tuplet brackets
	have a reference point for sloping
       */
      Stem::set_stemend (s, 2 * stem_y / staff_space);
    }

  return posns;
}

void
Beam::set_beaming (Grob *me, Beaming_pattern const *beaming)
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
	      int count = beaming->beamlet_count (i, d);
	      if (i > 0
		  && i + 1 < stems.size ()
		  && Stem::is_invisible (stem))
		count = min (count, beaming->beamlet_count (i,-d));

	      if ( ((i == 0 && d == LEFT)
		    || (i == stems.size ()-1 && d == RIGHT))
		   && stems.size () > 1
		   && to_boolean (me->get_property ("clip-edges")))
		count = 0;

	      Stem::set_beaming (stem, count, d);
	    }
	}
      while (flip (&d) != LEFT);
    }
}

int
Beam::forced_stem_count (Grob *me)
{
  extract_grob_set (me, "normal-stems", stems);

  int f = 0;
  for (vsize i = 0; i < stems.size (); i++)
    {
      Grob *s = stems[i];

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
Beam::normal_stem_count (Grob *me)
{
  extract_grob_set (me, "normal-stems", stems);
  return stems.size ();
}

Grob *
Beam::first_normal_stem (Grob *me)
{
  extract_grob_set (me, "normal-stems", stems);
  return stems.size () ? stems[0] : 0;
}

Grob *
Beam::last_normal_stem (Grob *me)
{
  extract_grob_set (me, "normal-stems", stems);
  return stems.size () ? stems.back () : 0;
}

/*
  [TODO]

  handle rest under beam (do_post: beams are calculated now)
  what about combination of collisions and rest under beam.

  Should lookup

  rest -> stem -> beam -> interpolate_y_position ()
*/
MAKE_SCHEME_CALLBACK_WITH_OPTARGS (Beam, rest_collision_callback, 2, 1, "");
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
      || !Beam::normal_stem_count (beam))
    return scm_from_double (0.0);

  Drul_array<Real> pos (robust_scm2drul (beam->get_property ("positions"),
					 Drul_array<Real> (0,0)));

  Real staff_space = Staff_symbol_referencer::staff_space (rest);

  scale_drul (&pos, staff_space);

  Real dy = pos[RIGHT] - pos[LEFT];

  Drul_array<Grob*> visible_stems (first_normal_stem (beam),
				   last_normal_stem (beam));
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

  /*
    TODO: this is dubious, because this call needs the info we're
    computing right now.
   */
  Interval rest_extent = rest->extent (common_y, Y_AXIS);
  rest_extent.translate (offset);
  
  Real rest_dim = rest_extent[d];
  Real minimum_distance
    = staff_space * (robust_scm2double (stem->get_property ("stemlet-length"), 0.0)
		     + robust_scm2double (rest->get_property ("minimum-distance"), 0.0));

  Real shift = d * min (d * (beam_y - d * minimum_distance - rest_dim), 0.0);

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

  return scm_from_double (offset + staff_space * shift);
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

bool
Beam::is_cross_staff (Grob *me)
{
  extract_grob_set (me, "stems", stems);
  Grob *staff_symbol = Staff_symbol_referencer::get_staff_symbol (me);
  for (vsize i = 0; i < stems.size (); i++)
    if (Staff_symbol_referencer::get_staff_symbol (stems[i]) != staff_symbol)
      return true;
  return false;
}

MAKE_SCHEME_CALLBACK (Beam, calc_cross_staff, 1)
SCM
Beam::calc_cross_staff (SCM smob)
{
  return scm_from_bool (is_cross_staff (unsmob_grob (smob)));
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
	       "A beam.\n"
	       "\n"
	       "The @code{thickness} property is the weight of beams,"
	       " measured in staffspace.  The @code{direction} property is"
	       " not user-serviceable.  Use the @code{direction} property"
	       " of @code{Stem} instead.",
	       
	       /* properties */
	       "annotation "
	       "auto-knee-gap "
	       "beamed-stem-shorten "
	       "beaming "
	       "break-overshoot "
	       "clip-edges "
	       "concaveness "
	       "damping "
	       "details "
	       "direction "
	       "gap "
	       "gap-count "
	       "grow-direction "
	       "inspect-quants "
	       "knee "
	       "length-fraction "
	       "least-squares-dy "
	       "neutral-direction "
	       "normal-stems "
	       "positions "
	       "quant-score "
	       "quantized-positions "
	       "shorten "
	       "stems "
	       "thickness "
	       );
