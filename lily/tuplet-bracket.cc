/*
  tuplet-bracket.cc -- implement Tuplet_bracket

  source file of the GNU LilyPond music typesetter

  (c) 1997--2009 Jan Nieuwenhuizen <janneke@gnu.org>
  Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

/*
  TODO:

  - tuplet bracket should probably be subject to the same rules as
  beam sloping/quanting.

  - There is no support for kneed brackets, or nested brackets.

  - number placement for parallel beams should be much more advanced:
  for sloped beams some extra horizontal offset must be introduced.

  - number placement is usually done over the center note, not the
  graphical center.
*/

/*
  TODO: quantise, we don't want to collide with staff lines.
  (or should we be above staff?)

  todo: handle breaking elegantly.
*/


#include "tuplet-bracket.hh"
#include "line-interface.hh"
#include "beam.hh"
#include "warn.hh"
#include "output-def.hh"
#include "font-interface.hh"
#include "text-interface.hh"
#include "stem.hh"
#include "note-column.hh"
#include "pointer-group-interface.hh"
#include "directional-element-interface.hh"
#include "spanner.hh"
#include "staff-symbol-referencer.hh"
#include "lookup.hh"
#include "paper-column.hh"
#include "moment.hh"

static Item *
get_x_bound_item (Grob *me_grob, Direction hdir, Direction my_dir)
{
  Spanner *me = dynamic_cast<Spanner *> (me_grob);
  Item *g = me->get_bound (hdir);
  if (Note_column::has_interface (g)
      && Note_column::get_stem (g)
      && Note_column::dir (g) == my_dir)
    g = Note_column::get_stem (g);

  return g;
}


void
flatten_number_pair_property (Grob *me,
			      Direction xdir,  SCM sym)
{
  Drul_array<Real> zero (0, 0);
  Drul_array<Real> pair
    = robust_scm2drul (me->internal_get_property (sym), zero);
  pair[xdir] = 0.0;
  
  me->set_property (sym, ly_interval2scm (pair));
}


/*
  Return beam that encompasses the span of the tuplet bracket.
*/
Grob *
Tuplet_bracket::parallel_beam (Grob *me_grob, vector<Grob*> const &cols,
			       bool *equally_long)
{
  Spanner *me = dynamic_cast<Spanner *> (me_grob);

  if (me->get_bound (LEFT)->break_status_dir ()
      || me->get_bound (RIGHT)->break_status_dir ())
    return 0;

  Drul_array<Grob*> stems (Note_column::get_stem (cols[0]),
			   Note_column::get_stem (cols.back ()));

  if (!stems[RIGHT]
      || !stems[LEFT]
      || (dynamic_cast<Item*> (stems[RIGHT])->get_column ()
	  != me->get_bound (RIGHT)->get_column ()))
    return 0;

  Drul_array<Grob*> beams;
  Direction d = LEFT;
  do {
    beams[d] = stems[d] ? Stem::get_beam (stems[d]) : 0;
  } while (flip (&d) != LEFT);
  
  *equally_long = false;
  if (! (beams[LEFT] && (beams[LEFT] == beams[RIGHT]) && !me->is_broken ()))
    return 0;

  extract_grob_set (beams[LEFT], "stems", beam_stems);
  if (beam_stems.size () == 0)
    {
      programming_error ("beam under tuplet bracket has no stems");
      *equally_long = 0;
      return 0;
    }

  *equally_long =
    (beam_stems[0] == stems[LEFT]
     && beam_stems.back () == stems[RIGHT]);
  return beams[LEFT];
}


MAKE_SCHEME_CALLBACK (Tuplet_bracket, calc_connect_to_neighbors,1);
SCM
Tuplet_bracket::calc_connect_to_neighbors (SCM smob)
{
  Spanner *me = unsmob_spanner (smob);

  Direction dir = get_grob_direction (me); 
  Drul_array<Item *> bounds (get_x_bound_item (me, LEFT, dir),
			     get_x_bound_item (me, RIGHT, dir));
  
  Drul_array<bool> connect_to_other (false, false);
  Direction d = LEFT;
  do
    {
      Direction break_dir = bounds[d]->break_status_dir ();
      Spanner *orig_spanner = dynamic_cast<Spanner*> (me->original ());
      vsize neighbor_idx = me->get_break_index () - break_dir;
      if (break_dir
	  && d == RIGHT
	  && neighbor_idx < orig_spanner->broken_intos_.size ())
	{
	  Grob *neighbor = orig_spanner->broken_intos_[neighbor_idx];

	  /* trigger possible suicide*/
	  (void) neighbor->get_property ("positions");
	}

      connect_to_other[d]
	= (break_dir
	   && neighbor_idx < orig_spanner->broken_intos_.size ()
	   && orig_spanner->broken_intos_[neighbor_idx]->is_live ());
    }
  while (flip (&d) != LEFT);


  if (connect_to_other[LEFT] || connect_to_other[RIGHT])
    return scm_cons (scm_from_bool (connect_to_other[LEFT]),
		     scm_from_bool (connect_to_other[RIGHT]));
		     
  return SCM_EOL;
}

Grob* 
Tuplet_bracket::get_common_x (Spanner *me)
{
  extract_grob_set (me, "note-columns", columns);

  Grob * commonx = common_refpoint_of_array (columns, me, X_AXIS);
  commonx = commonx->common_refpoint (me->get_bound (LEFT), X_AXIS);
  commonx = commonx->common_refpoint (me->get_bound (RIGHT), X_AXIS);

  return commonx;
}
  
MAKE_SCHEME_CALLBACK (Tuplet_bracket, calc_control_points,1)
SCM
Tuplet_bracket::calc_control_points (SCM smob)
{
  Spanner *me = unsmob_spanner (smob);

  extract_grob_set (me, "note-columns", columns);

  SCM scm_positions = me->get_property ("positions");
  if (!me->is_live ())
    return SCM_EOL;
  
  if (!scm_is_pair (scm_positions))
    programming_error ("Positions should be number pair");
    
  Drul_array<Real> positions
    = robust_scm2drul (scm_positions, Drul_array<Real> (0,0));

  Grob *commonx = get_common_x (me);
  Direction dir = get_grob_direction (me);

  Drul_array<Item *> bounds;
  bounds[LEFT] = get_x_bound_item (me, LEFT, dir);
  bounds[RIGHT] = get_x_bound_item (me, RIGHT, dir);

  Drul_array<bool> connect_to_other =
    robust_scm2booldrul (me->get_property ("connect-to-neighbor"),
			 Drul_array<bool> (false, false));
  
    
  Interval x_span;
  Direction d = LEFT;
  do
    {
      x_span[d] = robust_relative_extent (bounds[d], commonx, X_AXIS)[d];

      if (connect_to_other[d])
	{
	  Interval overshoot (robust_scm2drul (me->get_property ("break-overshoot"),
					       Interval (-0.5, 0.0)));

	  if (d == RIGHT)
	    x_span[d] += d * overshoot[d];
	  else
	    x_span[d] = robust_relative_extent (bounds[d], commonx, X_AXIS)[RIGHT]
	      - overshoot[LEFT];
	}
      
      else if (d == RIGHT
	       && (columns.empty ()
		   || (bounds[d]->get_column ()
		       != dynamic_cast<Item *> (columns.back ())->get_column ())))
	{
	  /*
	    We're connecting to a column, for the last bit of a broken
	    fullLength bracket.
	  */
	  Real padding =
	    robust_scm2double(me->get_property("full-length-padding"), 1.0);

	  if (bounds[d]->break_status_dir ())
	    padding = 0.0;

	  Real coord = bounds[d]->relative_coordinate(commonx, X_AXIS);
	  if (to_boolean (me->get_property ("full-length-to-extent")))
	    coord = robust_relative_extent(bounds[d], commonx, X_AXIS)[LEFT];

	  coord = max (coord, x_span[LEFT]);

	  x_span[d] = coord - padding;
	}
    }
  while (flip (&d) != LEFT);

  
  
  x_span -= me->get_bound (LEFT)->relative_coordinate (commonx, X_AXIS);
  return scm_list_2 (ly_offset2scm (Offset (x_span[LEFT], positions[LEFT])),
		     ly_offset2scm (Offset (x_span[RIGHT], positions[RIGHT])));
}

/*
  TODO:

  in the case that there is no bracket, but there is a (single) beam,
  follow beam precisely for determining tuplet number location.
*/
MAKE_SCHEME_CALLBACK (Tuplet_bracket, print, 1);
SCM
Tuplet_bracket::print (SCM smob)
{
  Spanner *me = unsmob_spanner (smob);
  Stencil mol;

  extract_grob_set (me, "note-columns", columns);
  bool equally_long = false;
  Grob *par_beam = parallel_beam (me, columns, &equally_long);
  
  bool bracket_visibility = !(par_beam && equally_long);
  /*
    Fixme: the type of this prop is sucky.
  */
  SCM bracket = me->get_property ("bracket-visibility");
  if (scm_is_bool (bracket))
    bracket_visibility = ly_scm2bool (bracket);
  else if (bracket == ly_symbol2scm ("if-no-beam"))
    bracket_visibility = !par_beam;
  
  /* Don't print a tuplet bracket and number if no control-points were calculated */
  SCM cpoints =  me->get_property ("control-points");
  if (scm_ilength (cpoints) < 2)
    {
      me->suicide ();
      return SCM_EOL;
    }
  /*  if the tuplet does not span any time, i.e. a single-note tuplet, hide
      the bracket, but still let the number be displayed */
  if (robust_scm2moment (me->get_bound (LEFT)->get_column ()->get_property ("when"), Moment (0))
      == robust_scm2moment (me->get_bound (RIGHT)->get_column ()->get_property ("when"), Moment (0)))
  {
      bracket_visibility = false;
  }
  
  Drul_array<Offset> points;
  points[LEFT] = ly_scm2offset (scm_car (cpoints));
  points[RIGHT] = ly_scm2offset (scm_cadr (cpoints));
  
  Interval x_span (points[LEFT][X_AXIS], points[RIGHT][X_AXIS]);
  Drul_array<Real> positions (points[LEFT][Y_AXIS], points[RIGHT][Y_AXIS]);

  Output_def *pap = me->layout ();

  Grob *number_grob = unsmob_grob (me->get_object ("tuplet-number"));
  
  /*
    No bracket when it would be smaller than the number.
  */
  Real gap = 0.;
  if (bracket_visibility && number_grob)
    {
      Interval ext = number_grob->extent (number_grob, X_AXIS);
      if (!ext.is_empty ())
	{
	  gap = ext.length () + 1.0;
      
	  if (0.75 * x_span.length () < gap)
	    bracket_visibility = false;
	}
    }

  if (bracket_visibility)
    {
      Drul_array<Real> zero (0, 0);
      Real ss = Staff_symbol_referencer::staff_space (me);
      Drul_array<Real> height
	= robust_scm2drul (me->get_property ("edge-height"), zero);
      Drul_array<Real> flare
	= robust_scm2drul (me->get_property ("bracket-flare"), zero);
      Drul_array<Real> shorten
	= robust_scm2drul (me->get_property ("shorten-pair"), zero);
      Drul_array<Stencil> edge_stencils;

      Direction dir = get_grob_direction (me);
      
      scale_drul (&height, -ss * dir);
      scale_drul (&flare, ss);
      scale_drul (&shorten, ss);

      Drul_array<bool> connect_to_other =
	robust_scm2booldrul (me->get_property ("connect-to-neighbor"),
			     Drul_array<bool> (false, false));

      Direction d = LEFT;
      do
	{
	  if (connect_to_other[d])
	    {
	      height[d] = 0.0;
	      flare[d] = 0.0;
	      shorten[d] = 0.0;

	      SCM edge_text = me->get_property ("edge-text");

	      if (scm_is_pair (edge_text))
		{
		  SCM properties = Font_interface::text_font_alist_chain (me);
		  SCM text = index_get_cell (edge_text, d);
		  if (Text_interface::is_markup (text))
		    {
		      SCM t = Text_interface::interpret_markup (pap->self_scm (),
								properties, text);

		      Stencil *edge_text = unsmob_stencil (t);
		      edge_text->translate_axis (x_span[d] - x_span[LEFT], X_AXIS);
		      edge_stencils[d] = *edge_text;
		    }
		}
	    }
	}
      while (flip (&d) != LEFT);

      Stencil brack = make_bracket (me, Y_AXIS,
				    points[RIGHT] - points[LEFT],
				    height,
				    /*
				      0.1 = more space at right due to italics
				      TODO: use italic correction of font.
				    */
				    Interval (-0.5, 0.5) * gap + 0.1,
				    flare, shorten);

      do
	{
	  if (!edge_stencils[d].is_empty ())
	    brack.add_stencil (edge_stencils[d]);
	}
      while (flip (&d) != LEFT);

      mol.add_stencil (brack);
    }

  mol.translate (points[LEFT]);
  return mol.smobbed_copy ();
}

/*
  should move to lookup?

  TODO: this will fail for very short (shorter than the flare)
  brackets.
*/
Stencil
Tuplet_bracket::make_bracket (Grob *me, // for line properties.
			      Axis protusion_axis,
			      Offset dz,
			      Drul_array<Real> height,
			      Interval gap,
			      Drul_array<Real> flare,
			      Drul_array<Real> shorten)
{
  Drul_array<Offset> corners (Offset (0, 0), dz);

  Real length = dz.length ();
  Drul_array<Offset> gap_corners;

  Axis bracket_axis = other_axis (protusion_axis);

  Drul_array<Offset> straight_corners = corners;

  Direction d = LEFT;
  do
    straight_corners[d] += -d * shorten[d] / length * dz;
  while (flip (&d) != LEFT);

  if (!gap.is_empty ())
    {
      do
	gap_corners[d] = (dz * 0.5) + gap[d] / length * dz;
      while (flip (&d) != LEFT);
    }

  Drul_array<Offset> flare_corners = straight_corners;
  do
    {
      flare_corners[d][bracket_axis] = straight_corners[d][bracket_axis];
      flare_corners[d][protusion_axis] += height[d];
      straight_corners[d][bracket_axis] += -d * flare[d];
    }
  while (flip (&d) != LEFT);

  Stencil m;
  do
    {
      if (!gap.is_empty ())
	m.add_stencil (Line_interface::line (me, straight_corners[d],
					     gap_corners[d]));

      m.add_stencil (Line_interface::line (me, straight_corners[d],
					   flare_corners[d]));
    }

  while (flip (&d) != LEFT);

  if (gap.is_empty ())
    m.add_stencil (Line_interface::line (me, straight_corners[LEFT],
					 straight_corners[RIGHT]));
  
  return m;
}

void
Tuplet_bracket::get_bounds (Grob *me, Grob **left, Grob **right)
{
  extract_grob_set (me, "note-columns", columns);
  vsize l = 0;
  while (l < columns.size () && Note_column::has_rests (columns[l]))
    l++;

  vsize r = columns.size ();
  while (r > l && Note_column::has_rests (columns[r-1]))
    r--;

  *left = *right = 0;

  if (l < r)
    {
      *left = columns[l];
      *right = columns[r-1];
    }
}

/*
  use first -> last note for slope, and then correct for disturbing
  notes in between.  */
void
Tuplet_bracket::calc_position_and_height (Grob *me_grob, Real *offset, Real *dy)
{
  Spanner *me = dynamic_cast<Spanner *> (me_grob);

  extract_grob_set (me, "note-columns", columns);
  extract_grob_set (me, "tuplets", tuplets);

  Grob *commony = common_refpoint_of_array (columns, me, Y_AXIS);
  commony = common_refpoint_of_array (tuplets, commony, Y_AXIS);
  if (Grob *st = Staff_symbol_referencer::get_staff_symbol (me))
    commony = st->common_refpoint (commony, Y_AXIS);
  Real my_offset = me->relative_coordinate (commony, Y_AXIS);

  Grob *commonx = get_common_x (me);
  commonx = common_refpoint_of_array (tuplets, commonx, Y_AXIS);

  Interval staff;
  Grob *st = Staff_symbol_referencer::get_staff_symbol (me);

  /* staff-padding doesn't work correctly on cross-staff tuplets
     because it only considers one staff symbol. Until this works,
     disable it. */
  if (st && !to_boolean (me->get_property ("cross-staff")))
    {
      Real pad = robust_scm2double (me->get_property ("staff-padding"), -1.0);
      if  (pad >= 0.0)
	{
	  staff = st->extent (commony, Y_AXIS) - my_offset;
	  staff.widen (pad);
	}
    }
  
  Direction dir = get_grob_direction (me);

  bool equally_long = false;
  Grob *par_beam = parallel_beam (me, columns, &equally_long);
  
  Item *lgr = get_x_bound_item (me, LEFT, dir);
  Item *rgr = get_x_bound_item (me, RIGHT, dir);
  Real x0 = robust_relative_extent (lgr, commonx, X_AXIS)[LEFT];
  Real x1 = robust_relative_extent (rgr, commonx, X_AXIS)[RIGHT];
  bool follow_beam = par_beam
    && get_grob_direction (par_beam) == dir
    && ! to_boolean (par_beam->get_property ("knee"));

  vector<Offset> points;
  if (columns.size ()
      && follow_beam
      && Note_column::get_stem (columns[0])
      && Note_column::get_stem (columns.back ()))
    {
      /*
	trigger set_stem_ends
       */
      (void) par_beam->get_property ("quantized-positions");

      Drul_array<Grob *> stems (Note_column::get_stem (columns[0]),
				Note_column::get_stem (columns.back ()));

      Real ss = 0.5 * Staff_symbol_referencer::staff_space (me);
      Real lp = ss * robust_scm2double (stems[LEFT]->get_property ("stem-end-position"), 0.0)
        + stems[LEFT]->get_parent (Y_AXIS)->relative_coordinate (commony, Y_AXIS);
      Real rp = ss * robust_scm2double (stems[RIGHT]->get_property ("stem-end-position"), 0.0)
        + stems[RIGHT]->get_parent (Y_AXIS)->relative_coordinate (commony, Y_AXIS);

      *dy = rp - lp;
      points.push_back (Offset (stems[LEFT]->relative_coordinate (commonx, X_AXIS) - x0, lp));
      points.push_back (Offset (stems[RIGHT]->relative_coordinate (commonx, X_AXIS) - x0, rp));
    }
  else
    {
      /*
	Use outer non-rest columns to determine slope
      */
      Grob *left_col = 0;
      Grob *right_col = 0;
      get_bounds (me, &left_col, &right_col);
      if (left_col && right_col)
	{
	  Interval rv = Note_column::cross_staff_extent (right_col, commony);
	  Interval lv = Note_column::cross_staff_extent (left_col, commony);
	  rv.unite (staff);
	  lv.unite (staff);

	  Real graphical_dy = rv[dir] - lv[dir];

	  Slice ls = Note_column::head_positions_interval (left_col);
	  Slice rs = Note_column::head_positions_interval (right_col);

	  Interval musical_dy;
	  musical_dy[UP] = rs[UP] - ls[UP];
	  musical_dy[DOWN] = rs[DOWN] - ls[DOWN];
	  if (sign (musical_dy[UP]) != sign (musical_dy[DOWN]))
	    *dy = 0.0;
	  else if (sign (graphical_dy) != sign (musical_dy[DOWN]))
	    *dy = 0.0;
	  else
	    *dy = graphical_dy;
	}
      else
	*dy = 0;

      for (vsize i = 0; i < columns.size (); i++)
	{
	  Interval note_ext = Note_column::cross_staff_extent (columns[i], commony);
	  Real x = columns[i]->relative_coordinate (commonx, X_AXIS) - x0;

	  points.push_back (Offset (x, note_ext[dir]));
	}
    }

  if (!follow_beam)
    {
      points.push_back (Offset (x0 - x0, staff[dir]));
      points.push_back (Offset (x1 - x0, staff[dir]));
    }
  
  /*
    This is a slight hack. We compute two encompass points from the
    bbox of the smaller tuplets.

    We assume that the smaller bracket is 1.0 space high.
  */
  Real ss = Staff_symbol_referencer::staff_space (me);
  for (vsize i = 0; i < tuplets.size (); i++)
    {
      Interval tuplet_x (tuplets[i]->extent (commonx, X_AXIS));
      Interval tuplet_y (tuplets[i]->extent (commony, Y_AXIS));

      if (!tuplets[i]->is_live ())
	continue;
      
      Direction d = LEFT;
      Drul_array<Real> positions = robust_scm2interval (tuplets[i]->get_property ("positions"),
							Interval (0,0));

      
      Real other_dy = positions[RIGHT] - positions[LEFT];

      do
	{
	  Real y
	    = tuplet_y.linear_combination (d * sign (other_dy));

	  /*
	    We don't take padding into account for nested tuplets.
	    the edges can come very close to the stems, likewise for
	    nested tuplets?
	  */

	  points.push_back (Offset (tuplet_x[d] - x0, y));
	}
      while (flip (&d) != LEFT);
    }

  *offset = -dir * infinity_f;
  Real factor = (columns.size () > 1) ? 1 / (x1 - x0) : 1.0;
  for (vsize i = 0; i < points.size (); i++)
    {
      Real x = points[i][X_AXIS];
      Real tuplety = (*dy) * x * factor + my_offset;

      if (points[i][Y_AXIS] * dir > (*offset + tuplety) * dir)
	*offset = points[i][Y_AXIS] - tuplety;
    }

  *offset += scm_to_double (me->get_property ("padding")) * dir;

  /*
    horizontal brackets should not collide with staff lines.

    Kind of pointless since we put them outside the staff anyway, but
    let's leave code for the future when possibly allow them to move
    into the staff once again.
  
    This doesn't seem to support cross-staff tuplets atm.
  */
  if (*dy == 0
      && fabs (*offset) < ss * Staff_symbol_referencer::staff_radius (me))
    {
      // quantize, then do collision check.
      *offset *= 2 / ss;

      *offset = rint (*offset);
      if (Staff_symbol_referencer::on_line (me, (int) rint (*offset)))
	*offset += dir;

      *offset *= 0.5 * ss;
    }
}


MAKE_SCHEME_CALLBACK (Tuplet_bracket, calc_direction, 1);
SCM
Tuplet_bracket::calc_direction (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Direction dir = Tuplet_bracket::get_default_dir (me);
  return scm_from_int (dir);
}

MAKE_SCHEME_CALLBACK (Tuplet_bracket, calc_positions, 1);
SCM
Tuplet_bracket::calc_positions (SCM smob)
{
  Spanner *me = unsmob_spanner (smob);

  Real dy = 0.0;
  Real offset = 0.0;
  calc_position_and_height (me, &offset, &dy);
  
  SCM x = scm_cons (scm_from_double (offset),
		    scm_from_double (offset + dy));
  
  return x;
}

/*
  similar to beam ?
*/
Direction
Tuplet_bracket::get_default_dir (Grob *me)
{
  Drul_array<int> dirs (0, 0);
  extract_grob_set (me, "note-columns", columns);
  for (vsize i = 0; i < columns.size (); i++)
    {
      Grob *nc = columns[i];
      Direction d = Note_column::dir (nc);
      if (d)
	dirs[d]++;
    }

  return dirs[UP] >= dirs[DOWN] ? UP : DOWN;
}

void
Tuplet_bracket::add_column (Grob *me, Item *n)
{
  Pointer_group_interface::add_grob (me, ly_symbol2scm ("note-columns"), n);
  add_bound_item (dynamic_cast<Spanner *> (me), n);
}

void
Tuplet_bracket::add_tuplet_bracket (Grob *me, Grob *bracket)
{
  Pointer_group_interface::add_grob (me, ly_symbol2scm ("tuplets"), bracket);
}

MAKE_SCHEME_CALLBACK (Tuplet_bracket, calc_cross_staff, 1);
SCM
Tuplet_bracket::calc_cross_staff (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  extract_grob_set (me, "note-columns", cols);
  extract_grob_set (me, "tuplets", tuplets);

  Grob *commony = common_refpoint_of_array (cols, me, Y_AXIS);
  commony = common_refpoint_of_array (tuplets, commony, Y_AXIS);
  if (Grob *st = Staff_symbol_referencer::get_staff_symbol (me))
    commony = st->common_refpoint (commony, Y_AXIS);
  if (me->check_cross_staff (commony))
    return SCM_BOOL_T;

  bool equally_long = false;
  Grob *par_beam = parallel_beam (me, cols, &equally_long);

  if (par_beam && to_boolean (par_beam->get_property ("cross-staff")))
    return SCM_BOOL_T;

  for (vsize i = 0; i < cols.size (); i++)
    {
      Grob *stem = unsmob_grob (cols[i]->get_object ("stem"));
      if (stem && to_boolean (stem->get_property ("cross-staff")))
	return SCM_BOOL_T;
    }

  return SCM_BOOL_F;
}

ADD_INTERFACE (Tuplet_bracket,
	       "A bracket with a number in the middle, used for tuplets."
	       "  When the bracket spans a line break, the value of"
	       " @code{break-overshoot} determines how far it extends"
	       " beyond the staff.  At a line break, the markups in the"
	       " @code{edge-text} are printed at the edges.",

	       /* properties */
	       "bracket-flare "
	       "bracket-visibility "
	       "break-overshoot "
	       "connect-to-neighbor "
	       "control-points "
	       "direction "
	       "edge-height "
	       "edge-text "
	       "full-length-padding "
	       "full-length-to-extent "
	       "gap "
	       "positions "
	       "note-columns "
	       "padding "
	       "tuplet-number "
	       "shorten-pair "
	       "staff-padding "
	       "thickness "
	       "tuplets "
	       );


