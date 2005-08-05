/*
  tuplet-bracket.cc -- implement Tuplet_bracket

  source file of the GNU LilyPond music typesetter

  (c) 1997--2005 Jan Nieuwenhuizen <janneke@gnu.org>
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

#include <math.h>

#include "tuplet-bracket.hh"
#include "line-interface.hh"
#include "beam.hh"
#include "warn.hh"
#include "font-interface.hh"
#include "output-def.hh"
#include "text-interface.hh"
#include "stem.hh"
#include "note-column.hh"
#include "pointer-group-interface.hh"
#include "directional-element-interface.hh"
#include "spanner.hh"
#include "staff-symbol-referencer.hh"
#include "lookup.hh"

static Item *
get_x_bound_item (Grob *me_grob, Direction hdir, Direction my_dir)
{
  Spanner *me = dynamic_cast<Spanner*> (me_grob);
  Item *g = me->get_bound (hdir);
  if (Note_column::has_interface (g)
      && Note_column::get_stem (g)
      && Note_column::dir (g) == my_dir)
    {
      g = Note_column::get_stem (g);
    }

  return g;
}

Grob *
Tuplet_bracket::parallel_beam (Grob *me_grob, Link_array<Grob> const &cols, bool *equally_long)
{
  Spanner *me = dynamic_cast<Spanner *> (me_grob);
  
  if (me->get_bound (LEFT)->break_status_dir ()
      || me->get_bound (RIGHT)->break_status_dir ())
    return 0;
  
  Grob *s1 = Note_column::get_stem (cols[0]);
  Grob *s2 = Note_column::get_stem (cols.top ());


  if (s2 != me->get_bound (RIGHT))
    return 0;

  Grob *b1 = s1 ? Stem::get_beam (s1) : 0;
  Grob *b2 = s2 ? Stem::get_beam (s2) : 0;


  *equally_long = false;
  if (! (b1 && (b1 == b2) && !me->is_broken ()))
    return 0;

  extract_grob_set (b1, "stems", beam_stems);
  if (beam_stems.size () == 0)
    {
      programming_error ("beam under tuplet bracket has no stems");
      *equally_long = 0;
      return 0;
    }

  *equally_long = (beam_stems[0] == s1 && beam_stems.top () == s2);
  return b1;
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

  {
    SCM lp = me->get_property ("left-position");
    SCM rp = me->get_property ("right-position");

    if (!scm_is_number (rp) || !scm_is_number (lp))
      {
	/*
	  UGH. dependency tracking!
	 */
	extract_grob_set (me, "tuplets", tuplets);
	for (int i = 0; i < tuplets.size (); i++)
	  Tuplet_bracket::print (tuplets[i]->self_scm());

	after_line_breaking (smob);
      }
  }

  Real ly = robust_scm2double (me->get_property ("left-position"), 0);
  Real ry = robust_scm2double (me->get_property ("right-position"), 0);

  bool equally_long = false;
  Grob *par_beam = parallel_beam (me, columns, &equally_long);

  Spanner *sp = dynamic_cast<Spanner *> (me);

  bool bracket_visibility = !(par_beam && equally_long);
  bool number_visibility = true;

  /*
    Fixme: the type of this prop is sucky.
  */
  SCM bracket = me->get_property ("bracket-visibility");
  if (scm_is_bool (bracket))
    {
      bracket_visibility = ly_scm2bool (bracket);
    }
  else if (bracket == ly_symbol2scm ("if-no-beam"))
    bracket_visibility = !par_beam;

  SCM numb = me->get_property ("number-visibility");
  if (scm_is_bool (numb))
    {
      number_visibility = ly_scm2bool (numb);
    }
  else if (numb == ly_symbol2scm ("if-no-beam"))
    number_visibility = !par_beam;

  Grob *commonx = common_refpoint_of_array (columns, me, X_AXIS);
  commonx = commonx->common_refpoint (sp->get_bound (LEFT), X_AXIS);
  commonx = commonx->common_refpoint (sp->get_bound (RIGHT), X_AXIS);

  Direction dir = get_grob_direction (me);

  Drul_array<Item *> bounds;
  bounds[LEFT] = get_x_bound_item (me, LEFT, dir);
  bounds[RIGHT] = get_x_bound_item (me, RIGHT, dir);

  Drul_array<bool> connect_to_other; 
  Interval x_span;
  Direction d = LEFT;
  do
    {
      x_span[d] = robust_relative_extent (bounds[d], commonx, X_AXIS)[d];
      Direction break_dir = bounds[d]->break_status_dir ();
      Spanner *orig_spanner = dynamic_cast<Spanner*> (me->original_);
      connect_to_other[d]
	= (break_dir
	   && (me->get_break_index() - break_dir < orig_spanner->broken_intos_.size()));
      
      if (connect_to_other[d])
	{
	  Interval overshoot (robust_scm2drul (me->get_property ("break-overshoot"),
					       Interval (-0.5, 0.0)));

	  if (d == RIGHT)
	    x_span[d] += d * overshoot[d];
	  else
	    x_span[d] = robust_relative_extent(bounds[d], commonx, X_AXIS)[RIGHT]
	      - overshoot[LEFT];
	}
      else if (d == RIGHT &&
	       (columns.is_empty ()
		|| (bounds[d]->get_column () !=
		    dynamic_cast<Item*> (columns.top())->get_column ())))
	{
	  /*
	    TODO: make padding tunable? 
	   */
	  x_span[d] = robust_relative_extent (bounds[d], commonx, X_AXIS) [LEFT] - 1.0;
	}
    }
  while (flip (&d) != LEFT);

  Real w = x_span.length();
  SCM number = me->get_property ("text");

  Output_def *pap = me->get_layout ();
  Stencil num;
  if (scm_is_string (number) && number_visibility)
    {
      SCM properties = Font_interface::text_font_alist_chain (me);
      SCM snum = Text_interface::interpret_markup (pap->self_scm (), properties, number);
      num = *unsmob_stencil (snum);
      num.align_to (X_AXIS, CENTER);
      num.translate_axis (w / 2, X_AXIS);
      num.align_to (Y_AXIS, CENTER);

      num.translate_axis ((ry - ly) / 2, Y_AXIS);

      mol.add_stencil (num);
    }

  /*
    No bracket when it would be smaller than the number.

    TODO: should use GAP in calculation too.
  */
  if (bracket_visibility && number_visibility
      && mol.extent (X_AXIS).length () > w)
    {
      bracket_visibility = false;
    }

  if (bracket_visibility)
    {
      Real gap = 0.;

      if (!num.extent (X_AXIS).is_empty ())
	gap = num.extent (X_AXIS).length () + 1.0;

      Drul_array<Real> zero (0,0);
      Real ss = Staff_symbol_referencer::staff_space (me);
      Drul_array<Real> height
	= robust_scm2drul (me->get_property ("edge-height"), zero);
      Drul_array<Real> flare
	= robust_scm2drul (me->get_property ("bracket-flare"), zero);
      Drul_array<Real> shorten
	= robust_scm2drul (me->get_property ("shorten-pair"), zero);
      Drul_array<Stencil> edge_stencils;
      
      scale_drul (&height, -ss * dir);
      scale_drul (&flare, ss);
      scale_drul (&shorten, ss);
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
		      SCM t = Text_interface::interpret_markup (pap->self_scm (), properties,
								text);
		  
		      Stencil *edge_text = unsmob_stencil (t);
		      edge_text->translate_axis (x_span[d] - x_span[LEFT], X_AXIS);
		      edge_stencils[d] = *edge_text;
		    }
		}
	    }
	}
      while (flip (&d) != LEFT);

 
      Stencil brack = make_bracket (me, Y_AXIS,
				    Offset (w, ry - ly),
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

  mol.translate_axis (ly, Y_AXIS);
  mol.translate_axis (x_span[LEFT]
		      - sp->get_bound (LEFT)->relative_coordinate (commonx, X_AXIS), X_AXIS);
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
    {
      straight_corners[d] += -d * shorten[d] / length * dz;
    }
  while (flip (&d) != LEFT);

  if (gap.is_empty ())
    gap = Interval (0, 0);
  do
    {
      gap_corners[d] = (dz * 0.5) + gap[d] / length * dz;
    }
  while (flip (&d) != LEFT);

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
      m.add_stencil (Line_interface::line (me, straight_corners[d],
					   gap_corners[d]));

      m.add_stencil (Line_interface::line (me, straight_corners[d],
					   flare_corners[d]));
    }
  while (flip (&d) != LEFT);

  return m;
}

void
Tuplet_bracket::get_bounds (Grob *me, Grob **left, Grob **right)
{
  extract_grob_set (me, "note-columns", columns);
  int l = 0;
  while (l < columns.size () && Note_column::has_rests (columns[l]))
    l++;

  int r = columns.size ()- 1;
  while (r >= l && Note_column::has_rests (columns[r]))
    r--;

  *left = *right = 0;

  if (l <= r)
    {
      *left = columns[l];
      *right = columns[r];
    }
}


/*
  use first -> last note for slope, and then correct for disturbing
  notes in between.  */
void
Tuplet_bracket::calc_position_and_height (Grob *me_grob, Real *offset, Real *dy)
{
  Spanner *me = dynamic_cast<Spanner*> (me_grob);
  
  extract_grob_set (me, "note-columns", columns);
  extract_grob_set (me, "tuplets", tuplets);
  
  Grob *commony = common_refpoint_of_array (columns, me, Y_AXIS);
  commony = common_refpoint_of_array (tuplets, commony, Y_AXIS);
  if (Grob *st = Staff_symbol_referencer::get_staff_symbol (me))
    {
      commony = st->common_refpoint (commony, Y_AXIS); 
    }

  Grob *commonx = common_refpoint_of_array (columns, me, X_AXIS);
  commonx = common_refpoint_of_array (tuplets, commonx, Y_AXIS);
  commonx = commonx->common_refpoint (me->get_bound (LEFT), X_AXIS);
  commonx = commonx->common_refpoint (me->get_bound (RIGHT), X_AXIS);

  Interval staff;
  if (Grob *st = Staff_symbol_referencer::get_staff_symbol (me))
    staff = st->extent (commony, Y_AXIS);

  Direction dir = get_grob_direction (me);

  /*
    Use outer non-rest columns to determine slope
  */
  Grob *left_col = 0;
  Grob *right_col = 0;
  get_bounds (me, &left_col, &right_col);
  if (left_col && right_col)
    {
      Interval rv = right_col->extent (commony, Y_AXIS);
      Interval lv = left_col->extent (commony, Y_AXIS);
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

  *offset = -dir * infinity_f;

  Item *lgr = get_x_bound_item (me, LEFT, dir);
  Item *rgr = get_x_bound_item (me, RIGHT, dir);
  Real x0 = robust_relative_extent (lgr, commonx, X_AXIS)[LEFT];
  Real x1 = robust_relative_extent (rgr, commonx, X_AXIS)[RIGHT];

  /*
    offset
  */
  Real factor = columns.size () > 1 ? 1 / (x1 - x0) : 1.0;

  Array<Offset> points;
  points.push (Offset (x0, staff[dir]));
  points.push (Offset (x1, staff[dir]));
  
  for (int i = 0; i < columns.size (); i++)
    {
      Interval note_ext = columns[i]->extent (commony, Y_AXIS);
      Real notey = note_ext[dir] - me->relative_coordinate (commony, Y_AXIS);

      Real x = columns[i]->relative_coordinate (commonx, X_AXIS) - x0;
      points.push (Offset (x, notey));
    }
  
  /*
    This is a slight hack. We compute two encompass points from the
    bbox of the smaller tuplets.
    
    We assume that the smaller bracket is 1.0 space high.
  */
  Real ss = Staff_symbol_referencer::staff_space (me);
  for (int i = 0; i < tuplets.size (); i++)
    {
      Interval tuplet_x (tuplets[i]->extent (commonx, X_AXIS));
      Interval tuplet_y (tuplets[i]->extent (commony, Y_AXIS));

      Direction d = LEFT;
      Real lp = scm_to_double (tuplets[i]->get_property ("left-position"));
      Real rp = scm_to_double (tuplets[i]->get_property ("right-position"));
      Real other_dy = rp - lp;

      do
	{
	  Real y =
	    tuplet_y.linear_combination (d * sign (other_dy));

#if 0
	  /*
	    Let's not take padding into account for nested tuplets.
	    the edges can come very close to the stems, likewise for
	    nested tuplets?
	   */
	  Drul_array<Real> my_height
	    = robust_scm2drul (me->get_property ("edge-height"), Interval (0,0));
	  if (dynamic_cast<Spanner*> (tuplets[i])->get_bound (d)
	      ==  me->get_bound (d))
	    {
	      y += dir * my_height[d];
	    }
#endif
	  
	  points.push (Offset (tuplet_x[d] - x0, y));
	}
      while (flip (&d) != LEFT);
    }

  for (int i = 0; i < points.size (); i++)
    {
      Real x = points[i][X_AXIS];
      Real tuplety = *dy * x * factor;

      if (points[i][Y_AXIS] * dir > (*offset + tuplety) * dir)
	*offset = points[i][Y_AXIS] - tuplety;
    }
		  
  *offset += scm_to_double (me->get_property ("padding")) * dir;

  /*
    horizontal brackets should not collide with staff lines.

    Kind of pointless since we put them outside the staff anyway, but
    let's leave code for the future when possibly allow them to move
    into the staff once again.
  */
  if (*dy == 0 &&
      fabs (*offset) < ss * Staff_symbol_referencer::staff_radius (me))
    {
      // quantize, then do collision check.
      *offset *= 2 / ss;

      *offset = rint (*offset);
      if (Staff_symbol_referencer::on_staffline (me, (int) rint (*offset)))
	*offset += dir;

      *offset *= 0.5 * ss;
    }
}

/*
  We depend on the beams if there are any.
*/
MAKE_SCHEME_CALLBACK (Tuplet_bracket, before_line_breaking, 1);
SCM
Tuplet_bracket::before_line_breaking (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  extract_grob_set (me, "note-columns", columns);

  for (int i = columns.size (); i--;)
    {
      Grob *s = Note_column::get_stem (columns[i]);
      Grob *b = s ? Stem::get_beam (s) : 0;
      if (b)
	me->add_dependency (b);
    }
  return SCM_UNSPECIFIED;
}

MAKE_SCHEME_CALLBACK (Tuplet_bracket, after_line_breaking, 1);

SCM
Tuplet_bracket::after_line_breaking (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  extract_grob_set (me, "note-columns", columns);

  Direction dir = get_grob_direction (me);
  if (!dir)
    {
      dir = Tuplet_bracket::get_default_dir (me);
      set_grob_direction (me, dir);
    }

  bool equally_long = false;
  Grob *par_beam = parallel_beam (me, columns, &equally_long);

  /*
    We follow the beam only if there is one, and we are next to it.
  */
  Real dy = 0.0;
  Real offset = 0.0;
  if (!par_beam
      || get_grob_direction (par_beam) != dir)
    {
      calc_position_and_height (me, &offset, &dy);
    }
  else
    {
      SCM ps = par_beam->get_property ("positions");

      Real lp = scm_to_double (scm_car (ps));
      Real rp = scm_to_double (scm_cdr (ps));

      /*
	duh. magic.
      */
      offset = lp + dir * (0.5 + scm_to_double (me->get_property ("padding")));
      dy = rp- lp;
    }

  SCM lp = me->get_property ("left-position");
  SCM rp = me->get_property ("right-position");

  if (scm_is_number (lp) && !scm_is_number (rp))
    {
      rp = scm_from_double (scm_to_double (lp) + dy);
    }
  else if (scm_is_number (rp) && !scm_is_number (lp))
    {
      lp = scm_from_double (scm_to_double (rp) - dy);
    }
  else if (!scm_is_number (rp) && !scm_is_number (lp))
    {
      lp = scm_from_double (offset);
      rp = scm_from_double (offset + dy);
    }

  me->set_property ("left-position", lp);
  me->set_property ("right-position", rp);

  return SCM_UNSPECIFIED;
}

/*
  similar to beam ?
*/
Direction
Tuplet_bracket::get_default_dir (Grob *me)
{
  Drul_array<int> dirs (0, 0);
  extract_grob_set (me, "note-columns", columns);
  for (int i = 0 ; i < columns.size (); i++)
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
  me->add_dependency (n);

  add_bound_item (dynamic_cast<Spanner *> (me), n);
}

void
Tuplet_bracket::add_tuplet_bracket (Grob *me, Grob *bracket)
{
  Pointer_group_interface::add_grob (me, ly_symbol2scm ("tuplets"), bracket);
  me->add_dependency (bracket);
}



ADD_INTERFACE (Tuplet_bracket,
	       "tuplet-bracket-interface",
	       "A bracket with a number in the middle, used for tuplets. "
	       "When the bracket spans  a line break, the value of "
	       "@code{break-overshoot} determines how far it extends "
	       "beyond the staff. "
	       "At a line break, the markups in the @code{edge-text} are printed "
	       "at the edges. ",

	       "note-columns bracket-flare edge-height shorten-pair "
	       "tuplets edge-text break-overshoot "
	       "padding left-position right-position bracket-visibility "
	       "number-visibility thickness direction");

