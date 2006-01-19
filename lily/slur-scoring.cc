/*
  slur-scoring.cc -- Score based slur formatting

  source file of the GNU LilyPond music typesetter

  (c) 1996--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/


#include "slur-scoring.hh"
#include "libc-extension.hh"
#include "slur-configuration.hh"
#include "beam.hh"
#include "directional-element-interface.hh"
#include "pointer-group-interface.hh"
#include "slur.hh"
#include "note-column.hh"
#include "output-def.hh"
#include "pitch.hh"
#include "spanner.hh"
#include "staff-symbol-referencer.hh"
#include "staff-symbol.hh"
#include "stem.hh"
#include "warn.hh"
#include "paper-column.hh"
#include "accidental-interface.hh"

/*
  TODO:

  - curve around flag for y coordinate

  - short-cut: try a smaller region first.

  - handle non-visible stems better.

  - try to prune number of scoring criteria

  - take encompass-objects more into account when determining
  slur shape

  - calculate encompass scoring directly after determining slur shape.

  - optimize.
*/
struct Slur_score_state;

Slur_score_state::Slur_score_state ()
{
  musical_dy_ = 0.0;
  valid_ = false;
  edge_has_beams_ = false;
  has_same_beam_ = false;
  is_broken_ = false;
  dir_ = CENTER;
  slur_ = 0;
  common_[X_AXIS] = 0;
  common_[Y_AXIS] = 0;
}

Slur_score_state::~Slur_score_state ()
{
  junk_pointers (configurations_);
}

Real
get_detail (SCM alist, SCM sym)
{
  SCM entry = scm_assq (sym, alist);
  return robust_scm2double (scm_is_pair (entry)
			    ? scm_cdr (entry)
			    : SCM_EOL,
			    0.0);
}

void
Slur_score_parameters::fill (Grob *me)
{
  SCM details = me->get_property ("details");

  region_size_
    = (int) get_detail (details, ly_symbol2scm ("region-size"));
  head_encompass_penalty_
    = get_detail (details, ly_symbol2scm ("head-encompass-penalty"));
  stem_encompass_penalty_
    = get_detail (details, ly_symbol2scm ("stem-encompass-penalty"));
  closeness_factor_
    = get_detail (details, ly_symbol2scm ("closeness-factor"));
  edge_attraction_factor_
    = get_detail (details, ly_symbol2scm ("edge-attraction-factor"));
  same_slope_penalty_
    = get_detail (details, ly_symbol2scm ("same-slope-penalty"));
  steeper_slope_factor_
    = get_detail (details, ly_symbol2scm ("steeper-slope-factor"));
  non_horizontal_penalty_
    = get_detail (details, ly_symbol2scm ("non-horizontal-penalty"));
  max_slope_
    = get_detail (details, ly_symbol2scm ("max-slope"));
  max_slope_factor_
    = get_detail (details, ly_symbol2scm ("max-slope-factor"));
  free_head_distance_
    = get_detail (details, ly_symbol2scm ("free-head-distance"));
  absolute_closeness_measure_
    = get_detail (details, ly_symbol2scm ("absolute-closeness-measure"));
  extra_object_collision_penalty_
    = get_detail (details, ly_symbol2scm ("extra-object-collision-penalty"));
  accidental_collision_
    = get_detail (details, ly_symbol2scm ("accidental-collision"));
  extra_encompass_free_distance_
    = get_detail (details, ly_symbol2scm ("extra-encompass-free-distance"));
  head_slur_distance_factor_
    = get_detail (details, ly_symbol2scm ("head-slur-distance-factor"));
  head_slur_distance_max_ratio_
    = get_detail (details, ly_symbol2scm ("head-slur-distance-max-ratio"));
  free_slur_distance_
    = get_detail (details, ly_symbol2scm ("free-slur-distance"));
  edge_slope_exponent_
    = get_detail (details, ly_symbol2scm ("edge-slope-exponent"));
}

Real
broken_trend_y (Slur_score_state const &state, Direction hdir)
{
  /* A broken slur should maintain the same vertical trend
     the unbroken slur would have had.  */
  Real by = 0.0;
  if (Spanner *mother = dynamic_cast<Spanner *> (state.slur_->original ()))
    {
      int k = broken_spanner_index (state.slur_);
      int j = k + hdir;
      if (j < 0 || j >= mother->broken_intos_.size ())
	return by;

      Grob *neighbor = mother->broken_intos_[j];
      Spanner *common_mother
	= dynamic_cast<Spanner *> (state.common_[Y_AXIS]->original ());
      int common_k
	= broken_spanner_index (dynamic_cast<Spanner *> (state.common_[Y_AXIS]));
      int common_j = common_k + hdir;

      if (common_j < 0 || common_j >= common_mother->broken_intos_.size ())
	return by;

      Grob *common_next_system = common_mother->broken_intos_[common_j];

      SCM last_point = scm_car (scm_last_pair (neighbor->get_property ("control-points")));

      return scm_to_double (scm_cdr (last_point))
	+ neighbor->relative_coordinate (common_next_system, Y_AXIS);
    }
  return by;
}

/*
  copy slur dir forwards across line break.
*/
void
Slur_score_state::set_next_direction ()
{
  if (extremes_[RIGHT].note_column_)
    return;

  if (Spanner *mother = dynamic_cast<Spanner *> (slur_->original ()))
    {
      int k = broken_spanner_index (slur_);
      int j = k + 1;
      if (j < 0 || j >= mother->broken_intos_.size ())
	return;

      Grob *neighbor = mother->broken_intos_[j];
      set_grob_direction (neighbor, dir_);
    }
}

Encompass_info
Slur_score_state::get_encompass_info (Grob *col) const
{
  Grob *stem = unsmob_grob (col->get_object ("stem"));
  Encompass_info ei;

  if (!stem)
    {
      programming_error ("no stem for note column");
      ei.x_ = col->relative_coordinate (common_[X_AXIS], X_AXIS);
      ei.head_ = ei.stem_ = col->extent (common_[Y_AXIS],
					 Y_AXIS)[dir_];
      return ei;
    }
  Direction stem_dir = get_grob_direction (stem);

  if (Grob *head = Note_column::first_head (col))
    ei.x_ = head->extent (common_[X_AXIS], X_AXIS).center ();
  else
    ei.x_ = col->extent (common_[X_AXIS], X_AXIS).center ();

  Grob *h = Stem::extremal_heads (stem)[Direction (dir_)];
  if (!h)
    {
      ei.head_ = ei.stem_ = col->extent (common_[Y_AXIS], Y_AXIS)[dir_];
      return ei;
    }

  ei.head_ = h->extent (common_[Y_AXIS], Y_AXIS)[dir_];

  if ((stem_dir == dir_)
      && !stem->extent (stem, Y_AXIS).is_empty ())
    {
      ei.stem_ = stem->extent (common_[Y_AXIS], Y_AXIS)[dir_];
      if (Grob *b = Stem::get_beam (stem))
	ei.stem_ += stem_dir * 0.5 * Beam::get_thickness (b);

      Interval x = stem->extent (common_[X_AXIS], X_AXIS);
      ei.x_ = x.is_empty ()
	? stem->relative_coordinate (common_[X_AXIS], X_AXIS)
	: x.center ();
    }
  else
    ei.stem_ = ei.head_;

  return ei;
}

Drul_array<Bound_info>
Slur_score_state::get_bound_info () const
{
  Drul_array<Bound_info> extremes;

  Direction d = LEFT;
  Direction dir = dir_;

  do
    {
      extremes[d].bound_ = slur_->get_bound (d);
      if (Note_column::has_interface (extremes[d].bound_))
	{
	  extremes[d].note_column_ = extremes[d].bound_;
	  extremes[d].stem_ = Note_column::get_stem (extremes[d].note_column_);
	  if (extremes[d].stem_)
	    {
	      extremes[d].stem_dir_ = get_grob_direction (extremes[d].stem_);

	      for (int a = X_AXIS; a < NO_AXES; a++)
		{
		  Axis ax = Axis (a);
		  Interval s = extremes[d].stem_->extent (common_[ax], ax);
		  if (s.is_empty ())
		    {
		      /*
			do not issue warning. This happens for rests and
			whole notes.
		      */
		      s = Interval (0, 0)
			+ extremes[d].stem_->relative_coordinate (common_[ax], ax);
		    }
		  extremes[d].stem_extent_[ax] = s;
		}

	      extremes[d].slur_head_
		= Stem::extremal_heads (extremes[d].stem_)[dir];
	      if (!extremes[d].slur_head_
		  && Note_column::has_rests (extremes[d].bound_))
		extremes[d].slur_head_ = Note_column::get_rest (extremes[d].bound_);
	      extremes[d].staff_ = Staff_symbol_referencer
		::get_staff_symbol (extremes[d].stem_);
	      extremes[d].staff_space_ = Staff_symbol_referencer
		::staff_space (extremes[d].stem_);
	    }

	  if (extremes[d].slur_head_)
	    extremes[d].slur_head_x_extent_
	      = extremes[d].slur_head_->extent (common_[X_AXIS], X_AXIS);

	}
    }
  while (flip (&d) != LEFT);

  return extremes;
}

void
Slur_score_state::fill (Grob *me)
{
  slur_ = dynamic_cast<Spanner *> (me);
  columns_
    = internal_extract_grob_array (me, ly_symbol2scm ("note-columns"));

  if (columns_.is_empty ())
    {
      me->suicide ();
      return;
    }

  staff_space_ = Staff_symbol_referencer::staff_space (me);
  Real lt = me->layout ()->get_dimension (ly_symbol2scm ("linethickness"));
  thickness_ = robust_scm2double (me->get_property ("thickness"), 1.0) * lt;

  dir_ = get_grob_direction (me);
  parameters_.fill (me);

  extract_grob_set (me, "note-columns", columns);
  extract_grob_set (me, "encompass-objects", extra_objects);

  Spanner *sp = dynamic_cast<Spanner *> (me);

  for (int i = X_AXIS; i < NO_AXES; i++)
    {
      Axis a = (Axis)i;
      common_[a] = common_refpoint_of_array (columns, me, a);
      common_[a] = common_refpoint_of_array (extra_objects, common_[a], a);

      Direction d = LEFT;
      do
	{
	  /*
	    If bound is not in note-columns, we don't want to know about
	    its Y-position
	  */
	  if (a != Y_AXIS)
	    common_[a] = common_[a]->common_refpoint (sp->get_bound (d), a);
	}
      while (flip (&d) != LEFT);
    }

  extremes_ = get_bound_info ();
  is_broken_ = (!extremes_[LEFT].note_column_
		|| !extremes_[RIGHT].note_column_);

  has_same_beam_
    = (extremes_[LEFT].stem_ && extremes_[RIGHT].stem_
       && Stem::get_beam (extremes_[LEFT].stem_) == Stem::get_beam (extremes_[RIGHT].stem_));

  base_attachments_ = get_base_attachments ();

  Drul_array<Real> end_ys
    = get_y_attachment_range ();

  configurations_ = enumerate_attachments (end_ys);
  for (int i = 0; i < columns_.size (); i++)
    encompass_infos_.push (get_encompass_info (columns_[i]));

  extra_encompass_infos_ = get_extra_encompass_infos ();
  valid_ = true;

  musical_dy_ = 0.0;
  Direction d = LEFT;
  do
    {
      if (!is_broken_
	  && extremes_[d].slur_head_)
	musical_dy_ += d
	  * extremes_[d].slur_head_->relative_coordinate (common_[Y_AXIS], Y_AXIS);
    }
  while (flip (&d) != LEFT);

  edge_has_beams_
    = (extremes_[LEFT].stem_ && Stem::get_beam (extremes_[LEFT].stem_))
    || (extremes_[RIGHT].stem_ && Stem::get_beam (extremes_[RIGHT].stem_));

  set_next_direction ();

  if (is_broken_)
    musical_dy_ = 0.0;
}


MAKE_SCHEME_CALLBACK(Slur, calc_control_points, 1)
SCM
Slur::calc_control_points (SCM smob)
{
  Spanner *me = unsmob_spanner (smob);

  Slur_score_state state;
  state.fill (me);

  if (!state.valid_)
    return SCM_EOL;

  state.generate_curves ();

  SCM end_ys = me->get_property ("positions");
  Bezier best;

  if (is_number_pair (end_ys))
    best = state.configurations_[state.get_closest_index (end_ys)]->curve_;
  else
    best = state.get_best_curve ();

  SCM controls = SCM_EOL;
  for (int i = 4; i--;)
    {
      Offset o = best.control_[i]
	- Offset (me->relative_coordinate (state.common_[X_AXIS], X_AXIS),
		  me->relative_coordinate (state.common_[Y_AXIS], Y_AXIS));
      controls = scm_cons (ly_offset2scm (o), controls);
    }

  return controls;
}

Bezier
Slur_score_state::get_best_curve ()
{
  int opt_idx = -1;
  Real opt = 1e6;

#if DEBUG_SLUR_SCORING
  SCM inspect_quants = slur_->get_property ("inspect-quants");
  if (to_boolean (slur_->layout ()
		  ->lookup_variable (ly_symbol2scm ("debug-slur-scoring")))
      && scm_is_pair (inspect_quants))
    {
      opt_idx = get_closest_index (inspect_quants);
      configurations_[opt_idx]->score (*this);
      opt = configurations_[opt_idx]->score_;
    }
  else
#endif
    {
      for (int i = 0; i < configurations_.size (); i++)
	configurations_[i]->score (*this);
      for (int i = 0; i < configurations_.size (); i++)
	{
	  if (configurations_[i]->score_ < opt)
	    {
	      opt = configurations_[i]->score_;
	      opt_idx = i;
	    }
	}
    }

#if DEBUG_SLUR_SCORING
  configurations_[opt_idx]->score_card_ += to_string ("=%.2f", opt);
  configurations_[opt_idx]->score_card_ += to_string ("i%d", opt_idx);

  // debug quanting
  slur_->set_property ("quant-score",
		       scm_makfrom0str (configurations_[opt_idx]->score_card_.to_str0 ()));

#endif

  return configurations_[opt_idx]->curve_;
}

int
Slur_score_state::get_closest_index (SCM inspect_quants) const
{
  Drul_array<Real> ins = ly_scm2interval (inspect_quants);

  int opt_idx = -1;
  Real mindist = 1e6;
  for (int i = 0; i < configurations_.size (); i++)
    {
      Real d = fabs (configurations_[i]->attachment_[LEFT][Y_AXIS] - ins[LEFT])
	+ fabs (configurations_[i]->attachment_[RIGHT][Y_AXIS] - ins[RIGHT]);
      if (d < mindist)
	{
	  opt_idx = i;
	  mindist = d;
	}
    }
  if (mindist > 1e5)
    programming_error ("can't not find quant");
  return opt_idx;
}

/*
  TODO: should analyse encompasses to determine sensible region, and
  should limit slopes available.
*/

Drul_array<Real>
Slur_score_state::get_y_attachment_range () const
{
  Drul_array<Real> end_ys;
  Direction d = LEFT;
  do
    {
      if (extremes_[d].note_column_)
	{
	  end_ys[d] = dir_
	    * max (max (dir_ * (base_attachments_[d][Y_AXIS]
					  + parameters_.region_size_ * dir_),
			dir_ * (dir_ + extremes_[d].note_column_->extent (common_[Y_AXIS], Y_AXIS)[dir_])),
		   dir_ * base_attachments_[-d][Y_AXIS]);
	}
      else
	end_ys[d] = base_attachments_[d][Y_AXIS] + parameters_.region_size_ * dir_;
    }
  while (flip (&d) != LEFT);

  return end_ys;
}

bool
spanner_less (Spanner *s1, Spanner *s2)
{
  Slice b1, b2;
  Direction d = LEFT;
  do
    {
      b1[d] = s1->get_bound (d)->get_column ()->get_rank ();
      b2[d] = s2->get_bound (d)->get_column ()->get_rank ();
    }
  while (flip (&d) != LEFT);

  return b2[LEFT] <= b1[LEFT] && b2[RIGHT] >= b1[RIGHT]
    && (b2[LEFT] != b1[LEFT] || b2[RIGHT] != b1[RIGHT]);
}

Drul_array<Offset>
Slur_score_state::get_base_attachments () const
{
  Drul_array<Offset> base_attachment;
  Direction d = LEFT;
  do
    {
      Grob *stem = extremes_[d].stem_;
      Grob *head = extremes_[d].slur_head_;

      Real x = 0.0;
      Real y = 0.0;
      if (extremes_[d].note_column_)
	{

	  /*
	    fixme: X coord should also be set in this case.
	  */
	  if (stem
	      && !Stem::is_invisible (stem)
	      && extremes_[d].stem_dir_ == dir_
	      && Stem::get_beaming (stem, -d)
	      && (!spanner_less (slur_, Stem::get_beam (stem))
		  || has_same_beam_))
	    y = extremes_[d].stem_extent_[Y_AXIS][dir_];
	  else if (head)
	    y = head->extent (common_[Y_AXIS], Y_AXIS)[dir_];
	  y += dir_ * 0.5 * staff_space_;

	  y = move_away_from_staffline (y, head);

	  Grob *fh = Note_column::first_head (extremes_[d].note_column_);
	  x
	    = (fh ? fh->extent (common_[X_AXIS], X_AXIS)
	       : extremes_[d].bound_->extent (common_[X_AXIS], X_AXIS))
	    .linear_combination (CENTER);
	}
      base_attachment[d] = Offset (x, y);
    }
  while (flip (&d) != LEFT);

  do
    {
      if (!extremes_[d].note_column_)
	{
	  Real x, y;
	  if (d == RIGHT)
	    x = extremes_[d].bound_->extent (common_[X_AXIS], X_AXIS)[d];
	  else
	    x = slur_->get_broken_left_end_align ();
	  Grob *col = (d == LEFT) ? columns_[0] : columns_.top ();

	  if (extremes_[-d].bound_ != col)
	    {
	      y = robust_relative_extent (col, common_[Y_AXIS], Y_AXIS)[dir_];
	      y += dir_ * 0.5 * staff_space_;

	      if (get_grob_direction (col) == dir_
		  && Note_column::get_stem (col)
		  && !Stem::is_invisible (Note_column::get_stem (col)))
		y -= dir_ * 1.5 * staff_space_;
	    }
	  else
	    y = base_attachment[-d][Y_AXIS];

	  y = move_away_from_staffline (y, col);

	  base_attachment[d] = Offset (x, y);
	}
    }
  while (flip (&d) != LEFT);

  do
    {
      for (int a = X_AXIS; a < NO_AXES; a++)
	{
	  Real &b = base_attachment[d][Axis (a)];

	  if (isinf (b) || isnan (b))
	    {
	      programming_error ("slur attachment is inf/nan");
	      b = 0.0;
	    }
	}
    }
  while (flip (&d) != LEFT);

  return base_attachment;
}

Real
Slur_score_state::move_away_from_staffline (Real y,
					    Grob *on_staff) const
{
  if (!on_staff)
    return y;
  
  Grob *staff_symbol = Staff_symbol_referencer::get_staff_symbol (on_staff);
  if (!staff_symbol)
    return y;

  Real pos
    = (y - staff_symbol->relative_coordinate (common_[Y_AXIS],
					      Y_AXIS))
    * 2.0 / staff_space_;

  if (fabs (pos - my_round (pos)) < 0.2
      && Staff_symbol_referencer::on_line (on_staff, (int) rint (pos))
      && Staff_symbol_referencer::line_count (on_staff) - 1 >= rint (pos))
    y += 1.5 * staff_space_ * dir_ / 10;

  return y;
}

Array<Offset>
Slur_score_state::generate_avoid_offsets () const
{
  Array<Offset> avoid;
  Link_array<Grob> encompasses = columns_;

  for (int i = 0; i < encompasses.size (); i++)
    {
      if (extremes_[LEFT].note_column_ == encompasses[i]
	  || extremes_[RIGHT].note_column_ == encompasses[i])
	continue;

      Encompass_info inf (get_encompass_info (encompasses[i]));
      Real y = dir_ * (max (dir_ * inf.head_, dir_ * inf.stem_));

      avoid.push (Offset (inf.x_, y + dir_ * parameters_.free_head_distance_));
    }

  extract_grob_set (slur_, "encompass-objects", extra_encompasses);
  for (int i = 0; i < extra_encompasses.size (); i++)
    {
      if (Slur::has_interface (extra_encompasses[i]))
	{
	  Grob *small_slur = extra_encompasses[i];
	  Bezier b = Slur::get_curve (small_slur);

	  Offset z = b.curve_point (0.5);
	  z += Offset (small_slur->relative_coordinate (common_[X_AXIS], X_AXIS),
		       small_slur->relative_coordinate (common_[Y_AXIS], Y_AXIS));

	  z[Y_AXIS] += dir_ * parameters_.free_slur_distance_;
	  avoid.push (z);
	}
      else if (extra_encompasses[i]->get_property ("avoid-slur") == ly_symbol2scm ("inside"))
	{
	  Grob *g = extra_encompasses [i];
	  Interval xe = g->extent (common_[X_AXIS], X_AXIS);
	  Interval ye = g->extent (common_[Y_AXIS], Y_AXIS);

	  if (!xe.is_empty ()
	      && !ye.is_empty ())
	    avoid.push (Offset (xe.center(), ye[dir_]));
	}
    }  
  return avoid;
}

void
Slur_score_state::generate_curves () const
{
  Real r_0 = robust_scm2double (slur_->get_property ("ratio"), 0.33);
  Real h_inf = staff_space_ * scm_to_double (slur_->get_property ("height-limit"));

  Array<Offset> avoid = generate_avoid_offsets ();
  for (int i = 0; i < configurations_.size (); i++)
    configurations_[i]->generate_curve (*this, r_0, h_inf, avoid);
}

Link_array<Slur_configuration>
Slur_score_state::enumerate_attachments (Drul_array<Real> end_ys) const
{
  Link_array<Slur_configuration> scores;

  Drul_array<Offset> os;
  os[LEFT] = base_attachments_[LEFT];
  Real minimum_length = staff_space_
    * robust_scm2double (slur_->get_property ("minimum-length"), 2.0);

  for (int i = 0; dir_ * os[LEFT][Y_AXIS] <= dir_ * end_ys[LEFT]; i++)
    {
      os[RIGHT] = base_attachments_[RIGHT];
      for (int j = 0; dir_ * os[RIGHT][Y_AXIS] <= dir_ * end_ys[RIGHT]; j++)
	{
	  Slur_configuration s;
	  Direction d = LEFT;
	  Drul_array<bool> attach_to_stem (false, false);
	  do
	    {
	      os[d][X_AXIS] = base_attachments_[d][X_AXIS];
	      if (extremes_[d].stem_
		  && !Stem::is_invisible (extremes_[d].stem_)
		  && extremes_[d].stem_dir_ == dir_)
		{
		  Interval stem_y = extremes_[d].stem_extent_[Y_AXIS];
		  stem_y.widen (0.25 * staff_space_);
		  if (stem_y.contains (os[d][Y_AXIS]))
		    {
		      os[d][X_AXIS] = extremes_[d].stem_extent_[X_AXIS][-d]
			- d * 0.3;
		      attach_to_stem[d] = true;
		    }
		  else if (dir_ * extremes_[d].stem_extent_[Y_AXIS][dir_]
			   < dir_ * os[d][Y_AXIS]
			   && !extremes_[d].stem_extent_[X_AXIS].is_empty ())

		    os[d][X_AXIS] = extremes_[d].stem_extent_[X_AXIS].center ();
		}
	    }
	  while (flip (&d) != LEFT);

	  Offset dz;
	  dz = os[RIGHT] - os[LEFT];
	  if (dz[X_AXIS] < minimum_length
	      || fabs (dz[Y_AXIS] / dz[X_AXIS]) > parameters_.max_slope_)
	    {
	      do
		{
		  if (extremes_[d].slur_head_
		      && !extremes_[d].slur_head_x_extent_.is_empty ())
		    {
		      os[d][X_AXIS] = extremes_[d].slur_head_x_extent_.center ();
		      attach_to_stem[d] = false;
		    }
		}
	      while (flip (&d) != LEFT);
	    }

	  dz = os[RIGHT] - os[LEFT];
	  do
	    {
	      if (extremes_[d].slur_head_
		  && !attach_to_stem[d])
		{
		  /* Horizontally move tilted slurs a little.  Move
		     more for bigger tilts.

		     TODO: parameter */
		  os[d][X_AXIS]
		    -= dir_ * extremes_[d].slur_head_x_extent_.length ()
		    * sin (dz.arg ()) / 3;
		}
	    }
	  while (flip (&d) != LEFT);

	  s.attachment_ = os;
	  s.index_ = scores.size ();

	  scores.push (new Slur_configuration (s));

	  os[RIGHT][Y_AXIS] += dir_ * staff_space_ / 2;
	}

      os[LEFT][Y_AXIS] += dir_ * staff_space_ / 2;
    }

  assert (scores.size () > 0);
  return scores;
}

Array<Extra_collision_info>
Slur_score_state::get_extra_encompass_infos () const
{
  extract_grob_set (slur_, "encompass-objects", encompasses);
  Array<Extra_collision_info> collision_infos;
  for (int i = encompasses.size (); i--;)
    {
      if (Slur::has_interface (encompasses[i]))
	{
	  Spanner *small_slur = dynamic_cast<Spanner *> (encompasses[i]);
	  Bezier b = Slur::get_curve (small_slur);

	  Offset relative (small_slur->relative_coordinate (common_[X_AXIS], X_AXIS),
			   small_slur->relative_coordinate (common_[Y_AXIS], Y_AXIS));

	  for (int k = 0; k < 3; k++)
	    {
	      Direction hdir = Direction (k / 2 - 1);

	      /*
		Only take bound into account if small slur starts
		together with big slur.
	      */
	      if (hdir && small_slur->get_bound (hdir) != slur_->get_bound (hdir))
		continue;

	      Offset z = b.curve_point (k / 2.0);
	      z += relative;

	      Interval yext;
	      yext.set_full ();
	      yext[dir_] = z[Y_AXIS] + dir_ * thickness_ * 1.0;

	      Interval xext (-1, 1);
	      xext = xext * (thickness_ * 2) + z[X_AXIS];
	      Extra_collision_info info (small_slur,
					 k - 1.0,
					 xext,
					 yext,
					 parameters_.extra_object_collision_penalty_);
	      collision_infos.push (info);
	    }
	}
      else
	{
	  Grob *g = encompasses [i];
	  Interval xe = g->extent (common_[X_AXIS], X_AXIS);
	  Interval ye = g->extent (common_[Y_AXIS], Y_AXIS);

	  Real xp = 0.0;
	  Real penalty = parameters_.extra_object_collision_penalty_;
	  if (Accidental_interface::has_interface (g))
	    {
	      penalty = parameters_.accidental_collision_;
	      /* Begin copy accidental.cc */
	      bool parens = false;
	      if (to_boolean (g->get_property ("cautionary")))
		{
		  SCM cstyle = g->get_property ("cautionary-style");
		  parens = ly_is_equal (cstyle, ly_symbol2scm ("parentheses"));
		}

	      SCM accs = g->get_property ("accidentals");
	      SCM scm_style = g->get_property ("style");
	      if (!scm_is_symbol (scm_style)
		  && !parens
		  && scm_ilength (accs) == 1)
		{
		  /* End copy accidental.cc */
		  switch (scm_to_int (scm_car (accs)))
		    {
		    case FLAT:
		    case DOUBLE_FLAT:
		      xp = LEFT;
		      break;
		    case SHARP:
		      xp = 0.5 * dir_;
		      break;
		    case NATURAL:
		      xp = -dir_;
		      break;
		    }
		}
	    }

	  ye.widen (thickness_ * 0.5);
	  xe.widen (thickness_ * 1.0);
	  Extra_collision_info info (g, xp, xe, ye, penalty);
	  collision_infos.push (info);
	}
    }

  return collision_infos;
}
 
Extra_collision_info::Extra_collision_info (Grob *g, Real idx, Interval x, Interval y, Real p)
{
  idx_ = idx;
  extents_[X_AXIS] = x;
  extents_[Y_AXIS] = y;
  penalty_ = p;
  grob_ = g;
  type_ = g->get_property ("avoid-slur");
}

Extra_collision_info::Extra_collision_info ()
{
  idx_ = 0.0;
  penalty_ = 0.;
  grob_ = 0;
  type_ = SCM_EOL; 
}
