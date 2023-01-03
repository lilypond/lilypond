/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "config.hh"

#include "slur-scoring.hh"

#include "axis-group-interface.hh"
#include "accidental-interface.hh"
#include "beam.hh"
#include "clef.hh"
#include "directional-element-interface.hh"
#include "libc-extension.hh"
#include "international.hh"
#include "misc.hh"
#include "note-column.hh"
#include "note-head.hh"
#include "output-def.hh"
#include "paper-column.hh"
#include "pitch.hh"
#include "pointer-group-interface.hh"
#include "slur-configuration.hh"
#include "slur.hh"
#include "spanner.hh"
#include "staff-symbol-referencer.hh"
#include "staff-symbol.hh"
#include "stem.hh"
#include "warn.hh"

#include <cmath>
#include <memory>
#include <queue>
#include <string>
#include <vector>

using std::string;
using std::unique_ptr;
using std::vector;

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
class Slur_score_state;

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
}

/*
  If a slur is broken across a line break, the direction
  of the post-break slur must be the same as the pre-break
  slur.
*/
Direction
Slur_score_state::slur_direction () const
{
  Grob *left_neighbor = slur_->broken_neighbor (LEFT);

  if (left_neighbor && left_neighbor->is_live ())
    return get_grob_direction (left_neighbor);

  Direction dir = get_grob_direction (slur_);

  if (Grob *right_neighbor = slur_->broken_neighbor (RIGHT))
    set_grob_direction (right_neighbor, dir);

  return dir;
}

Encompass_info
Slur_score_state::get_encompass_info (Grob *notecol) const
{
  Grob *stem = unsmob<Grob> (get_object (notecol, "stem"));
  Encompass_info ei;

  if (!stem)
    {
      ei.x_ = notecol->relative_coordinate (common_[X_AXIS], X_AXIS);
      ei.head_ = ei.stem_ = notecol->extent (common_[Y_AXIS], Y_AXIS)[dir_];
      return ei;
    }
  Direction stem_dir = get_grob_direction (stem);

  if (Grob *head = Note_column::first_head (notecol))
    {
      Interval head_ext = head->extent (common_[X_AXIS], X_AXIS);
      // FIXME: Is there a better option than setting to 0?
      if (head_ext.is_empty ())
        ei.x_ = 0;
      else
        ei.x_ = head_ext.center ();
    }
  else
    ei.x_ = notecol->extent (common_[X_AXIS], X_AXIS).center ();

  Grob *h = Stem::extremal_heads (stem)[Direction (dir_)];
  if (!h)
    {
      ei.head_ = ei.stem_ = notecol->extent (common_[Y_AXIS], Y_AXIS)[dir_];
      return ei;
    }

  ei.head_ = h->extent (common_[Y_AXIS], Y_AXIS)[dir_];

  if ((stem_dir == dir_) && !stem->extent (stem, Y_AXIS).is_empty ())
    {
      ei.stem_ = stem->extent (common_[Y_AXIS], Y_AXIS)[dir_];
      if (Grob *b = Stem::get_beam (stem))
        ei.stem_ += stem_dir * 0.5 * Beam::get_beam_thickness (b);

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

  const auto slur_dir = dir_;

  for (const auto bound_dir : {LEFT, RIGHT})
    {
      auto &info = extremes[bound_dir];
      auto *const bound = slur_->get_bound (bound_dir);
      info.bound_ = bound;
      if (has_interface<Note_column> (bound))
        {
          auto *const note_col = bound;
          info.note_column_ = note_col;
          auto *const stem = Note_column::get_stem (note_col);
          info.stem_ = stem;
          auto *const flag = Note_column::get_flag (note_col);
          info.flag_ = flag;

          if (stem)
            {
              info.stem_dir_ = get_grob_direction (stem);

              for (const auto ax : {X_AXIS, Y_AXIS})
                {
                  Interval s = stem->extent (common_[ax], ax);
                  if (flag)
                    s.unite (flag->extent (common_[ax], ax));
                  if (s.is_empty ())
                    {
                      /*
                        do not issue warning. This happens for rests and
                        whole notes.
                      */
                      s = Interval (0, 0)
                          + stem->relative_coordinate (common_[ax], ax);
                    }
                  info.stem_extent_[ax] = s;
                }

              info.slur_head_ = Stem::extremal_heads (stem)[slur_dir];
            }
          else
            {
              info.slur_head_
                = Note_column::extremal_heads (note_col)[slur_dir];
            }

          if (!info.slur_head_)
            info.slur_head_ = Note_column::get_rest (note_col);
        }
      else if (has_interface<Note_head> (bound))
        {
          info.slur_head_ = bound;
        }

      if (info.slur_head_)
        {
          info.slur_head_x_extent_
            = info.slur_head_->extent (common_[X_AXIS], X_AXIS);
          info.staff_
            = Staff_symbol_referencer::get_staff_symbol (info.slur_head_);
        }
    }

  return extremes;
}

void
Slur_score_state::fill (Spanner *me)
{
  slur_ = me;
  note_columns_
    = internal_extract_grob_array (me, ly_symbol2scm ("note-columns"));

  if (note_columns_.empty ())
    {
      me->suicide ();
      return;
    }

  Slur::replace_breakable_encompass_objects (me);
  staff_space_ = Staff_symbol_referencer::staff_space (me);
  line_thickness_
    = me->layout ()->get_dimension (ly_symbol2scm ("line-thickness"));
  thickness_
    = from_scm<double> (get_property (me, "thickness"), 1.0) * line_thickness_;

  dir_ = slur_direction ();
  parameters_.fill (me);

  extract_grob_set (me, "note-columns", columns);
  extract_grob_set (me, "encompass-objects", extra_objects);

  for (const auto a : {X_AXIS, Y_AXIS})
    {
      common_[a] = common_refpoint_of_array (columns, me, a);
      common_[a] = common_refpoint_of_array (extra_objects, common_[a], a);

      for (const auto d : {LEFT, RIGHT})
        {
          /*
            If bound is not in note-columns, we don't want to know about
            its Y-position
          */
          if (a != Y_AXIS)
            common_[a] = common_[a]->common_refpoint (me->get_bound (d), a);
        }
    }

  extremes_ = get_bound_info ();
  is_broken_
    = (!(extremes_[LEFT].note_column_ || extremes_[LEFT].slur_head_)
       || !(extremes_[RIGHT].note_column_ || extremes_[RIGHT].slur_head_));

  has_same_beam_ = (extremes_[LEFT].stem_ && extremes_[RIGHT].stem_
                    && Stem::get_beam (extremes_[LEFT].stem_)
                         == Stem::get_beam (extremes_[RIGHT].stem_));

  base_attachments_ = get_base_attachments ();

  Drul_array<Real> end_ys = get_y_attachment_range ();

  extra_encompass_infos_ = get_extra_encompass_infos ();

  Interval additional_ys (0.0, 0.0);

  for (vsize i = 0; i < extra_encompass_infos_.size (); i++)
    {
      if (extra_encompass_infos_[i].extents_[X_AXIS].is_empty ())
        continue;

      Real y_place = linear_interpolate (
        extra_encompass_infos_[i].extents_[X_AXIS].center (),
        base_attachments_[RIGHT][X_AXIS], base_attachments_[LEFT][X_AXIS],
        end_ys[RIGHT], end_ys[LEFT]);
      Real encompass_place = extra_encompass_infos_[i].extents_[Y_AXIS][dir_];
      if (scm_is_eq (extra_encompass_infos_[i].type_, ly_symbol2scm ("inside"))
          && minmax (dir_, encompass_place, y_place) == encompass_place
          && (!extra_encompass_infos_[i].grob_->internal_has_interface (
                ly_symbol2scm ("key-signature-interface"))
              && !has_interface<Clef> (extra_encompass_infos_[i].grob_)
              && !extra_encompass_infos_[i].grob_->internal_has_interface (
                ly_symbol2scm ("time-signature-interface"))))
        {
          for (const auto d : {LEFT, RIGHT})
            additional_ys[d] = minmax (
              dir_, additional_ys[d],
              (dir_
               * (parameters_.encompass_object_range_overshoot_
                  + (y_place - encompass_place)
                      * (normalize (
                           extra_encompass_infos_[i].extents_[X_AXIS].center (),
                           base_attachments_[RIGHT][X_AXIS],
                           base_attachments_[LEFT][X_AXIS])
                         + (dir_ == LEFT ? 0 : -1)))));
        }
    }

  for (const auto d : {LEFT, RIGHT})
    end_ys[d] += additional_ys[d];

  configurations_ = enumerate_attachments (end_ys);
  for (vsize i = 0; i < note_columns_.size (); i++)
    encompass_infos_.push_back (get_encompass_info (note_columns_[i]));

  valid_ = true;

  musical_dy_ = 0.0;
  for (const auto d : {LEFT, RIGHT})
    {
      if (!is_broken_ && extremes_[d].slur_head_)
        musical_dy_ += d
                       * extremes_[d].slur_head_->relative_coordinate (
                         common_[Y_AXIS], Y_AXIS);
    }

  edge_has_beams_
    = (extremes_[LEFT].stem_ && Stem::get_beam (extremes_[LEFT].stem_))
      || (extremes_[RIGHT].stem_ && Stem::get_beam (extremes_[RIGHT].stem_));

  if (is_broken_)
    musical_dy_ = 0.0;
}

MAKE_SCHEME_CALLBACK (Slur, calc_control_points, "ly:slur::calc-control-points",
                      1)
SCM
Slur::calc_control_points (SCM smob)
{
  Spanner *me = unsmob<Spanner> (smob);

  Slur_score_state state;
  state.fill (me);

  if (!state.valid_)
    return SCM_EOL;
  if (state.configurations_.empty ())
    {
      me->warning (_ ("no viable slur configuration found"));
      return SCM_EOL;
    }

  state.generate_curves ();

  SCM end_ys = get_property (me, "positions");
  SCM inspect_quants = get_property (me, "inspect-quants");
  bool debug_slurs = from_scm<bool> (
    me->layout ()->lookup_variable (ly_symbol2scm ("debug-slur-scoring")));

  if (is_number_pair (inspect_quants))
    {
      debug_slurs = true;
      end_ys = inspect_quants;
    }

  Slur_configuration *best = NULL;
  if (is_number_pair (end_ys))
    best = state.get_forced_configuration (from_scm<Interval> (end_ys));
  else
    best = state.get_best_curve ();

  if (debug_slurs)
    {
      string total = best->card ();
      total += to_string (" TOTAL=%.2f idx=%zu", best->score (), best->index_);
      set_property (me, "annotation", ly_string2scm (total));
    }

  SCM controls = SCM_EOL;
  for (int i = 4; i--;)
    {
      Offset o
        = best->curve_.control_[i]
          - Offset (me->relative_coordinate (state.common_[X_AXIS], X_AXIS),
                    me->relative_coordinate (state.common_[Y_AXIS], Y_AXIS));
      controls = scm_cons (to_scm (o), controls);
    }

  return controls;
}

Slur_configuration *
Slur_score_state::get_forced_configuration (Interval ys) const
{
  Slur_configuration *best = NULL;
  Real mindist = 1e6;
  for (vsize i = 0; i < configurations_.size (); i++)
    {
      Real d
        = fabs (configurations_[i]->attachment_[LEFT][Y_AXIS] - ys[LEFT])
          + fabs (configurations_[i]->attachment_[RIGHT][Y_AXIS] - ys[RIGHT]);
      if (d < mindist)
        {
          best = configurations_[i].get ();
          mindist = d;
        }
    }

  while (!best->done ())
    best->run_next_scorer (*this);

  if (mindist > 1e5)
    programming_error ("cannot find quant");

  return best;
}

Slur_configuration *
Slur_score_state::get_best_curve () const
{
  std::priority_queue<Slur_configuration *, std::vector<Slur_configuration *>,
                      Slur_configuration_less>
    queue;
  for (vsize i = 0; i < configurations_.size (); i++)
    queue.push (configurations_[i].get ());

  Slur_configuration *best = NULL;
  while (true)
    {
      best = queue.top ();
      if (best->done ())
        break;

      queue.pop ();
      best->run_next_scorer (*this);
      queue.push (best);
    }

  return best;
}

Interval
Slur_score_state::breakable_bound_extent (Direction d) const
{
  Grob *paper_col = slur_->get_bound (d)->get_column ();
  Interval ret;
  ret.set_empty ();

  extract_grob_set (slur_, "encompass-objects", extra_encompasses);

  for (vsize i = 0; i < extra_encompasses.size (); i++)
    {
      Item *item = dynamic_cast<Item *> (extra_encompasses[i]);
      if (item && paper_col == item->get_column ())
        ret.unite (robust_relative_extent (item, common_[X_AXIS], X_AXIS));
    }

  return ret;
}

/*
  TODO: should analyse encompasses to determine sensible region, and
  should limit slopes available.
*/

Drul_array<Real>
Slur_score_state::get_y_attachment_range () const
{
  Drul_array<Real> end_ys;
  for (const auto d : {LEFT, RIGHT})
    {
      if (extremes_[d].note_column_)
        {
          Interval nc_extent
            = extremes_[d].note_column_->extent (common_[Y_AXIS], Y_AXIS);
          if (nc_extent.is_empty ())
            slur_->warning (
              _ ("slur trying to encompass an empty note column."));
          else
            end_ys[d]
              = dir_
                * std::max (std::max (dir_
                                        * (base_attachments_[d][Y_AXIS]
                                           + parameters_.region_size_ * dir_),
                                      dir_ * (dir_ + nc_extent[dir_])),
                            dir_ * base_attachments_[-d][Y_AXIS]);
        }
      else if (extremes_[d].slur_head_)
        {
          // allow only minimal movement
          end_ys[d] = base_attachments_[d][Y_AXIS] + 0.3 * dir_;
        }
      else
        end_ys[d]
          = base_attachments_[d][Y_AXIS] + parameters_.region_size_ * dir_;
    }

  return end_ys;
}

bool
spanner_less (Spanner *s1, Spanner *s2)
{
  Slice b1, b2;
  for (const auto d : {LEFT, RIGHT})
    {
      b1[d] = s1->get_bound (d)->get_column ()->get_rank ();
      b2[d] = s2->get_bound (d)->get_column ()->get_rank ();
    }

  return b2[LEFT] <= b1[LEFT] && b2[RIGHT] >= b1[RIGHT]
         && (b2[LEFT] != b1[LEFT] || b2[RIGHT] != b1[RIGHT]);
}

Drul_array<Offset>
Slur_score_state::get_base_attachments () const
{
  Drul_array<Offset> base_attachment;
  for (const auto d : {LEFT, RIGHT})
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
          if (stem && !Stem::is_invisible (stem)
              && extremes_[d].stem_dir_ == dir_ && Stem::get_beaming (stem, -d)
              && Stem::get_beam (stem)
              && (!spanner_less (slur_, Stem::get_beam (stem))
                  || has_same_beam_))
            y = extremes_[d].stem_extent_[Y_AXIS][dir_];
          else if (head)
            y = head->extent (common_[Y_AXIS], Y_AXIS)[dir_];
          y += dir_ * 0.5 * staff_space_;

          y = move_away_from_staffline (y, head);

          Grob *fh = Note_column::first_head (extremes_[d].note_column_);
          x = (fh ? fh->extent (common_[X_AXIS], X_AXIS)
                  : extremes_[d].bound_->extent (common_[X_AXIS], X_AXIS))
                .center ();
          if (!std::isfinite (x))
            x = extremes_[d]
                  .note_column_->extent (common_[X_AXIS], X_AXIS)
                  .center ();
          if (!std::isfinite (y))
            y = extremes_[d]
                  .note_column_->extent (common_[Y_AXIS], Y_AXIS)
                  .center ();
        }
      else if (head)
        {
          y = head->extent (common_[Y_AXIS], Y_AXIS)
                .linear_combination (0.5 * dir_);

          // Don't "move_away_from_staffline" because that makes it
          // harder to recognize the specific attachment point
          x = head->extent (common_[X_AXIS], X_AXIS)[-d];
        }

      base_attachment[d] = Offset (x, y);
    }

  for (const auto d : {LEFT, RIGHT})
    {
      if (!extremes_[d].note_column_ && !extremes_[d].slur_head_)
        {
          Real x = 0;
          Real y = 0;

          Interval ext = breakable_bound_extent (d);
          if (ext.is_empty ())
            ext = Axis_group_interface::generic_bound_extent (
              extremes_[d].bound_, common_[X_AXIS], X_AXIS);
          x = ext[-d];

          Grob *col = (d == LEFT) ? note_columns_[0] : note_columns_.back ();

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

  for (const auto d : {LEFT, RIGHT})
    {
      for (const auto a : {X_AXIS, Y_AXIS})
        {
          Real &b = base_attachment[d][a];

          if (!std::isfinite (b))
            {
              programming_error ("slur attachment is inf/nan");
              b = 0.0;
            }
        }
    }

  return base_attachment;
}

Real
Slur_score_state::move_away_from_staffline (Real y, Grob *on_staff) const
{
  if (!on_staff)
    return y;

  Grob *staff_symbol = Staff_symbol_referencer::get_staff_symbol (on_staff);
  if (!staff_symbol)
    return y;

  Real pos = (y - staff_symbol->relative_coordinate (common_[Y_AXIS], Y_AXIS))
             * 2.0 / staff_space_;

  if (fabs (pos - round_halfway_up (pos)) < 0.2
      && Staff_symbol_referencer::on_staff_line (on_staff,
                                                 static_cast<int> (rint (pos))))
    y += 1.5 * staff_space_ * dir_ / 10;

  return y;
}

vector<Offset>
Slur_score_state::generate_avoid_offsets () const
{
  vector<Offset> avoid;
  vector<Grob *> encompasses = note_columns_;

  for (vsize i = 0; i < encompasses.size (); i++)
    {
      if (extremes_[LEFT].note_column_ == encompasses[i]
          || extremes_[RIGHT].note_column_ == encompasses[i])
        continue;

      Encompass_info inf (get_encompass_info (encompasses[i]));
      Real y = dir_ * (std::max (dir_ * inf.head_, dir_ * inf.stem_));

      avoid.push_back (
        Offset (inf.x_, y + dir_ * parameters_.free_head_distance_));
    }

  extract_grob_set (slur_, "encompass-objects", extra_encompasses);
  for (vsize i = 0; i < extra_encompasses.size (); i++)
    {
      if (has_interface<Slur> (extra_encompasses[i]))
        {
          Grob *small_slur = extra_encompasses[i];
          Bezier b = Slur::get_curve (small_slur);

          Offset z = b.curve_point (0.5);
          z += Offset (
            small_slur->relative_coordinate (common_[X_AXIS], X_AXIS),
            small_slur->relative_coordinate (common_[Y_AXIS], Y_AXIS));

          z[Y_AXIS] += dir_ * parameters_.free_slur_distance_;
          avoid.push_back (z);
        }
      else if (scm_is_eq (get_property (extra_encompasses[i], "avoid-slur"),
                          ly_symbol2scm ("inside")))
        {
          Grob *g = extra_encompasses[i];
          Interval xe = g->extent (common_[X_AXIS], X_AXIS);
          Interval ye = g->extent (common_[Y_AXIS], Y_AXIS);

          if (!xe.is_empty () && !ye.is_empty ())
            avoid.push_back (Offset (xe.center (), ye[dir_]));
        }
    }
  return avoid;
}

void
Slur_score_state::generate_curves () const
{
  Real r_0 = from_scm<double> (get_property (slur_, "ratio"), 0.33);
  Real h_inf
    = staff_space_ * from_scm<double> (get_property (slur_, "height-limit"));

  vector<Offset> avoid = generate_avoid_offsets ();
  for (vsize i = 0; i < configurations_.size (); i++)
    configurations_[i]->generate_curve (*this, r_0, h_inf, avoid);
}

vector<unique_ptr<Slur_configuration>>
Slur_score_state::enumerate_attachments (Drul_array<Real> end_ys) const
{
  vector<unique_ptr<Slur_configuration>> scores;

  Drul_array<Offset> os;
  os[LEFT] = base_attachments_[LEFT];
  Real minimum_length
    = staff_space_
      * from_scm<double> (get_property (slur_, "minimum-length"), 2.0);

  for (int i = 0; dir_ * os[LEFT][Y_AXIS] <= dir_ * end_ys[LEFT]; i++)
    {
      os[RIGHT] = base_attachments_[RIGHT];
      for (int j = 0; dir_ * os[RIGHT][Y_AXIS] <= dir_ * end_ys[RIGHT]; j++)
        {

          Drul_array<bool> attach_to_stem;
          for (const auto d : {LEFT, RIGHT})
            {
              os[d][X_AXIS] = base_attachments_[d][X_AXIS];
              if (extremes_[d].stem_ && !Stem::is_invisible (extremes_[d].stem_)
                  && extremes_[d].stem_dir_ == dir_)
                {
                  Interval stem_y = extremes_[d].stem_extent_[Y_AXIS];
                  stem_y.widen (0.25 * staff_space_);
                  if (stem_y.contains (os[d][Y_AXIS]))
                    {
                      os[d][X_AXIS]
                        = extremes_[d].stem_extent_[X_AXIS][-d] - d * 0.3;
                      attach_to_stem[d] = true;
                    }
                  else if (dir_ * extremes_[d].stem_extent_[Y_AXIS][dir_]
                             < dir_ * os[d][Y_AXIS]
                           && !extremes_[d].stem_extent_[X_AXIS].is_empty ())

                    os[d][X_AXIS] = extremes_[d].stem_extent_[X_AXIS].center ();
                }
            }

          Offset dz;
          dz = os[RIGHT] - os[LEFT];
          if (dz[X_AXIS] < minimum_length
              || fabs (dz[Y_AXIS] / dz[X_AXIS]) > parameters_.max_slope_)
            {
              for (const auto d : {LEFT, RIGHT})
                {
                  if (extremes_[d].slur_head_
                      && !extremes_[d].slur_head_x_extent_.is_empty ())
                    {
                      os[d][X_AXIS]
                        = extremes_[d].slur_head_x_extent_.center ();
                      attach_to_stem[d] = false;
                    }
                }
            }

          dz = (os[RIGHT] - os[LEFT]).direction ();
          for (const auto d : {LEFT, RIGHT})
            {
              if (extremes_[d].slur_head_ && !attach_to_stem[d])
                {
                  /* Horizontally move tilted slurs a little.  Move
                     more for bigger tilts.

                     TODO: parameter */
                  os[d][X_AXIS] -= dir_
                                   * extremes_[d].slur_head_x_extent_.length ()
                                   * dz[Y_AXIS] / 3;
                }
            }

          scores.push_back (
            Slur_configuration::new_config (os, scores.size ()));

          os[RIGHT][Y_AXIS] += dir_ * staff_space_ / 2;
        }

      os[LEFT][Y_AXIS] += dir_ * staff_space_ / 2;
    }

  return scores;
}

vector<Extra_collision_info>
Slur_score_state::get_extra_encompass_infos () const
{
  extract_grob_set (slur_, "encompass-objects", encompasses);
  vector<Extra_collision_info> collision_infos;
  for (vsize i = encompasses.size (); i--;)
    {
      if (has_interface<Slur> (encompasses[i]))
        {
          Spanner *small_slur = dynamic_cast<Spanner *> (encompasses[i]);
          Bezier b = Slur::get_curve (small_slur);

          Offset relative (
            small_slur->relative_coordinate (common_[X_AXIS], X_AXIS),
            small_slur->relative_coordinate (common_[Y_AXIS], Y_AXIS));

          for (int k = 0; k < 3; k++)
            {
              const Direction hdir (k - 1);

              /*
                Only take bound into account if small slur starts
                together with big slur.
              */
              if (hdir
                  && small_slur->get_bound (hdir) != slur_->get_bound (hdir))
                continue;

              Offset z = b.curve_point (k / 2.0);
              z += relative;

              Interval yext;
              yext.set_full ();
              yext[dir_] = z[Y_AXIS] + dir_ * thickness_ * 1.0;

              Interval xext (-1, 1);
              xext = xext * (thickness_ * 2) + z[X_AXIS];
              Extra_collision_info info (
                small_slur, hdir, xext, yext,
                parameters_.extra_object_collision_penalty_);
              collision_infos.push_back (info);
            }
        }
      else
        {
          Grob *g = encompasses[i];
          Interval xe = g->extent (common_[X_AXIS], X_AXIS);
          Interval ye = g->extent (common_[Y_AXIS], Y_AXIS);
          if (g->internal_has_interface (ly_symbol2scm ("dots-interface")))
            ye.widen (0.2);

          Real xp = 0.0;
          Real penalty = parameters_.extra_object_collision_penalty_;
          if (has_interface<Accidental_interface> (g))
            {
              penalty = parameters_.accidental_collision_;

              Rational alt
                = from_scm<Rational> (get_property (g, "alteration"));
              SCM scm_style = get_property (g, "style");
              if (!scm_is_symbol (scm_style)
                  && !from_scm<bool> (get_property (g, "parenthesized"))
                  && !from_scm<bool> (get_property (g, "restore-first")))
                {
                  if (alt == FLAT_ALTERATION || alt == DOUBLE_FLAT_ALTERATION)
                    xp = LEFT;
                  else if (alt == SHARP_ALTERATION)
                    xp = 0.5 * dir_;
                  else if (alt == NATURAL_ALTERATION)
                    xp = -dir_;
                }
            }

          ye.widen (thickness_ * 0.5);
          xe.widen (thickness_ * 1.0);
          Extra_collision_info info (g, xp, xe, ye, penalty);
          collision_infos.push_back (info);
        }
    }

  return collision_infos;
}

Extra_collision_info::Extra_collision_info (Grob *g, Real idx, Interval x,
                                            Interval y, Real p)
{
  idx_ = idx;
  extents_[X_AXIS] = x;
  extents_[Y_AXIS] = y;
  penalty_ = p;
  grob_ = g;
  type_ = get_property (g, "avoid-slur");
}

Extra_collision_info::Extra_collision_info ()
{
  idx_ = 0.0;
  penalty_ = 0.;
  grob_ = 0;
  type_ = SCM_EOL;
}
