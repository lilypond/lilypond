/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>


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

#include "tie-formatting-problem.hh"

#include "axis-group-interface.hh"
#include "paper-column.hh"
#include "bezier.hh"
#include "directional-element-interface.hh"
#include "libc-extension.hh"
#include "misc.hh"
#include "note-head.hh"
#include "rhythmic-head.hh"
#include "semi-tie.hh"
#include "spanner.hh"
#include "staff-symbol-referencer.hh"
#include "stem.hh"
#include "tie-configuration.hh"
#include "tie.hh"
#include "warn.hh"
#include "pointer-group-interface.hh"
#include "output-def.hh"

#include <algorithm>
#include <cmath>
#include <cstdio>
#include <set>
#include <string>
#include <vector>

using std::set;
using std::string;
using std::vector;

template <typename Container>
auto &
boundary (const Container &v, Direction dir, vsize i)
{
  assert (dir);
  return v[(dir == LEFT) ? i : v.size () - 1 - i];
}

void
Tie_formatting_problem::print_ties_configuration (
  Ties_configuration const *ties)
{
  for (vsize i = 0; i < ties->size (); i++)
    {
      char const *man_pos
        = (specifications_[i].has_manual_position_) ? "(M)" : "";
      char const *man_dir = (specifications_[i].has_manual_dir_) ? "(M)" : "";
      char const *dir = ((*ties)[i].dir_ == UP) ? "up" : "dn";

      printf ("(P%d%s, %s%s) ", (*ties)[i].position_, man_pos, dir, man_dir);
    }
  printf ("\n");
}

Interval
Tie_formatting_problem::get_attachment (Real y, Drul_array<int> columns) const
{
  Interval attachments (0, 0);

  for (const auto d : {LEFT, RIGHT})
    {
      auto i = chord_outlines_.find (Tie_rank_and_dir (columns[d], d));
      if (i == chord_outlines_.end ())
        programming_error ("Cannot find chord outline");
      else
        attachments[d] = i->second.height (y);
    }

  return attachments;
}

Tie_formatting_problem::Tie_formatting_problem ()
{
  x_refpoint_ = 0;
  y_refpoint_ = 0;
  use_horizontal_spacing_ = true;
}

void
Tie_formatting_problem::set_column_chord_outline (vector<Item *> bounds,
                                                  Direction dir,
                                                  int column_rank)
{
  Real staff_space = Staff_symbol_referencer::staff_space (bounds[0]);

  vector<Box> boxes;
  vector<Box> head_boxes;

  Grob *stem = 0;
  for (vsize i = 0; i < bounds.size (); i++)
    {
      Grob *head = bounds[i];
      if (!has_interface<Note_head> (head))
        continue;

      if (!stem)
        stem = unsmob<Grob> (get_object (head, "stem"));

      Real p = Staff_symbol_referencer::get_position (head);
      Interval y ((p - 1) * 0.5 * staff_space, (p + 1) * 0.5 * staff_space);

      Interval x = head->extent (x_refpoint_, X_AXIS);
      head_boxes.push_back (Box (x, y));
      boxes.push_back (Box (x, y));

      Grob *dots = Rhythmic_head::get_dots (head);
      if (dir == LEFT && dots)
        {
          Interval x = dots->extent (x_refpoint_, X_AXIS);
          int p = int (Staff_symbol_referencer::get_position (dots));

          /*
            TODO: shouldn't this use column-rank dependent key?
          */
          dot_positions_.insert (p);
          dot_x_.unite (x);

          Interval y (dots->extent (dots, Y_AXIS));
          y.translate (p * staff_space * 0.5);

          boxes.push_back (Box (x, y));
        }
    }

  const Tie_rank_and_dir key {column_rank, dir};

  if (stem)
    {
      if (Stem::is_normal_stem (stem))
        {
          Interval x;
          x.add_point (stem->relative_coordinate (x_refpoint_, X_AXIS));
          x.widen (staff_space / 20); // ugh.
          Interval y;

          Real stem_end_position = 0.0;
          if (Stem::is_cross_staff (stem))
            stem_end_position = get_grob_direction (stem) * infinity_f;
          else
            {
              if (use_horizontal_spacing_ || !Stem::get_beam (stem))
                stem_end_position
                  = stem->extent (stem, Y_AXIS)[get_grob_direction (stem)];
              else
                // May want to change this to the stem's pure height...
                stem_end_position
                  = Stem::head_positions (stem)[get_grob_direction (stem)]
                    * staff_space * .5;
            }

          y.add_point (stem_end_position);

          Direction stemdir = get_grob_direction (stem);
          y.add_point (Stem::head_positions (stem)[-stemdir] * staff_space
                       * .5);

          /*
            add extents of stem.
          */
          boxes.push_back (Box (x, y));

          stem_extents_[key].unite (Box (x, y));

          if (dir == LEFT)
            {
              Grob *flag = Stem::flag (stem);
              if (flag)
                {
                  Grob *commony = stem->common_refpoint (flag, Y_AXIS);
                  boxes.push_back (Box (flag->extent (x_refpoint_, X_AXIS),
                                        flag->extent (commony, Y_AXIS)));
                }
            }
        }
      else
        {
          Grob *head = Stem::support_head (stem);

          /*
            In case of invisible stem, don't pass x-center of heads.
          */
          Real x_center = head->extent (x_refpoint_, X_AXIS).center ();
          Interval x_ext;
          x_ext[-dir] = x_center;
          x_ext[dir] = infinity_f * dir;
          Interval y_ext;
          for (vsize j = 0; j < head_boxes.size (); j++)
            y_ext.unite (head_boxes[j][Y_AXIS]);

          boxes.push_back (Box (x_ext, y_ext));
        }

      extract_grob_set (stem, "note-heads", heads);
      for (vsize i = 0; i < heads.size (); i++)
        {
          if (find (bounds.begin (), bounds.end (),
                    dynamic_cast<Item *> (heads[i]))
              == bounds.end ())
            {
              /*
                other untied notes in the same chord.
              */

              Interval y = Staff_symbol_referencer::extent_in_staff (heads[i]);
              Interval x = heads[i]->extent (x_refpoint_, X_AXIS);
              boxes.push_back (Box (x, y));
            }

          Grob *acc = unsmob<Grob> (get_object (heads[i], "accidental-grob"));
          if (acc)
            get_property (
              acc, "after-line-breaking"); /* trigger tie-related suicide */

          if (acc && acc->is_live () && dir == RIGHT)
            {
              boxes.push_back (
                Box (acc->extent (x_refpoint_, X_AXIS),
                     Staff_symbol_referencer::extent_in_staff (acc)));
            }

          head_positions_[column_rank].add_point (
            int (Staff_symbol_referencer::get_position (heads[i])));
        }
    }

  for (const auto updowndir : {DOWN, UP})
    {
      Interval x;
      Interval y;
      if (head_boxes.size ())
        {
          Box b = boundary (head_boxes, updowndir, 0);
          x = b[X_AXIS];
          x[-dir] = b[X_AXIS].linear_combination (-dir / 2);
          y[-updowndir] = b[Y_AXIS][updowndir];
          y[updowndir] = updowndir * infinity_f;
        }

      if (!x.is_empty ())
        boxes.push_back (Box (x, y));
    }

  chord_outlines_[key]
    = Skyline (boxes, Y_AXIS, -dir).padded (details_.skyline_padding_);
  if (bounds[0]->break_status_dir ())
    {
      Interval iv (Axis_group_interface::staff_extent (
        bounds[0], x_refpoint_, X_AXIS, y_refpoint_, Y_AXIS));
      if (iv.is_empty ())
        iv.add_point (bounds[0]->relative_coordinate (x_refpoint_, X_AXIS));

      chord_outlines_[key].set_minimum_height (iv[-dir]);
    }
  else
    {
      Interval x;
      for (vsize j = 0; j < head_boxes.size (); j++)
        {
          x.unite (head_boxes[j][X_AXIS]);
        }

      chord_outlines_[key].set_minimum_height (x[dir]);
    }

  head_extents_[key].set_empty ();
  for (vsize i = 0; i < head_boxes.size (); i++)
    {
      head_extents_[key].unite (head_boxes[i]);
    }
}

void
Tie_formatting_problem::set_chord_outline (vector<Item *> bounds, Direction dir)

{
  vector<int> ranks;
  for (vsize i = 0; i < bounds.size (); i++)
    ranks.push_back (bounds[i]->get_column ()->get_rank ());

  std::sort (ranks.begin (), ranks.end ());
  auto last = std::unique (ranks.begin (), ranks.end ());

  for (auto it = ranks.begin (); it != last; ++it)
    {
      vector<Item *> col_items;
      for (vsize j = 0; j < bounds.size (); j++)
        {
          if (bounds[j]->get_column ()->get_rank () == *it)
            col_items.push_back (bounds[j]);
        }

      set_column_chord_outline (col_items, dir, *it);
    }
}

void
Tie_formatting_problem::from_tie (Grob *tie)
{
  vector<Grob *> ties;
  ties.push_back (tie);
  from_ties (ties);

  details_.from_grob (tie);
}

Grob *
Tie_formatting_problem::common_x_refpoint () const
{
  return x_refpoint_;
}

void
Tie_formatting_problem::from_ties (vector<Grob *> const &ties)
{
  if (ties.empty ())
    return;

  x_refpoint_ = ties[0];
  y_refpoint_ = ties[0];
  for (vsize i = 0; i < ties.size (); i++)
    {
      Spanner *tie = dynamic_cast<Spanner *> (ties[i]);
      Item *l = tie->get_bound (LEFT);
      Item *r = tie->get_bound (RIGHT);

      x_refpoint_ = l->common_refpoint (x_refpoint_, X_AXIS);
      x_refpoint_ = r->common_refpoint (x_refpoint_, X_AXIS);

      if (!l->break_status_dir ())
        y_refpoint_ = l->common_refpoint (y_refpoint_, Y_AXIS);
      if (!r->break_status_dir ())
        y_refpoint_ = r->common_refpoint (y_refpoint_, Y_AXIS);
    }

  details_.from_grob (ties[0]);

  for (const auto d : {LEFT, RIGHT})
    {
      vector<Item *> bounds;

      for (vsize i = 0; i < ties.size (); i++)
        {
          Spanner *tie = dynamic_cast<Spanner *> (ties[i]);
          Item *it = tie->get_bound (d);
          if (it->break_status_dir ())
            it = it->get_column ();

          bounds.push_back (it);
        }

      set_chord_outline (bounds, d);
    }

  for (vsize i = 0; i < ties.size (); i++)
    {
      Spanner *tie = dynamic_cast<Spanner *> (ties[i]);
      Tie_specification spec;
      spec.from_grob (tie);

      for (const auto d : {LEFT, RIGHT})
        {
          spec.note_head_drul_[d] = Tie::head (tie, d);
          spec.column_ranks_[d] = Tie::get_column_rank (tie, d);
        }
      specifications_.push_back (spec);
    }
}

void
Tie_formatting_problem::from_semi_ties (vector<Grob *> const &semi_ties,
                                        Direction head_dir)
{
  if (semi_ties.empty ())
    return;

  use_horizontal_spacing_ = false;
  details_.from_grob (semi_ties[0]);
  vector<Item *> heads;

  int column_rank = -1;
  for (vsize i = 0; i < semi_ties.size (); i++)
    {
      Item *semi_tie = dynamic_cast<Item *> (semi_ties[i]);
      Tie_specification spec;
      Item *head = Semi_tie::head (semi_tie);

      if (!head)
        programming_error ("LV tie without head?!");

      if (head)
        {
          spec.position_ = int (Staff_symbol_referencer::get_position (head));
        }

      spec.from_grob (semi_tie);

      spec.note_head_drul_[head_dir] = head;
      column_rank = Semi_tie::get_column_rank (semi_tie);
      spec.column_ranks_ = Drul_array<int> (column_rank, column_rank);
      heads.push_back (head);
      specifications_.push_back (spec);
    }

  x_refpoint_ = semi_ties[0];
  y_refpoint_ = semi_ties[0];

  for (vsize i = 0; i < semi_ties.size (); i++)
    {
      x_refpoint_ = semi_ties[i]->common_refpoint (x_refpoint_, X_AXIS);
      y_refpoint_ = semi_ties[i]->common_refpoint (y_refpoint_, Y_AXIS);
    }
  for (vsize i = 0; i < heads.size (); i++)
    {
      x_refpoint_ = heads[i]->common_refpoint (x_refpoint_, X_AXIS);
      y_refpoint_ = heads[i]->common_refpoint (y_refpoint_, Y_AXIS);
    }

  set_chord_outline (heads, head_dir);

  const Tie_rank_and_dir head_key {column_rank, head_dir};
  const Tie_rank_and_dir open_key {column_rank, -head_dir};
  Real extremal = chord_outlines_[head_key].max_height ();

  chord_outlines_[open_key] = Skyline (head_dir);
  chord_outlines_[open_key].set_minimum_height (extremal - head_dir * 1.5);
}

Tie_specification
Tie_formatting_problem::get_tie_specification (int i) const
{
  return specifications_[i];
}

/*
  Return configuration, create it if necessary.
*/
Tie_configuration *
Tie_formatting_problem::get_configuration (int pos, Direction dir,
                                           Drul_array<int> columns,
                                           bool tune_dy) const
{
  const Tie_configuration_map_key key {pos, dir, columns[LEFT], columns[RIGHT]};

  Tie_configuration_map::const_iterator f = possibilities_.find (key);
  if (f != possibilities_.end ())
    {
      return f->second.get ();
    }

  // Does it actually make sense to call this function const?
  auto *const me = const_cast<Tie_formatting_problem *> (this);

  auto conf = generate_configuration (pos, dir, columns, tune_dy);
  auto *alias = conf.get ();
  me->possibilities_[key] = std::move (conf);
  return alias;
}

std::unique_ptr<Tie_configuration>
Tie_formatting_problem::generate_configuration (int pos, Direction dir,
                                                Drul_array<int> columns,
                                                bool y_tune) const
{
  auto conf = std::make_unique<Tie_configuration> ();
  conf->position_ = pos;
  conf->dir_ = dir;

  conf->column_ranks_ = columns;

  Real y = conf->position_ * 0.5 * details_.staff_space_;

  if (dot_positions_.find (pos) != dot_positions_.end ())
    {
      conf->delta_y_ += dir * 0.25 * details_.staff_space_;
      y_tune = false;
    }

  if (y_tune
      && std::max (
           fabs (get_head_extent (columns[LEFT], LEFT, Y_AXIS)[dir] - y),
           fabs (get_head_extent (columns[RIGHT], RIGHT, Y_AXIS)[dir] - y))
           < 0.25
      && !Staff_symbol_referencer::on_line (details_.staff_symbol_referencer_,
                                            pos))
    {
      conf->delta_y_ = (get_head_extent (columns[LEFT], LEFT, Y_AXIS)[dir] - y)
                       + dir * details_.outer_tie_vertical_gap_;
    }

  if (y_tune)
    {
      conf->attachment_x_
        = get_attachment (y + conf->delta_y_, conf->column_ranks_);
      Real h = conf->height (details_);

      /*
        TODO:

        - should make sliding criterion, should flatten ties if

        - they're just the wrong (ie. touching line at top & bottom)
        size.

       */
      Interval staff_span = Staff_symbol_referencer::staff_span (
        details_.staff_symbol_referencer_);
      staff_span.widen (-1);
      bool const within_staff = staff_span.contains (pos);
      if (head_positions_slice (columns[LEFT]).contains (pos)
          || head_positions_slice (columns[RIGHT]).contains (pos)
          || within_staff)
        {
          if (h < details_.intra_space_threshold_ * 0.5 * details_.staff_space_)
            {
              if (Staff_symbol_referencer::on_line (
                    details_.staff_symbol_referencer_, pos))
                {
                  conf->delta_y_ += dir * details_.tip_staff_line_clearance_
                                    * 0.5 * details_.staff_space_;
                }
              else if (within_staff)
                {
                  conf->center_tie_vertically (details_);
                }
            }
          else
            {
              Real top_y = y + conf->delta_y_ + conf->dir_ * h;
              Real top_pos = top_y / (0.5 * details_.staff_space_);
              int round_pos = int (round_halfway_up (top_pos));

              /* TODO: should use other variable? */
              Real clearance = details_.center_staff_line_clearance_;
              if (fabs (top_pos - round_pos) < clearance
                  && Staff_symbol_referencer::on_staff_line (
                    details_.staff_symbol_referencer_, round_pos))
                {
                  Real new_y = (round_pos + clearance * conf->dir_) * 0.5
                               * details_.staff_space_;
                  conf->delta_y_ = (new_y - top_y);
                }
            }
        }
    }
  conf->attachment_x_
    = get_attachment (y + conf->delta_y_, conf->column_ranks_);
  if (conf->height (details_)
      < details_.intra_space_threshold_ * 0.5 * details_.staff_space_)
    {
      /*
        This is less sensible for long ties, since those are more
        horizontal.
      */
      Interval close_by
        = get_attachment (y + conf->delta_y_
                            + (dir * details_.intra_space_threshold_ * 0.25
                               * details_.staff_space_),
                          conf->column_ranks_);

      conf->attachment_x_.intersect (close_by);
    }

  conf->attachment_x_.widen (-details_.x_gap_);

  if (conf->column_span_length ())
    {
      /*
        avoid the stems that we attach to as well. We don't do this
        for semities (span length = 0)

        It would be better to check D against HEAD-DIRECTION if
        applicable.
      */
      for (const auto d : {LEFT, RIGHT})
        {
          Real y
            = conf->position_ * details_.staff_space_ * 0.5 + conf->delta_y_;
          if (get_stem_extent (conf->column_ranks_[d], d, X_AXIS).is_empty ()
              || !get_stem_extent (conf->column_ranks_[d], d, Y_AXIS)
                    .contains (y))
            continue;

          conf->attachment_x_[d]
            = d
              * std::min (
                d * conf->attachment_x_[d],
                d
                  * (get_stem_extent (conf->column_ranks_[d], d, X_AXIS)[-d]
                     - d * details_.stem_gap_));
        }
    }
  return conf;
}

Interval
Tie_formatting_problem::get_head_extent (int col, Direction d, Axis a) const
{
  auto i = head_extents_.find (Tie_rank_and_dir (col, d));
  if (i != head_extents_.end ())
    return (*i).second[a];
  else
    return Interval ();
}

Interval
Tie_formatting_problem::get_stem_extent (int col, Direction d, Axis a) const
{
  auto i = stem_extents_.find (Tie_rank_and_dir (col, d));
  if (i != stem_extents_.end ())
    return (*i).second[a];
  else
    return Interval ();
}

/**
   TIE_IDX and TIES_CONF are optional.
 */
Real
Tie_formatting_problem::score_aptitude (Tie_configuration *conf,
                                        Tie_specification const &spec,
                                        Ties_configuration *ties_conf,
                                        vsize tie_idx) const
{
  Real penalty = 0.0;
  Real curve_y = conf->position_ * details_.staff_space_ * 0.5 + conf->delta_y_;
  Real tie_y = spec.position_ * details_.staff_space_ * 0.5;
  if (Direction (curve_y - tie_y) != conf->dir_)
    {
      Real p = details_.wrong_direction_offset_penalty_;
      if (ties_conf)
        ties_conf->add_tie_score (p, tie_idx, "wrong dir");
      else
        penalty += p;
    }

  {
    Real relevant_dist = std::max (fabs (curve_y - tie_y) - 0.5, 0.0);
    Real p = details_.vertical_distance_penalty_factor_
             * convex_amplifier (1.0, 0.9, relevant_dist);
    if (ties_conf)
      ties_conf->add_tie_score (p, tie_idx, "vdist");
    else
      penalty += p;
  }

  for (const auto d : {LEFT, RIGHT})
    {
      if (!spec.note_head_drul_[d])
        continue;

      Interval head_x = spec.note_head_drul_[d]->extent (x_refpoint_, X_AXIS);
      Real dist = head_x.distance (conf->attachment_x_[d]);

      /*
        TODO: flatten with log or sqrt.
       */
      Real p = details_.horizontal_distance_penalty_factor_
               * convex_amplifier (1.25, 1.0, dist);
      if (ties_conf)
        ties_conf->add_tie_score (p, tie_idx,
                                  (d == LEFT) ? "lhdist" : "rhdist");
      else
        penalty += p;
    }

  if (ties_conf && ties_conf->size () == 1)
    {
      Drul_array<Grob *> stems;
      for (const auto d : {LEFT, RIGHT})
        {
          if (!spec.note_head_drul_[d])
            continue;

          Grob *stem
            = unsmob<Grob> (get_object (spec.note_head_drul_[d], "stem"));
          if (stem && Stem::is_normal_stem (stem))
            stems[d] = stem;
        }

      bool tie_stem_dir_ok = true;
      bool tie_position_dir_ok = true;
      if (stems[LEFT] && !stems[RIGHT])
        tie_stem_dir_ok = conf->dir_ != get_grob_direction (stems[LEFT]);
      else if (!stems[LEFT] && stems[RIGHT])
        tie_stem_dir_ok = conf->dir_ != get_grob_direction (stems[RIGHT]);
      else if (stems[LEFT] && stems[RIGHT]
               && get_grob_direction (stems[LEFT])
                    == get_grob_direction (stems[RIGHT]))
        tie_stem_dir_ok = conf->dir_ != get_grob_direction (stems[LEFT]);
      else if (spec.position_)
        tie_position_dir_ok = conf->dir_ == Direction (spec.position_);

      if (!tie_stem_dir_ok)
        ties_conf->add_score (details_.same_dir_as_stem_penalty_,
                              "tie/stem dir");
      if (!tie_position_dir_ok)
        ties_conf->add_score (details_.same_dir_as_stem_penalty_,
                              "tie/pos dir");
    }

  return penalty;
}

Slice
Tie_formatting_problem::head_positions_slice (int rank) const
{
  Position_extent_map::const_iterator i (head_positions_.find (rank));
  if (i != head_positions_.end ())
    {
      return (*i).second;
    }
  Slice empty;
  return empty;
}

/*
  Score a configuration, ie. how well these ties looks without regard
  to the note heads that they should connect to.
 */
void
Tie_formatting_problem::score_configuration (Tie_configuration *conf) const
{
  if (conf->is_scored ())
    {
      return;
    }

  Real length = conf->attachment_x_.length ();

  Real length_penalty
    = peak_around (0.33 * details_.min_length_, details_.min_length_, length);
  conf->add_score (details_.min_length_penalty_factor_ * length_penalty,
                   "minlength");

  Real tip_pos = conf->position_ + conf->delta_y_ / 0.5 * details_.staff_space_;
  Real tip_y = tip_pos * details_.staff_space_ * 0.5;
  Real height = conf->height (details_);

  Real top_y = tip_y + conf->dir_ * height;
  Real top_pos = 2 * top_y / details_.staff_space_;
  Real round_top_pos = rint (top_pos);
  Interval staff_span
    = Staff_symbol_referencer::staff_span (details_.staff_symbol_referencer_);
  if (Staff_symbol_referencer::on_line (details_.staff_symbol_referencer_,
                                        int (round_top_pos))
      && staff_span[UP] * 0.5 > top_y)
    {
      conf->add_score (
        details_.staff_line_collision_penalty_
          * peak_around (0.1 * details_.center_staff_line_clearance_,
                         details_.center_staff_line_clearance_,
                         fabs (top_pos - round_top_pos)),
        "line center");
    }

  int rounded_tip_pos = int (rint (tip_pos));
  staff_span.widen (-1);
  if (Staff_symbol_referencer::on_line (details_.staff_symbol_referencer_,
                                        rounded_tip_pos)
      && (head_positions_slice (conf->column_ranks_[LEFT])
            .contains (rounded_tip_pos)
          || head_positions_slice (conf->column_ranks_[RIGHT])
               .contains (rounded_tip_pos)
          || staff_span.contains (rounded_tip_pos)))
    {
      conf->add_score (
        details_.staff_line_collision_penalty_
          * peak_around (0.1 * details_.tip_staff_line_clearance_,
                         details_.tip_staff_line_clearance_,
                         fabs (tip_pos - rint (tip_pos))),
        "tipline");
    }

  if (!dot_x_.is_empty ())
    {
      /* use left edge? */
      Real x = dot_x_.center ();

      Bezier b = conf->get_transformed_bezier (details_);
      if (b.control_point_extent (X_AXIS).contains (x))
        {
          Real y = b.get_other_coordinate (X_AXIS, x);

          for (set<int>::const_iterator i (dot_positions_.begin ());
               i != dot_positions_.end (); i++)
            {
              int dot_pos = (*i);
              conf->add_score (
                details_.dot_collision_penalty_
                  * peak_around (
                    .1 * details_.dot_collision_clearance_,
                    details_.dot_collision_clearance_,
                    fabs (dot_pos * details_.staff_space_ * 0.5 - y)),
                "dot collision");
            }
        }
    }

  conf->set_scored ();
}

void
Tie_formatting_problem::score_ties_aptitude (Ties_configuration *ties) const
{
  if (ties->size () != specifications_.size ())
    {
      programming_error ("Huh? Mismatch between sizes.");
      return;
    }

  for (vsize i = 0; i < ties->size (); i++)
    score_aptitude (&(*ties)[i], specifications_[i], ties, i);
}

void
Tie_formatting_problem::score_ties (Ties_configuration *ties) const
{
  if (ties->is_scored ())
    return;

  score_ties_configuration (ties);
  score_ties_aptitude (ties);
  ties->set_scored ();
}

void
Tie_formatting_problem::score_ties_configuration (
  Ties_configuration *ties) const
{
  for (vsize i = 0; i < ties->size (); i++)
    {
      score_configuration (&(*ties)[i]);
      ties->add_tie_score ((*ties)[i].score (), i, "conf");
    }

  Real last_edge = 0.0;
  Real last_center = 0.0;
  for (vsize i = 0; i < ties->size (); i++)
    {
      Bezier b ((*ties)[i].get_transformed_bezier (details_));

      Real center = b.curve_point (0.5)[Y_AXIS];
      Real edge = b.curve_point (0.0)[Y_AXIS];

      if (i)
        {
          if (edge <= last_edge)
            ties->add_score (details_.tie_column_monotonicity_penalty_,
                             "monoton edge");
          if (center <= last_center)
            ties->add_score (details_.tie_column_monotonicity_penalty_,
                             "monoton cent");

          ties->add_score (
            details_.tie_tie_collision_penalty_
              * peak_around (0.1 * details_.tie_tie_collision_distance_,
                             details_.tie_tie_collision_distance_,
                             fabs (center - last_center)),
            "tietie center");
          ties->add_score (
            details_.tie_tie_collision_penalty_
              * peak_around (0.1 * details_.tie_tie_collision_distance_,
                             details_.tie_tie_collision_distance_,
                             fabs (edge - last_edge)),
            "tietie edge");
        }

      last_edge = edge;
      last_center = center;
    }

  if (ties->size () > 1)
    {
      ties->add_score (details_.outer_tie_length_symmetry_penalty_factor_
                         * fabs (ties->front ().attachment_x_.length ()
                                 - ties->back ().attachment_x_.length ()),
                       "length symm");

      ties->add_score (
        details_.outer_tie_vertical_distance_symmetry_penalty_factor_
          * fabs (
            fabs (specifications_[0].position_ * 0.5 * details_.staff_space_
                  - (ties->front ().position_ * 0.5 * details_.staff_space_
                     + ties->front ().delta_y_))
            - fabs (specifications_.back ().position_ * 0.5
                      * details_.staff_space_
                    - (ties->back ().position_ * 0.5 * details_.staff_space_
                       + ties->back ().delta_y_))),
        "pos symmetry");
    }
}

/*
  Generate with correct X-attachments and beziers, copying delta_y_
  from TIES_CONFIG if necessary.
*/
Ties_configuration
Tie_formatting_problem::generate_ties_configuration (
  Ties_configuration const &ties_config)
{
  Ties_configuration copy;
  for (vsize i = 0; i < ties_config.size (); i++)
    {
      Tie_configuration *ptr = get_configuration (
        ties_config[i].position_, ties_config[i].dir_,
        ties_config[i].column_ranks_, !specifications_[i].has_manual_delta_y_);
      if (specifications_[i].has_manual_delta_y_)
        {
          ptr->delta_y_
            = (specifications_[i].manual_position_ - ties_config[i].position_)
              * 0.5 * details_.staff_space_;
        }
      copy.push_back (*ptr);
    }

  return copy;
}

Ties_configuration
Tie_formatting_problem::generate_base_chord_configuration ()
{
  Ties_configuration ties_config;
  for (const auto &spec : specifications_)
    {
      Tie_configuration conf;
      if (spec.has_manual_dir_)
        conf.dir_ = spec.manual_dir_;
      if (spec.has_manual_position_)
        {
          conf.position_
            = static_cast<int> (round_halfway_up (spec.manual_position_));
          if (spec.has_manual_delta_y_)
            conf.delta_y_ = (spec.manual_position_ - conf.position_) * 0.5
                            * details_.staff_space_;
        }
      else
        {
          conf.position_ = spec.position_;
        }

      conf.column_ranks_ = spec.column_ranks_;
      ties_config.push_back (conf);
    }

  set_ties_config_standard_directions (&ties_config);
  for (vsize i = 0; i < ties_config.size (); i++)
    if (!specifications_[i].manual_position_)
      ties_config[i].position_ += ties_config[i].dir_;

  ties_config = generate_ties_configuration (ties_config);

  return ties_config;
}

Ties_configuration
Tie_formatting_problem::find_best_variation (
  Ties_configuration const &base,
  vector<Tie_configuration_variation> const &vars)
{
  Ties_configuration best = base;

  /*
    This simply is 1-opt: we have K substitions, and we try applying
    exactly every one for each.
  */
  for (vsize i = 0; i < vars.size (); i++)
    {
      Ties_configuration variant (base);
      for (vsize j = 0; j < vars[i].index_suggestion_pairs_.size (); j++)
        variant[vars[i].index_suggestion_pairs_[j].first]
          = *vars[i].index_suggestion_pairs_[j].second;

      variant.reset_score ();
      score_ties (&variant);

      if (variant.score () < best.score ())
        {
          best = variant;
        }
    }

  return best;
}

Ties_configuration
Tie_formatting_problem::generate_optimal_configuration ()
{
  Ties_configuration base = generate_base_chord_configuration ();
  score_ties (&base);

  vector<Tie_configuration_variation> vars;
  if (specifications_.size () > 1)
    vars = generate_collision_variations (base);
  else
    vars = generate_single_tie_variations (base);

  Ties_configuration best = find_best_variation (base, vars);

  if (specifications_.size () > 1)
    {
      vars = generate_extremal_tie_variations (best);
      best = find_best_variation (best, vars);
    }
  return best;
}

void
Tie_formatting_problem::set_ties_config_standard_directions (
  Ties_configuration *tie_configs)
{
  if (tie_configs->empty ())
    return;

  if (!tie_configs->front ().dir_)
    {
      auto &front = tie_configs->front ();

      if (tie_configs->size () == 1)
        front.dir_ = Direction (sign (front.position_));

      if (!front.dir_)
        front.dir_
          = (tie_configs->size () > 1) ? DOWN : details_.neutral_direction_;
    }

  if (!tie_configs->back ().dir_)
    tie_configs->back ().dir_ = UP;

  /*
    Seconds
   */
  for (vsize i = 1; i < tie_configs->size (); i++)
    {
      Real diff
        = ((*tie_configs)[i].position_ - (*tie_configs)[i - 1].position_);

      Real span_diff = specifications_[i].column_span ()
                       - specifications_[i - 1].column_span ();
      if (span_diff && fabs (diff) <= 2)
        {
          if (span_diff > 0)
            (*tie_configs)[i].dir_ = UP;
          else if (span_diff < 0)
            (*tie_configs)[i - 1].dir_ = DOWN;
        }
      else if (fabs (diff) <= 1)
        {
          if (!(*tie_configs)[i - 1].dir_)
            (*tie_configs)[i - 1].dir_ = DOWN;
          if (!(*tie_configs)[i].dir_)
            (*tie_configs)[i].dir_ = UP;
        }
    }

  for (auto &conf : *tie_configs)
    {
      if (conf.dir_)
        continue;

      Direction position_dir = Direction (sign (conf.position_));
      if (!position_dir)
        position_dir = DOWN;

      conf.dir_ = position_dir;
    }
}

vector<Tie_configuration_variation>
Tie_formatting_problem::generate_extremal_tie_variations (
  Ties_configuration const &ties) const
{
  vector<Tie_configuration_variation> vars;
  for (int i = 1; i <= details_.multi_tie_region_size_; i++)
    {
      Drul_array<Tie_configuration *> configs;
      for (const auto d : {DOWN, UP})
        {
          const Tie_configuration &config = boundary (ties, d, 0);
          if (config.dir_ == d
              && !boundary (specifications_, d, 0).has_manual_position_)
            {
              Tie_configuration_variation var;
              configs[d] = get_configuration (config.position_ + d * i, d,
                                              config.column_ranks_, true);
              var.add_suggestion ((d == DOWN) ? 0 : ties.size () - 1,
                                  configs[d]);
              vars.push_back (var);
            }
        }
      if (configs[LEFT] && configs[RIGHT])
        {
          Tie_configuration_variation var;
          var.add_suggestion (0, configs[DOWN]);
          var.add_suggestion (ties.size () - 1, configs[UP]);
          vars.push_back (var);
        }
    }

  return vars;
}

vector<Tie_configuration_variation>
Tie_formatting_problem::generate_single_tie_variations (
  Ties_configuration const &ties) const
{
  vector<Tie_configuration_variation> vars;

  int sz = details_.single_tie_region_size_;
  if (specifications_[0].has_manual_position_)
    sz = 1;
  for (int i = 0; i < sz; i++)
    {
      for (const auto d : {LEFT, RIGHT})
        {
          if (i == 0 && ties[0].dir_ == d)
            continue;

          int p = ties[0].position_ + i * d;

          if (!specifications_[0].has_manual_dir_
              || d == specifications_[0].manual_dir_)
            {
              Tie_configuration_variation var;
              var.add_suggestion (
                0, get_configuration (p, d, specifications_[0].column_ranks_,
                                      !specifications_[0].has_manual_delta_y_));
              vars.push_back (var);
            }
        }
    }
  return vars;
}

vector<Tie_configuration_variation>
Tie_formatting_problem::generate_collision_variations (
  Ties_configuration const &ties) const
{
  Real center_distance_tolerance = 0.25;

  vector<Tie_configuration_variation> vars;
  Real last_center = 0.0;
  for (vsize i = 0; i < ties.size (); i++)
    {
      Bezier b (ties[i].get_transformed_bezier (details_));

      Real center = b.curve_point (0.5)[Y_AXIS];

      if (i)
        {
          if (center <= last_center + center_distance_tolerance)
            {
              if (!specifications_[i].has_manual_dir_)
                {
                  Tie_configuration_variation var;
                  var.add_suggestion (
                    i, get_configuration (
                         specifications_[i].position_ - ties[i].dir_,
                         -ties[i].dir_,

                         ties[i].column_ranks_,
                         !specifications_[i].has_manual_delta_y_));

                  vars.push_back (var);
                }

              if (!specifications_[i - 1].has_manual_dir_)
                {
                  Tie_configuration_variation var;
                  var.add_suggestion (
                    i - 1,
                    get_configuration (
                      specifications_[i - 1].position_ - ties[i - 1].dir_,
                      -ties[i - 1].dir_, specifications_[i - 1].column_ranks_,
                      !specifications_[i - 1].has_manual_delta_y_));

                  vars.push_back (var);
                }

              if (i == 1 && !specifications_[i - 1].has_manual_position_
                  && ties[i - 1].dir_ == DOWN)
                {
                  Tie_configuration_variation var;
                  var.add_suggestion (
                    i - 1, get_configuration (
                             specifications_[i - 1].position_ - 1, DOWN,
                             specifications_[i - 1].column_ranks_,
                             !specifications_[i - 1].has_manual_delta_y_));
                  vars.push_back (var);
                }
              if (i == ties.size () && !specifications_[i].has_manual_position_
                  && ties[i].dir_ == UP)
                {
                  Tie_configuration_variation var;
                  var.add_suggestion (
                    i, get_configuration (
                         specifications_[i].position_ + 1, UP,
                         specifications_[i].column_ranks_,
                         !specifications_[i].has_manual_delta_y_));
                  vars.push_back (var);
                }
            }
          else if (dot_positions_.find (ties[i].position_)
                     != dot_positions_.end ()
                   && !specifications_[i].has_manual_position_)
            {
              Tie_configuration_variation var;
              var.add_suggestion (
                i, get_configuration (ties[i].position_ + ties[i].dir_,
                                      ties[i].dir_, ties[i].column_ranks_,
                                      !specifications_[i].has_manual_delta_y_));
              vars.push_back (var);
            }
        }

      last_center = center;
    }

  return vars;
}

void
Tie_formatting_problem::set_manual_tie_configuration (SCM manual_configs)
{
  vsize k = 0;
  for (SCM s = manual_configs; scm_is_pair (s) && k < specifications_.size ();
       s = scm_cdr (s))
    {
      SCM entry = scm_car (s);
      if (scm_is_pair (entry))
        {
          Tie_specification &spec = specifications_[k];

          if (scm_is_number (scm_car (entry)))
            {
              spec.has_manual_position_ = true;
              spec.manual_position_ = from_scm<double> (scm_car (entry));
              /* TODO: check whether inexact? is an appropriate condition here */
              spec.has_manual_delta_y_
                = (scm_is_true (scm_inexact_p (scm_car (entry))));
            }

          if (scm_is_number (scm_cdr (entry)))
            {
              spec.has_manual_dir_ = true;
              spec.manual_dir_ = Direction (from_scm<int> (scm_cdr (entry)));
            }
        }
      k++;
    }
}

void
Tie_formatting_problem::set_debug_scoring (Ties_configuration const &base)
{
  if (from_scm<bool> (x_refpoint_->layout ()->lookup_variable (
        ly_symbol2scm ("debug-tie-scoring"))))
    {
      for (vsize i = 0; i < base.size (); i++)
        {
          string card = base.complete_tie_card (i);
          set_property (specifications_[i].tie_grob_, "annotation",
                        ly_string2scm (card));
        }
    }
}
