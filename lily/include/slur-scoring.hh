/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef SLUR_SCORING_HH
#define SLUR_SCORING_HH

#include "box.hh"
#include "lily-guile.hh"
#include "slur-score-parameters.hh"

#include <memory>
#include <vector>

/* potential collisions with non-notes.
 */
struct Extra_collision_info
{
  // The place within X extents that we care about here, from -1..1
  Real idx_;
  Box extents_;

  // configurable penalty, so accidentals can be treated specially.
  Real penalty_;

  // the grob for the collision
  Grob *grob_;

  // How collisions should be resolved. See #'avoid-slur grob property.
  SCM type_;

  Extra_collision_info (Grob *g, Real idx, Interval x, Interval y, Real p);
  Extra_collision_info ();
};

/* The notes that the slur covers. This is the note head most close to
   the slur, and possibly a stem. */
struct Encompass_info
{
  Real x_;
  Real stem_;
  Real head_;
  Encompass_info ()
  {
    x_ = 0.0;
    stem_ = 0.0;
    head_ = 0.0;
  }
  Real get_point (Direction dir) const
  {
    Interval y;
    y.add_point (stem_);
    y.add_point (head_);
    return y[dir];
  }
};

/* The objects that are at the begin and end of a slur */
struct Bound_info
{
  Box stem_extent_;
  Direction stem_dir_;
  Item *bound_;
  Grob *note_column_;
  Grob *slur_head_;
  Grob *staff_;
  Grob *stem_;
  Grob *flag_;
  Interval slur_head_x_extent_;

  Bound_info ()
  {
    stem_ = 0;
    staff_ = 0;
    slur_head_ = 0;
    stem_dir_ = CENTER;
    note_column_ = 0;
  }
};

/* Input data and scoring state for a single slur formatting problem. */
class Slur_score_state
{
public:
  Spanner *slur_;
  Grob *common_[NO_AXES];
  bool valid_;
  bool edge_has_beams_;
  bool is_broken_;
  bool has_same_beam_;

  Real musical_dy_;
  std::vector<Grob *> note_columns_;
  std::vector<Encompass_info> encompass_infos_;
  std::vector<Extra_collision_info> extra_encompass_infos_;

  Direction dir_;
  Slur_score_parameters parameters_;
  Drul_array<Bound_info> extremes_;
  Drul_array<Offset> base_attachments_;
  std::vector<std::unique_ptr<Slur_configuration>> configurations_;
  Real staff_space_;
  Real line_thickness_;
  Real thickness_;

  Slur_score_state ();
  ~Slur_score_state ();

  Slur_configuration *get_forced_configuration (Interval ys) const;
  Slur_configuration *get_best_curve () const;
  void fill (Spanner *);
  Direction slur_direction () const;

  std::vector<Offset> generate_avoid_offsets () const;
  Drul_array<Bound_info> get_bound_info () const;
  void generate_curves () const;
  std::vector<std::unique_ptr<Slur_configuration>>
  enumerate_attachments (Drul_array<Real> end_ys) const;
  Drul_array<Offset> get_base_attachments () const;
  Drul_array<Real> get_y_attachment_range () const;
  Encompass_info get_encompass_info (Grob *col) const;
  std::vector<Extra_collision_info> get_extra_encompass_infos () const;
  Real move_away_from_staffline (Real y, Grob *on_staff) const;

  Interval breakable_bound_extent (Direction) const;
};

void set_slur_control_points (Grob *me);

#endif /* SLUR_SCORING_HH */
