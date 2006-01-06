/*
  slur-scoring.hh -- declare Slur_score_parameters

  source file of the GNU LilyPond music typesetter

  (c) 2004--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef SLUR_SCORING_HH
#define SLUR_SCORING_HH

#include "box.hh"
#include "lily-proto.hh"
#include "parray.hh"
#include "lily-guile.hh"

struct Slur_score_parameters
{
  int region_size_;
  Real head_encompass_penalty_;
  Real stem_encompass_penalty_;
  Real closeness_factor_;
  Real edge_attraction_factor_;
  Real same_slope_penalty_;
  Real steeper_slope_factor_;
  Real non_horizontal_penalty_;
  Real max_slope_;
  Real max_slope_factor_;
  Real extra_object_collision_penalty_;
  Real accidental_collision_;
  Real free_slur_distance_;
  Real free_head_distance_;
  Real extra_encompass_free_distance_;
  Real absolute_closeness_measure_;
  Real edge_slope_exponent_;
  Real head_slur_distance_max_ratio_;
  Real head_slur_distance_factor_;

  void fill (Grob *him);
};

struct Extra_collision_info
{
  Real idx_;
  Box extents_;
  Real penalty_;
  Grob *grob_;
  SCM type_;
  
  Extra_collision_info (Grob *g, Real idx, Interval x, Interval y, Real p);
  Extra_collision_info ();

};

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

struct Bound_info
{
  Box stem_extent_;
  Direction stem_dir_;
  Item *bound_;
  Grob *note_column_;
  Grob *slur_head_;
  Grob *staff_;
  Grob *stem_;
  Interval slur_head_x_extent_;
  Real staff_space_;

  Bound_info ()
  {
    stem_ = 0;
    staff_ = 0;
    slur_head_ = 0;
    stem_dir_ = CENTER;
    note_column_ = 0;
  }
};

struct Slur_score_state
{
  Spanner *slur_;
  Grob *common_[NO_AXES];
  bool valid_;
  bool edge_has_beams_;
  bool is_broken_;
  bool has_same_beam_;

  Real musical_dy_;
  Link_array<Grob> columns_;
  Array<Encompass_info> encompass_infos_;
  Array<Extra_collision_info> extra_encompass_infos_;

  Direction dir_;
  Slur_score_parameters parameters_;
  Drul_array<Bound_info> extremes_;
  Drul_array<Offset> base_attachments_;
  Link_array<Slur_configuration> configurations_;
  Real staff_space_;
  Real thickness_;

  Slur_score_state ();
  ~Slur_score_state ();

  Bezier get_best_curve ();
  void fill (Grob *);
  void set_next_direction ();

  Array<Offset> Slur_score_state::generate_avoid_offsets () const;
  Drul_array<Bound_info> get_bound_info () const;
  void generate_curves () const;
  Link_array<Slur_configuration> enumerate_attachments (Drul_array<Real> end_ys) const;
  Drul_array<Offset> get_base_attachments () const;
  Drul_array<Real> get_y_attachment_range () const;
  Encompass_info get_encompass_info (Grob *col) const;
  Array<Extra_collision_info> get_extra_encompass_infos () const;
  Real move_away_from_staffline (Real y, Grob *on_staff) const;
  int get_closest_index (SCM inspect_quants) const;
};

void set_slur_control_points (Grob *me);

#endif /* SLUR_SCORING_HH */
