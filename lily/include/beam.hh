/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

#ifndef BEAM_HH
#define BEAM_HH

#include "grob-interface.hh"
#include "std-vector.hh"
#include "lily-proto.hh"
#include "stem-info.hh"

/*
  TODO: move quanting in separate file.
*/
struct Beam_quant_parameters
{
  Real SECONDARY_BEAM_DEMERIT;
  Real STEM_LENGTH_DEMERIT_FACTOR;
  Real REGION_SIZE;

  /*
    threshold to combat rounding errors.
  */
  Real BEAM_EPS;

  // possibly ridiculous, but too short stems just won't do
  Real STEM_LENGTH_LIMIT_PENALTY;
  Real DAMPING_DIRECTION_PENALTY;
  Real MUSICAL_DIRECTION_FACTOR;
  Real HINT_DIRECTION_PENALTY;
  Real IDEAL_SLOPE_FACTOR;
  Real ROUND_TO_ZERO_SLOPE;

  void fill (Grob *him);
};

struct Beam_segment
{
  int vertical_count_;
  Interval horizontal_; 
  Beam_segment ();
};

struct Beam_stem_segment 
{
  Beam_stem_segment ();

  Grob *stem_;
  Real width_;
  Real stem_x_;
  int rank_;
  vsize stem_index_;
  bool gapped_;
  Direction dir_;
  int max_connect_;
  
};


bool operator <(Beam_stem_segment const &a, Beam_stem_segment const &b);

class Beam
{
public:
  static int normal_stem_count (Grob *);
  static Grob *first_normal_stem (Grob *);
  static Grob *last_normal_stem (Grob *);
  DECLARE_GROB_INTERFACE();
  static void add_stem (Grob *, Grob *);
  static bool is_cross_staff (Grob *);
  static bool is_knee (Grob *);
  static void set_beaming (Grob *, Beaming_pattern const *);
  static void set_stemlens (Grob *);
  static int get_beam_count (Grob *me);
  static Real get_beam_translation (Grob *me);
  static Real get_beam_thickness (Grob *me);
  static void connect_beams (Grob *me);
  static vector<Beam_segment> get_beam_segments (Grob *me_grob, Grob **common); 
  static Interval no_visible_stem_positions (Grob *me, Interval default_value);
  
  DECLARE_SCHEME_CALLBACK (rest_collision_callback, (SCM element, SCM prev_off));
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_beaming, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_stem_shorten, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_direction, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_positions, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_least_squares_positions, (SCM, SCM));
  DECLARE_SCHEME_CALLBACK (calc_normal_stems, (SCM));  
  DECLARE_SCHEME_CALLBACK (calc_concaveness, (SCM));
  DECLARE_SCHEME_CALLBACK (set_stem_lengths, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_cross_staff, (SCM));

  /* position callbacks */
  DECLARE_SCHEME_CALLBACK (shift_region_to_valid, (SCM, SCM));
  DECLARE_SCHEME_CALLBACK (slope_damping, (SCM, SCM));
  DECLARE_SCHEME_CALLBACK (quanting, (SCM, SCM));
  
static Real score_slopes_dy (Real, Real, Real, Real, Real, bool, Beam_quant_parameters const *);

  static Real score_stem_lengths (vector<Grob*> const &stems,
				  vector<Stem_info> const &stem_infos,
				  vector<Real> const &base_stem_ys,
				  vector<Real> const &stem_xs,
				  Real xl, Real xr,
				  bool knee,
				  Real yl, Real yr, Beam_quant_parameters const *);
  static Real score_forbidden_quants (Real, Real,
				      Real, Real, Real, Real,
				      Drul_array<int>, Direction, Direction,
				      Beam_quant_parameters const *);

  static int get_direction_beam_count (Grob *me, Direction d);
private:
  static Direction get_default_dir (Grob *);
  static void set_stem_directions (Grob *, Direction);
  static void consider_auto_knees (Grob *);
  static void set_stem_shorten (Grob *);
  static Real calc_stem_y (Grob *, Grob *s, Grob **c,
			   Real, Real, Direction,
			   Drul_array<Real> pos, bool french);
  static int forced_stem_count (Grob *);
};


#endif /* BEAM_HH */

