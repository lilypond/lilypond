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

#ifndef BEAM_HH
#define BEAM_HH

#include "grob-interface.hh"
#include "lily-proto.hh"
#include "stem-info.hh"

#include <vector>

struct Beam_segment
{
  int vertical_count_;
  Interval horizontal_;
  Beam_segment ();
};

struct Beam_stem_end
{
  Real stem_y_;
  Real french_beaming_stem_adjustment_;
  Beam_stem_end ();
};

bool beam_segment_less (Beam_segment const &a, Beam_segment const &b);

struct Beam_stem_segment
{
  Beam_stem_segment ();

  Grob *stem_;
  Real width_;
  Real stem_x_;
  vsize rank_;
  vsize stem_index_;
  bool gapped_;
  Direction dir_;
  int max_connect_;
};

bool operator<(Beam_stem_segment const &a, Beam_stem_segment const &b);

class Beam
{
public:
  static vsize normal_stem_count (Grob *);
  static Grob *first_normal_stem (Grob *);
  static Grob *last_normal_stem (Grob *);
  static void add_stem (Grob *, Grob *);
  static bool is_cross_staff (Grob *);
  static bool is_knee (Grob *);
  static void set_beaming (Grob *, Beaming_pattern const *);
  static void set_stemlens (Grob *);
  static int get_beam_count (Grob *me);
  static Real get_beam_translation (Grob *me);
  static Real get_beam_thickness (Grob *me);
  static void connect_beams (Grob *me);
  static std::vector<Beam_segment> get_beam_segments (Grob *me_grob,
                                                      Grob **common);

  DECLARE_SCHEME_CALLBACK (rest_collision_callback,
                           (SCM element, SCM prev_off));
  DECLARE_SCHEME_CALLBACK (pure_rest_collision_callback,
                           (SCM element, SCM, SCM, SCM prev_off));
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_beaming, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_stem_shorten, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_direction, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_normal_stems, (SCM));
  DECLARE_SCHEME_CALLBACK (set_stem_lengths, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_cross_staff, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_beam_segments, (SCM));
  /* position callbacks */
  DECLARE_SCHEME_CALLBACK (quanting, (SCM, SCM, SCM));
  DECLARE_SCHEME_CALLBACK (calc_x_positions, (SCM));

  static int get_direction_beam_count (Grob *me, Direction d);

private:
  friend class Beam_scoring_problem;

  static Direction get_default_dir (Grob *);
  static std::vector<Beam_segment> get_beam_segments (Grob *);
  static void set_stem_directions (Grob *, Direction);
  static void consider_auto_knees (Grob *);
  static void set_stem_shorten (Grob *);
  static vsize forced_stem_count (Grob *);
  static Beam_stem_end calc_stem_y (Grob *, Grob *s, Grob **c, Real, Real,
                                    Direction, Interval pos, int french_count);
};

#endif /* BEAM_HH */
