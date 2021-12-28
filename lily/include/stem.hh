/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2022 Han-Wen Nienhuys

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

#ifndef STEM_HH
#define STEM_HH

#include "lily-proto.hh"
#include "stem-info.hh"
#include "grob-interface.hh"

#include <vector>

class Stem
{
public:
  static std::vector<int> note_head_positions (Grob *, bool filter = false);
  static int duration_log (Grob *);
  static void set_beaming (Grob *, int, Direction d);
  static int get_beaming (Grob *, Direction d);
  static Spanner *get_beam (Grob *);
  static Grob *first_head (Grob *);
  static Grob *last_head (Grob *);
  static Drul_array<Grob *> extremal_heads (Grob *);
  static Grob *support_head (Grob *);
  static void add_head (Grob *me, Grob *n);
  static Stem_info get_stem_info (Grob *);
  static Real chord_start_y (Grob *);
  static void set_stem_positions (Grob *, Real, Real);
  static void cache_pure_height (Grob *, Interval, Interval);
  static Slice beam_multiplicity (Grob *);
  static Direction get_default_dir (Grob *);
  static Real thickness (Grob *);
  static Real beam_end_corrective (Grob *);
  static vsize head_count (Grob *);
  static bool is_invisible (Grob *);
  static bool is_normal_stem (Grob *);
  static bool is_cross_staff (Grob *);
  static Interval head_positions (Grob *);
  static Interval internal_pure_height (Grob *, bool);
  static Interval internal_height (Grob *, bool);
  static bool is_valid_stem (Grob *);
  static Grob *get_reference_head (Grob *);
  static Real internal_calc_stem_end_position (Grob *, bool);
  static Real internal_calc_stem_begin_position (Grob *, bool);

  static void set_spacing_hints (Grob *);
  static Grob *flag (Grob *);

  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_default_direction, (SCM));
  DECLARE_SCHEME_CALLBACK (offset_callback, (SCM element));
  DECLARE_SCHEME_CALLBACK (calc_direction, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_length, (SCM));
  DECLARE_SCHEME_CALLBACK (pure_calc_length, (SCM, SCM, SCM));
  DECLARE_SCHEME_CALLBACK (calc_stem_begin_position, (SCM));
  DECLARE_SCHEME_CALLBACK (pure_calc_stem_begin_position, (SCM, SCM, SCM));
  DECLARE_SCHEME_CALLBACK (calc_stem_end_position, (SCM));
  DECLARE_SCHEME_CALLBACK (pure_calc_stem_end_position, (SCM, SCM, SCM));
  DECLARE_SCHEME_CALLBACK (calc_stem_info, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_positioning_done, (SCM));
  DECLARE_SCHEME_CALLBACK (width, (SCM smob));
  DECLARE_SCHEME_CALLBACK (pure_height, (SCM, SCM, SCM));
  DECLARE_SCHEME_CALLBACK (height, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_cross_staff, (SCM));
};
#endif
