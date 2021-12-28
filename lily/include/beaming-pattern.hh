/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef BEAMING_PATTERN_HH
#define BEAMING_PATTERN_HH

#include "direction.hh"
#include "drul-array.hh"
#include "lily-guile.hh"
#include "lily-proto.hh"
#include "rational.hh"

#include <vector>

struct Beaming_options
{
  SCM grouping_;
  bool subdivide_beams_;
  bool strict_beat_beaming_;
  Rational base_moment_;
  Rational measure_length_;

  Beaming_options ();
  void from_context (Context *);
  void gc_mark () const;
};

struct Beam_rhythmic_element
{
  Rational start_moment_;
  Drul_array<int> beam_count_drul_;

  int rhythmic_importance_;
  bool invisible_;

  Rational factor_;

  bool tuplet_start_;

  Beam_rhythmic_element (Rational, int, bool, Rational, bool);
  Beam_rhythmic_element ();

  int count (Direction d) const;
};

/*
  Generate beaming given durations of notes. Beam uses this to
  set_beaming () for each of its stems.
*/
class Beaming_pattern
{
public:
  Beaming_pattern ();

  void beamify (Beaming_options const &);
  void add_stem (Rational d, int beams, bool invisible, Rational factor,
                 bool tuplet_start);
  int beamlet_count (vsize idx, Direction d) const;
  bool invisibility (vsize idx) const;
  Rational factor (vsize idx) const;
  bool tuplet_start (vsize idx) const;
  Rational start_moment (vsize idx) const;
  Rational end_moment (vsize idx) const;
  Beaming_pattern *split_pattern (vsize idx);

private:
  std::vector<Beam_rhythmic_element> infos_;
  Direction flag_direction (Beaming_options const &, vsize) const;
  void find_rhythmic_importance (Beaming_options const &);
  void unbeam_invisible_stems ();
  Rational remaining_length (vsize idx) const;
  int beam_count_for_rhythmic_position (vsize idx) const;
  int beam_count_for_length (Rational len) const;
};

#endif /* BEAMING_PATTERN_HH */
