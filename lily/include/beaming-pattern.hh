/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2023 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "context.hh"
#include "direction.hh"
#include "drul-array.hh"
#include "duration.hh"
#include "lily-guile.hh"
#include "lily-proto.hh"
#include "moment.hh"
#include "rational.hh"
#include "tuplet-description.hh"

#include <memory>
#include <vector>

struct Beaming_options
{
  bool subdivide_beams_;
  bool strict_beat_beaming_;
  bool respect_incomplete_beams_;

  SCM beat_structure_;
  Rational base_moment_;
  Rational measure_length_;

  SCM time_signature_;

  Rational minimum_subdivision_interval_;
  Rational maximum_subdivision_interval_;

  void from_context (Context const *);

  void gc_mark () const;
};

/*
  Generate beaming given durations of notes. Beam uses this to
  set_beaming () for each of its stems.
*/
class Beaming_pattern
{
public:
  // measure_offset_ specifies the measure position of the first stem
  // and must be nonnegative
  Rational const measure_offset_;

  void beamify (Beaming_options const &);
  void add_stem (Rational const &, bool, Duration const &,
                 Tuplet_description const *);
  Beaming_pattern *split_pattern (vsize, Rational const &);
  unsigned beamlet_count (vsize, Direction) const;
  Rational const &start_moment (vsize) const;
  Rational end_moment (vsize) const;

  Beaming_pattern (Rational const &);

private:
  struct Beam_rhythmic_element;
  std::vector<Beam_rhythmic_element> infos_;

  void unbeam_invisible_stems ();
  void set_rhythmic_importance (Beaming_options const &);
  void subdivide_beams (Beaming_options const &);
  bool at_span_start (vsize) const;
  bool at_span_stop (vsize) const;
};

struct Beaming_pattern::Beam_rhythmic_element
{
  Rational const start_moment_;
  unsigned beam_count_;
  Drul_array<unsigned>
    beam_count_drul_; // stores beam count of left-right neighboring stems

  int rhythmic_importance_;
  bool const invisible_; // rests are "invisibile"

  Duration const duration_;
  // if not under a tuplet, then nullptr
  Tuplet_description const *const tuplet_;

  Beam_rhythmic_element (Rational const &, bool, Duration const &,
                         Tuplet_description const *);
  unsigned count () const;
};

#endif /* BEAMING_PATTERN_HH */
