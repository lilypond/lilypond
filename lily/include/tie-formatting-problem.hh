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

#ifndef TIE_FORMATTING_PROBLEM_HH
#define TIE_FORMATTING_PROBLEM_HH

#include "direction.hh"
#include "drul-array.hh"
#include "skyline.hh"
#include "tie-configuration.hh"
#include "tie-details.hh"
#include "tie-specification.hh"

#include <map>
#include <memory>
#include <set>
#include <tuple>
#include <vector>

using Tie_configuration_map_key = std::tuple<int, Direction, int, int>;
using Tie_configuration_map
  = std::map<Tie_configuration_map_key, std::unique_ptr<Tie_configuration>>;

struct Tie_configuration_variation
{
  std::vector<std::pair<vsize, Tie_configuration *>> index_suggestion_pairs_;
  void add_suggestion (vsize index, Tie_configuration *suggestion)
  {
    index_suggestion_pairs_.push_back (std::make_pair (index, suggestion));
  }
};

typedef std::tuple<int, Direction> Tie_rank_and_dir;
typedef std::map<Tie_rank_and_dir, Skyline> Chord_outline_map;
typedef std::map<Tie_rank_and_dir, Box> Column_extent_map;
typedef std::map<int, Slice> Position_extent_map;

class Tie_formatting_problem
{
  Chord_outline_map chord_outlines_;
  Column_extent_map stem_extents_;
  Column_extent_map head_extents_;
  Position_extent_map head_positions_;

  std::set<int> dot_positions_;
  Interval dot_x_;
  std::vector<Tie_specification> specifications_;
  bool use_horizontal_spacing_;

  Tie_configuration_map possibilities_;

  Grob *x_refpoint_;
  Grob *y_refpoint_;

  Tie_configuration *get_configuration (int position, Direction dir,
                                        Drul_array<int> cols,
                                        bool tune_y) const;
  std::unique_ptr<Tie_configuration>
  generate_configuration (int position, Direction dir, Drul_array<int> cols,
                          bool tune_y) const;

  std::vector<Tie_configuration_variation>
  generate_collision_variations (Ties_configuration const &ties) const;
  std::vector<Tie_configuration_variation>
  generate_extremal_tie_variations (Ties_configuration const &ties) const;
  std::vector<Tie_configuration_variation>
  generate_single_tie_variations (Ties_configuration const &ties) const;

  void score_configuration (Tie_configuration *) const;
  Real score_aptitude (Tie_configuration *, Tie_specification const &,
                       Ties_configuration *, vsize) const;
  void score_ties_aptitude (Ties_configuration *ties) const;
  void score_ties_configuration (Ties_configuration *ties) const;
  void
  set_ties_config_standard_directions (Ties_configuration *tie_configs_ptr);
  void score_ties (Ties_configuration *) const;

  Slice head_positions_slice (int) const;
  Ties_configuration generate_base_chord_configuration ();
  Ties_configuration
  find_best_variation (Ties_configuration const &base,
                       std::vector<Tie_configuration_variation> const &vars);

public:
  Tie_details details_;
  void print_ties_configuration (Ties_configuration const *);

  Interval get_stem_extent (int, Direction, Axis) const;
  Interval get_head_extent (int, Direction, Axis) const;

public:
  Tie_formatting_problem ();
  ~Tie_formatting_problem () = default;

  Tie_specification get_tie_specification (int) const;
  Ties_configuration generate_optimal_configuration ();
  Ties_configuration generate_ties_configuration (Ties_configuration const &);

  void from_ties (std::vector<Grob *> const &ties);
  void from_tie (Grob *tie);
  void from_semi_ties (std::vector<Grob *> const &, Direction head_dir);
  void set_chord_outline (std::vector<Item *>, Direction);
  void set_column_chord_outline (std::vector<Item *>, Direction, int rank);
  void set_manual_tie_configuration (SCM);
  Interval get_attachment (Real, Drul_array<int>) const;
  Grob *common_x_refpoint () const;
  void set_debug_scoring (Ties_configuration const &);
};

#endif /* TIE_FORMATTING_PROBLEM_HH */
