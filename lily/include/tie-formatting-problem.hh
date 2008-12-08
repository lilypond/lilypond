/*
  tie-formatting-problem.hh -- declare Tie_formatting_problem

  source file of the GNU LilyPond music typesetter

  (c) 2005--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#ifndef TIE_FORMATTING_PROBLEM_HH
#define TIE_FORMATTING_PROBLEM_HH

#include "drul-array.hh"
#include "skyline.hh"
#include "tie-configuration.hh"
#include "tie-details.hh"
#include "tie-specification.hh"
#include "tuple.hh"

#include <map>
#include <set>

typedef map< Tuple<int,4>, Tie_configuration *> Tie_configuration_map;

struct Tie_configuration_variation
{
  vector<pair<int, Tie_configuration *> > index_suggestion_pairs_;
  void add_suggestion(int index, Tie_configuration* suggestion)
  {
    index_suggestion_pairs_.push_back (make_pair (index, suggestion));
  }
};

typedef map < Tuple<int, 2>, Skyline> Chord_outline_map;
typedef map < Tuple<int, 2>, Box> Column_extent_map;
typedef map <int, Slice> Position_extent_map;

class Tie_formatting_problem
{
  Chord_outline_map chord_outlines_;
  Column_extent_map stem_extents_;
  Column_extent_map head_extents_;
  Position_extent_map head_positions_;
  
  set<int> dot_positions_;
  Interval dot_x_;
  vector<Tie_specification> specifications_;
  bool use_horizontal_spacing_;
  
  Tie_configuration_map possibilities_;

  Grob *x_refpoint_;
  Grob *y_refpoint_;

  
  Tie_configuration *get_configuration (int position, Direction dir, Drul_array<int> cols, bool tune_y) const;
  Tie_configuration *generate_configuration (int position, Direction dir, Drul_array<int> cols, bool tune_y) const;

  vector<Tie_configuration_variation> generate_collision_variations (Ties_configuration const &ties) const;
  vector<Tie_configuration_variation> generate_extremal_tie_variations (Ties_configuration const &ties) const;
  vector<Tie_configuration_variation> generate_single_tie_variations (Ties_configuration const &ties) const;
  
  void score_configuration (Tie_configuration *) const;
  Real score_aptitude (Tie_configuration *, Tie_specification const &,
		       Ties_configuration *, int) const;
  void score_ties_aptitude (Ties_configuration *ties) const;
  void score_ties_configuration (Ties_configuration *ties) const;
  void set_ties_config_standard_directions (Ties_configuration *tie_configs_ptr);
  void score_ties (Ties_configuration *) const;
  
  Slice head_positions_slice (int) const;
  Ties_configuration generate_base_chord_configuration ();
  Ties_configuration find_best_variation (Ties_configuration const &base,
					  vector<Tie_configuration_variation> const &vars);

public:
  Tie_details details_;
  void print_ties_configuration (Ties_configuration const *);

  Interval get_stem_extent (int, Direction, Axis) const; 
  Interval get_head_extent (int, Direction, Axis) const; 
  
public:
  Tie_formatting_problem ();
  ~Tie_formatting_problem ();

  Tie_specification get_tie_specification (int) const;
  Ties_configuration generate_optimal_configuration ();
  Ties_configuration generate_ties_configuration (Ties_configuration const &);

  void from_ties (vector<Grob*> const &ties);
  void from_tie (Grob *tie);
  void from_semi_ties (vector<Grob*> const &, Direction head_dir);
  void set_chord_outline (vector<Item*>, Direction);
  void set_column_chord_outline (vector<Item*>, Direction, int rank);
  void set_manual_tie_configuration (SCM);
  Interval get_attachment (Real, Drul_array<int>) const;
  Grob *common_x_refpoint () const;
  void set_debug_scoring (Ties_configuration const &);
};

#endif /* TIE_FORMATTING_PROBLEM_HH */
