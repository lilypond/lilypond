/*
  tie-formatting-problem.hh -- declare Tie_formatting_problem

  source file of the GNU LilyPond music typesetter

  (c) 2005--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#ifndef TIE_FORMATTING_PROBLEM_HH
#define TIE_FORMATTING_PROBLEM_HH

#include "drul-array.hh"
#include "std-vector.hh"
#include "skyline.hh"
#include "lily-proto.hh"
#include "tie-configuration.hh"
#include "tie-details.hh"

#include <map>
#include <set>

typedef map< pair<int, int>, Tie_configuration *> Tie_configuration_map;

struct Tie_specification
{
  int position_;
  Drul_array<Grob*> note_head_drul_;
  
  bool has_manual_position_;
  bool has_manual_dir_;
  
  Real manual_position_;
  Direction manual_dir_;
  
  Tie_specification ();
};

struct Tie_configuration_variation
{
  int index_;
  Tie_configuration *suggestion_;
  Tie_configuration_variation ();
};

class Tie_formatting_problem
{
  Drul_array< vector<Skyline_entry> > chord_outlines_;
  Drul_array< Box > stem_extents_;
  Drul_array< Box > head_extents_;
  set<int> dot_positions_;
  Interval dot_x_;
  vector<Tie_specification> specifications_;
  
  Tie_configuration_map possibilities_;

  Grob *x_refpoint_;

  
  Tie_configuration *get_configuration (int position, Direction dir) const;
  Tie_configuration *generate_configuration (int position, Direction dir) const;
  vector<Tie_configuration_variation> generate_collision_variations (Ties_configuration const &ties) const;
  vector<Tie_configuration_variation> generate_extremal_tie_variations (Ties_configuration const &ties) const;

  void score_configuration (Tie_configuration *) const;
  Real score_aptitude (Tie_configuration *, Tie_specification const &,
		       Ties_configuration *, int) const;
  void score_ties_aptitude (Ties_configuration *ties) const;
  void score_ties_configuration (Ties_configuration *ties) const;
  void set_ties_config_standard_directions (Ties_configuration *tie_configs_ptr);
  void score_ties (Ties_configuration *) const;
  
  Ties_configuration generate_base_chord_configuration ();
  Ties_configuration find_best_variation (Ties_configuration const &base,
					  vector<Tie_configuration_variation> vars);

public:
  Tie_details details_;
  void print_ties_configuration (Ties_configuration const *);
public:
  Tie_formatting_problem ();
  ~Tie_formatting_problem ();

  Tie_specification get_tie_specification (int) const;
  Ties_configuration generate_optimal_chord_configuration ();
  Ties_configuration generate_ties_configuration (Ties_configuration const &);
  Tie_configuration find_optimal_tie_configuration (Tie_specification const &) const;
  void from_ties (vector<Grob*> const &ties);
  void from_tie (Grob *tie);
  void from_semi_ties (vector<Grob*> const &, Direction head_dir);
  void set_chord_outline (vector<Item*>, Direction);
  void set_manual_tie_configuration (SCM);
  Interval get_attachment (Real) const;
  Grob *common_x_refpoint () const;
};

#endif /* TIE_FORMATTING_PROBLEM_HH */
