/*
  tie-formatting-problem.hh -- declare

  source file of the GNU LilyPond music typesetter

  (c) 2005 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#ifndef TIE_FORMATTING_PROBLEM_HH
#define TIE_FORMATTING_PROBLEM_HH

#include "drul-array.hh"
#include "parray.hh"
#include "skyline.hh"
#include "lily-proto.hh"

#include <map>
#include <set>

struct Tie_details
{
  Real height_limit_;
  Real ratio_;
  Real staff_space_;
  Real x_gap_;
  Real between_length_limit_;
  Grob *staff_symbol_referencer_;
  
  Tie_details ();
  void from_grob (Grob *);
};


typedef map< pair<int, int>, Tie_configuration *> Tie_configuration_map;

class Tie_formatting_problem
{
  Drul_array< Array<Skyline_entry> > chord_outlines_;
  set<int> dot_positions_;
  Tie_configuration_map possibilities_;

  Tie_configuration *get_configuration (int position, Direction dir);
  Tie_configuration *generate_configuration (int position, Direction dir);
  Real score_configuration (Tie_configuration const&);
  Real score_aptitude (Tie_configuration const&, int);
  
  Grob *x_refpoint_;

  
public:
  Tie_details details_;

public:
  Tie_formatting_problem ();
  ~Tie_formatting_problem ();

  Tie_configuration find_optimal_tie_configuration (int p, Direction d);
  void from_ties (Link_array<Grob> const &ties);
  void from_tie (Grob *tie);
  void from_lv_ties (Link_array<Grob> const &);
  void set_chord_outline (Link_array<Item>, Direction);

  Interval get_attachment (Real) const;
  Grob *common_x_refpoint () const;
};

#endif /* TIE_FORMATTING_PROBLEM_HH */
