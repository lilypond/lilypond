/*
  tie-configuration.hh -- declare Tie_configuration

  source file of the GNU LilyPond music typesetter

  (c) 2005--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#ifndef TIE_CONFIGURATION_HH
#define TIE_CONFIGURATION_HH

#include "direction.hh"
#include "interval.hh"
#include "std-vector.hh"
#include "main.hh" // DEBUG_SLUR_SCORING

class Tie_configuration
{
#if DEBUG_SLUR_SCORING
  string score_card_;
#endif
  Real score_;
  bool scored_;
  friend class Tie_formatting_problem;

public:
  Real score () const { return score_; }
  string card () const { return score_card_; }

  int position_;
  Direction dir_;
  Real delta_y_;
  Drul_array<int> column_ranks_;
  
  /* computed. */
  Interval attachment_x_;

  void add_score (Real, string);
  Tie_configuration ();
  void center_tie_vertically (Tie_details const &);
  Bezier get_transformed_bezier (Tie_details const &) const;
  Bezier get_untransformed_bezier (Tie_details const &) const;
  Real height (Tie_details const&) const;
  int column_span_length () const;
  
  static int compare (Tie_configuration const &a,
		      Tie_configuration const &b);
  static Real distance (Tie_configuration const &a,
		       Tie_configuration const &b);
};

INSTANTIATE_COMPARE (Tie_configuration, Tie_configuration::compare);

class Ties_configuration : public vector<Tie_configuration>
{
  Real score_;
  string score_card_;
  bool scored_;
  vector<string> tie_score_cards_;

  friend class Tie_formatting_problem;
public:
  Ties_configuration ();
  void add_score (Real amount, string description);
  void add_tie_score (Real amount, int i, string description);
  Real score () const;
  void reset_score ();
  string card () const; 
  string tie_card (int i) const { return tie_score_cards_[i]; }
  string complete_tie_card (vsize i) const;
  string complete_score_card () const; 
};

#endif /* TIE_CONFIGURATION_HH */



