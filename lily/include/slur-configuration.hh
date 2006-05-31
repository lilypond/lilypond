/*
  slur-configuration.hh -- declare Slur_configuration

  source file of the GNU LilyPond music typesetter

  (c) 2004--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef SLUR_CONFIGURATION_HH
#define SLUR_CONFIGURATION_HH

#include "bezier.hh"
#include "std-vector.hh"
#include "main.hh"

class Slur_configuration
{
  Real score_;
  string score_card_;

public:
  Drul_array<Offset> attachment_;
  Bezier curve_;
  Real height_;

  int index_;

  Slur_configuration ();

  Real score () const { return score_; }
  string card () const { return score_card_; } 
  void add_score (Real, string);
  
  void generate_curve (Slur_score_state const &state, Real r0, Real h_inf,
		       vector<Offset> const &);
  void calculate_score (Slur_score_state const &);
protected:
  void score_extra_encompass (Slur_score_state const &);
  void score_slopes (Slur_score_state const &);
  void score_edges (Slur_score_state const &);
  void score_encompass (Slur_score_state const &);
};

#endif /* SLUR_CONFIGURATION_HH */

