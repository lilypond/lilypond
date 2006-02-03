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
public:
  Drul_array<Offset> attachment_;
  Real score_;
  Bezier curve_;
  Real height_;

  int index_;

#if DEBUG_SLUR_SCORING
  std::string score_card_;
#endif

  Slur_configuration ();

  void generate_curve (Slur_score_state const &state, Real r0, Real h_inf,
		       std::vector<Offset> const &);
  void score (Slur_score_state const &);
protected:
  void score_extra_encompass (Slur_score_state const &);
  void score_slopes (Slur_score_state const &);
  void score_edges (Slur_score_state const &);
  void score_encompass (Slur_score_state const &);
};

#endif /* SLUR_CONFIGURATION_HH */

