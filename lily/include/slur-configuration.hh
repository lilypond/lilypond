/*
  slur-configuration.hh -- declare Slur_configuration

  source file of the GNU LilyPond music typesetter

  (c) 2004--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef SLUR_CONFIGURATION_HH
#define SLUR_CONFIGURATION_HH

#include "bezier.hh"
#include "lily-proto.hh"
#include "std-vector.hh"


enum Configuration_tag
  {
    SLUR_STEM = 0x01,
    SLUR_HEAD = 0x02,
    SLUR_FREE = 0x04,
    SLUR_FREE_HEAD = 0x08,
    SLUR_FREE_STEM = 0x10,
    SLUR_STEM_TIP = 0x10,
  };

class Slur_configuration
{
  Real score_;
  string score_card_;

public:
  Drul_array<Offset> attachment_;
  Bezier curve_;
  Real height_;
  unsigned tags_;
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

