/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef SLUR_CONFIGURATION_HH
#define SLUR_CONFIGURATION_HH

#include "bezier.hh"
#include "lily-proto.hh"

#include <memory>
#include <vector>

/* A candidate position for a slur. */
class Slur_configuration
{
  Real score_;
  std::string score_card_;

public:
  Drul_array<Offset> attachment_;
  Bezier curve_;
  Real height_;
  size_t index_;

  /* The different scoring functions we have, ordered by increasing
     computational cost */
  enum Slur_scorers
  {
    INITIAL_SCORE,
    SLOPE,
    EDGES,
    EXTRA_ENCOMPASS,
    ENCOMPASS,
    NUM_SCORERS,
  };

  int next_scorer_todo;

  Slur_configuration ();

  Real score () const { return score_; }
  std::string card () const { return score_card_; }
  void add_score (Real, const std::string &);

  void generate_curve (Slur_score_state const &state, Real r0, Real h_inf,
                       std::vector<Offset> const &);
  void run_next_scorer (Slur_score_state const &);
  bool done () const;
  static std::unique_ptr<Slur_configuration>
  new_config (Drul_array<Offset> const &offs, size_t idx);

protected:
  void score_extra_encompass (Slur_score_state const &);
  void score_slopes (Slur_score_state const &);
  void score_edges (Slur_score_state const &);
  void score_encompass (Slur_score_state const &);

  friend class Slur_configuration_less;
};

// Comparator for a queue of Beam_configuration*.
class Slur_configuration_less
{
public:
  bool operator() (Slur_configuration *const &l, Slur_configuration *const &r)
  {
    // Invert
    return l->score_ > r->score_;
  }
};

#endif /* SLUR_CONFIGURATION_HH */
