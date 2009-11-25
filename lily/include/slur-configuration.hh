/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

