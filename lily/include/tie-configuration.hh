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

#ifndef TIE_CONFIGURATION_HH
#define TIE_CONFIGURATION_HH

#include "direction.hh"
#include "interval.hh"
#include "lily-proto.hh"

#include <vector>

class Tie_configuration
{
  std::string score_card_;
  Real score_;
  bool scored_;

public:
  Real score () const { return score_; }
  std::string card () const { return score_card_; }

  int position_;
  Direction dir_;
  Real delta_y_;
  Drul_array<int> column_ranks_;

  /* computed. */
  Interval attachment_x_;

  void add_score (Real, const std::string &);
  bool is_scored () const { return scored_; }
  void set_scored () { scored_ = true; }
  Tie_configuration ();
  void center_tie_vertically (Tie_details const &);
  Bezier get_transformed_bezier (Tie_details const &) const;
  Bezier get_untransformed_bezier (Tie_details const &) const;
  Real height (Tie_details const &) const;
  int column_span_length () const;

  static Real distance (Tie_configuration const &a, Tie_configuration const &b);
};

class Ties_configuration : private std::vector<Tie_configuration>
{
  Real score_;
  std::string score_card_;
  bool scored_;
  std::vector<std::string> tie_score_cards_;

public:
  Ties_configuration ();
  void add_score (Real amount, const std::string &description);
  void add_tie_score (Real amount, vsize i, const std::string &description);
  bool is_scored () const { return scored_; }
  void set_scored () { scored_ = true; }
  Real score () const;
  void reset_score ();
  std::string card () const;
  std::string tie_card (vsize i) const { return tie_score_cards_[i]; }
  std::string complete_tie_card (vsize i) const;
  std::string complete_score_card () const;

public: // exposed subset of vector interface
  using vector::back;
  using vector::begin;
  using vector::empty;
  using vector::end;
  using vector::front;
  using vector::operator[];
  using vector::push_back;
  using vector::size;
};

#endif /* TIE_CONFIGURATION_HH */
