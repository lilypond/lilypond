/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "std-vector.hh"
#include "lily-proto.hh"

class Tie_configuration
{
  std::string score_card_;
  Real score_;
  bool scored_;
  friend class Tie_formatting_problem;

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
  Tie_configuration ();
  void center_tie_vertically (Tie_details const &);
  Bezier get_transformed_bezier (Tie_details const &) const;
  Bezier get_untransformed_bezier (Tie_details const &) const;
  Real height (Tie_details const &) const;
  int column_span_length () const;

  static int compare (Tie_configuration const &a,
                      Tie_configuration const &b);
  static Real distance (Tie_configuration const &a,
                        Tie_configuration const &b);
};

INSTANTIATE_COMPARE (Tie_configuration, Tie_configuration::compare);

// TODO: Avoid public inheritance from STL containers because they don't have
// virtual destructors, which can lead to bugs if they are not used carefully.
class Ties_configuration : public std::vector<Tie_configuration>
{
  Real score_;
  std::string score_card_;
  bool scored_;
  std::vector<std::string> tie_score_cards_;

  friend class Tie_formatting_problem;
public:
  Ties_configuration ();
  void add_score (Real amount, const std::string &description);
  void add_tie_score (Real amount, vsize i, const std::string &description);
  Real score () const;
  void reset_score ();
  std::string card () const;
  std::string tie_card (vsize i) const { return tie_score_cards_[i]; }
  std::string complete_tie_card (vsize i) const;
  std::string complete_score_card () const;
};

#endif /* TIE_CONFIGURATION_HH */

