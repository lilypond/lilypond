/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef SIMPLE_SPACER_HH
#define SIMPLE_SPACER_HH

#include "lily-proto.hh"
#include "spring.hh"
#include "smobs.hh"

#include <vector>

class Simple_spacer : public Simple_smob<Simple_spacer>
{
public:
  Simple_spacer ();
  /* construct */
  void add_rod (vsize l, vsize r, Real dist);
  void add_spring (Spring const &spring);

  /* solve */
  struct Solution {
    Real force_;
    bool fits_;
  };
  Solution solve (Real line_len, bool ragged) const;
  std::vector<Real> spring_positions (Real force, bool ragged) const;
  Real force_penalty (Real line_len, Real force, bool ragged) const;
  Real configuration_length (Real force) const;

private:
  Real range_len (vsize l, vsize r, Real force) const;
  Real range_ideal_len (vsize l, vsize r) const;
  Real range_stiffness (vsize l, vsize r, bool stretch) const;
  Solution expand_line (Real line_len) const;
  Solution compress_line (Real line_len) const;
  Real heuristic_rod_force (vsize l, vsize r, Real dist) const;

  std::vector<Spring> springs_;

  // minimum positive force such that all springs are free.
  Real min_force_;
};

/* returns a vector of dimensions breaks.size () * breaks.size () */
std::vector<Real> get_line_forces (std::vector<Paper_column *> const &columns,
                                   Real line_len,
                                   Real indent,
                                   bool ragged);

Column_x_positions get_line_configuration (std::vector<Paper_column *> const &columns,
                                           Real line_len,
                                           Real indent,
                                           bool ragged);

#endif /* SIMPLE_SPACER_HH */
