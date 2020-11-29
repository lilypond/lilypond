/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

  void solve (Real line_len, bool ragged);
  void add_rod (vsize l, vsize r, Real dist);
  void add_spring (Spring const &);
  Real range_len (vsize l, vsize r, Real force) const;
  Real range_ideal_len (vsize l, vsize r) const;
  Real range_stiffness (vsize l, vsize r, bool stretch) const;
  Real configuration_length (Real) const;
  std::vector<Real> spring_positions () const;

  void set_force (Real force);
  Real force () const;
  Real force_penalty (bool ragged) const;
  bool fits () const;

private:
  Real expand_line ();
  Real compress_line ();
  Real heuristic_rod_force (vsize l, vsize r, Real dist);

  std::vector<Spring> springs_;
  Real line_len_;
  Real force_;
  bool ragged_;
  bool fits_;
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
