/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2008--2022 Han-Wen Nienhuys <hanwen@lilypond.org>

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

#ifndef SKYLINE_PAIR_HH
#define SKYLINE_PAIR_HH

#include "skyline.hh"

#include <vector>

class Skyline_pair
{
private:
  Drul_array<Skyline> skylines_;

public:
  Skyline_pair ();
  Skyline_pair (std::vector<Box> const &boxes, Axis a);
  Skyline_pair (std::vector<Drul_array<Offset>> const &buildings, Axis a);
  Skyline_pair (std::vector<Skyline_pair> const &skypairs);
  Skyline_pair (Box const &, Axis a);
  Skyline_pair (Skyline, Skyline);

  Real left () const;
  Real right () const;
  void raise (Real);
  void shift (Real);
  void pad (Real);
  void merge (Skyline_pair const &other);
  Skyline &operator[] (Direction d);
  Skyline const &operator[] (Direction d) const;
  bool is_empty () const;
  void print () const;
  void print_points () const;

  DECLARE_SCHEME_CALLBACK (skyline, (SCM, SCM));
};

#endif /* SKYLINE_PAIR_HH */
