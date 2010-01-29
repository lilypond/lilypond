/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2008--2010 Han-Wen Nienhuys <hanwen@lilypond.org>

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

class Skyline_pair
{
private:
  Drul_array<Skyline> skylines_;

  DECLARE_SIMPLE_SMOBS(Skyline_pair);
public:
  Skyline_pair ();
  Skyline_pair (vector<Box> const &boxes, Real horizon_padding, Axis a);
  Skyline_pair (Box const &, Real horizon_padding, Axis a);
  void raise (Real);
  void shift (Real);
  void insert (Box const &, Real horizon_padding, Axis);
  void merge (Skyline_pair const &other);
  Skyline &operator [] (Direction d);
  Skyline const &operator [] (Direction d) const;
  bool is_empty () const;
  void print () const;
  void print_points () const;
};

#endif /* SKYLINE_PAIR_HH */
