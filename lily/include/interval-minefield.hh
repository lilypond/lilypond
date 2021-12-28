/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2011--2022 Mike Solomon <mike@mikesolomon.org>
  Jan Nieuwenhuizen <janneke@gnu.org>

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

#ifndef INTERVAL_MINEFIELD_HH
#define INTERVAL_MINEFIELD_HH

#include "lily-proto.hh"
#include "interval.hh"

#include <vector>

class Interval_minefield
{
public:
  Interval_minefield (Interval, Real);
  void add_forbidden_interval (Interval forbidden);
  Interval feasible_placements ();
  void solve ();

private:
  std::vector<Interval> forbidden_intervals_;
  Interval feasible_placements_;
  Real bulk_;
};

#endif // INTERVAL_MINEFIELD_HH
