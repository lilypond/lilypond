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

#ifndef INTERVAL_SET_HH
#define INTERVAL_SET_HH

#include "interval.hh"

#include <vector>

class Interval_set
{
public:
  Interval_set ();

  static Interval_set interval_union (std::vector<Interval>);

  std::vector<Interval> const &intervals () const { return intervals_; }
  std::vector<Interval>::const_iterator upper_bound (Real x) const;
  Real nearest_point (Real x, Direction dir = CENTER) const;
  Interval_set complement () const;

private:
  std::vector<Interval> intervals_;
};

#endif /* INTERVAL_SET_HH */
