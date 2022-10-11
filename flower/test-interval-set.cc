/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2012--2022 Joe Neeman <joeneeman@gmail.com>

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

#include "interval-set.hh"

#include "yaffut.hh"

using std::vector;

FUNC (interval_set_union)
{
  vector<Interval> ivs;

  // Overlapping intervals.
  ivs.push_back (Interval (-1, 1));
  ivs.push_back (Interval (0, 3));
  ivs.push_back (Interval (1, 2));
  Interval_set result = Interval_set::interval_union (ivs);
  // TODO: Yaffut parameters are (expected, actual); these are reversed.
  EQUAL (result.intervals ().size (), 1u);
  // Compare intervals using to_string, since yaffut doesn't know how to compare intervals.
  EQUAL (result.intervals ()[0].to_string (), Interval (-1, 3).to_string ());

  // Non-overlapping intervals.
  ivs.push_back (Interval (-5, -4));
  result = Interval_set::interval_union (ivs);
  EQUAL (result.intervals ().size (), 2u);
  EQUAL (result.intervals ()[0].to_string (), Interval (-5, -4).to_string ());
  EQUAL (result.intervals ()[1].to_string (), Interval (-1, 3).to_string ());

  // Infinite intervals.
  ivs.push_back (Interval (-infinity_f, -4));
  result = Interval_set::interval_union (ivs);
  EQUAL (result.intervals ().size (), 2u);
  EQUAL (result.intervals ()[0].to_string (),
         Interval (-infinity_f, -4).to_string ());
  EQUAL (result.intervals ()[1].to_string (), Interval (-1, 3).to_string ());

  // Empty intervals.
  ivs.push_back (Interval (infinity_f, -infinity_f));
  result = Interval_set::interval_union (ivs);
  EQUAL (result.intervals ().size (), 2u);
}

FUNC (interval_set_nearest_point)
{
  vector<Interval> ivs;

  ivs.push_back (Interval (-3, -1));
  ivs.push_back (Interval (1, 3));
  Interval_set set = Interval_set::interval_union (ivs);

  // If the point is in the set, direction does not matter.
  EQUAL (set.nearest_point (-2, UP), -2);
  EQUAL (set.nearest_point (-2, DOWN), -2);
  EQUAL (set.nearest_point (-2, CENTER), -2);

  // If the point is not in the set, direction does matter.
  EQUAL (set.nearest_point (-0.5, UP), 1);
  EQUAL (set.nearest_point (-0.5, DOWN), -1);
  EQUAL (set.nearest_point (-0.5, CENTER), -1);
  EQUAL (set.nearest_point (0.5, CENTER), 1);

  // The return value can be +- infinity.
  EQUAL (set.nearest_point (5, UP), infinity_f);
  EQUAL (set.nearest_point (5, DOWN), 3);
  EQUAL (set.nearest_point (-5, DOWN), -infinity_f);
  EQUAL (set.nearest_point (-5, UP), -3);
}

FUNC (interval_set_complement)
{
  vector<Interval> ivs;

  ivs.push_back (Interval (-3, -1));
  ivs.push_back (Interval (1, 3));
  Interval_set set = Interval_set::interval_union (ivs).complement ();
  EQUAL (set.intervals ().size (), 3u);
  EQUAL (set.intervals ()[0].to_string (),
         Interval (-infinity_f, -3).to_string ());
  EQUAL (set.intervals ()[1].to_string (), Interval (-1, 1).to_string ());
  EQUAL (set.intervals ()[2].to_string (),
         Interval (3, infinity_f).to_string ());

  // Half-infinite sets are handled correctly.
  ivs.push_back (Interval (-infinity_f, -2));
  set = Interval_set::interval_union (ivs).complement ();
  EQUAL (set.intervals ().size (), 2u);
  EQUAL (set.intervals ()[0].to_string (), Interval (-1, 1).to_string ());
  EQUAL (set.intervals ()[1].to_string (),
         Interval (3, infinity_f).to_string ());

  // Full and empty sets are handled correctly.
  set = Interval_set ().complement ();
  EQUAL (set.intervals ().size (), 1u);
  EQUAL (set.intervals ()[0].to_string (),
         Interval (-infinity_f, infinity_f).to_string ());
  set = set.complement ();
  EQUAL (set.intervals ().size (), 0u);
}
