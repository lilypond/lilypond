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

#include "interval-set.hh"

#include <algorithm>
#include <vector>

using std::vector;

/*
  A union of intervals in the real line.

  This class gives good performance for finding the union of
  a collection of intervals (n log n) and for testing if a point
  belongs to the union (log n).  It does not give an efficient way to
  update the set (ie. by adding more intervals); to do this
  efficiently would require a self-balancing tree, and it would not
  be currently useful in lilypond anyway.
*/

Interval_set::Interval_set ()
{
}

Interval_set
Interval_set::interval_union (vector<Interval> ivs)
{
  std::sort (ivs.begin (), ivs.end (), Interval::left_less);

  Interval_set ret;

  if (ivs.empty ())
    return ret;

  ret.intervals_.push_back (ivs.front ());

  // Go over the intervals from left to right.  If the current interval
  // overlaps with the last one, merge them.  Otherwise, append the
  // current interval to the list.
  for (vsize i = 1; i < ivs.size (); ++i)
    {
      Interval iv = ivs[i];
      Interval &last = ret.intervals_.back ();

      if (last[RIGHT] >= iv[LEFT])
        // overlapping intervals: merge them
        last[RIGHT] = std::max (last[RIGHT], iv[RIGHT]);
      else if (!iv.is_empty ())
        ret.intervals_.push_back (iv);
    }

  return ret;
}

// Returns an iterator pointing to the first interval whose left
// endpoint is at least x.  That interval may or may not contain x.
vector<Interval>::const_iterator
Interval_set::upper_bound (Real x) const
{
  Interval xx (x, x);
  return std::upper_bound (intervals_.begin (), intervals_.end (), xx,
                           Interval::left_less);
}

Real
Interval_set::nearest_point (Real x, Direction d) const
{
  Real left = -infinity_f; // The closest point to the left of x.
  Real right = infinity_f; // The closest point to the right of x.

  vector<Interval>::const_iterator i = upper_bound (x);
  if (i != intervals_.end ())
    right = (*i)[LEFT];

  if (i != intervals_.begin ())
    {
      Interval left_iv = *(i - 1);
      // left_iv[LEFT] is guaranteed to be less than x. So if
      // left_iv[RIGHT] >= x then left_iv contains x, which must then
      // be the nearest point to x.
      if (left_iv[RIGHT] >= x)
        return x;

      left = left_iv[RIGHT];
    }

  if (d == RIGHT)
    return right;
  if (d == LEFT)
    return left;
  else
    return (right - x) < (x - left) ? right : left;
}

Interval_set
Interval_set::complement () const
{
  Interval_set ret;

  if (intervals_.empty ())
    {
      ret.intervals_.push_back (Interval (-infinity_f, infinity_f));
      return ret;
    }

  if (intervals_[0][LEFT] > -infinity_f)
    ret.intervals_.push_back (Interval (-infinity_f, intervals_[0][LEFT]));

  for (vsize i = 1; i < intervals_.size (); ++i)
    ret.intervals_.push_back (
      Interval (intervals_[i - 1][RIGHT], intervals_[i][LEFT]));

  if (intervals_.back ()[RIGHT] < infinity_f)
    ret.intervals_.push_back (Interval (intervals_.back ()[RIGHT], infinity_f));

  return ret;
}
