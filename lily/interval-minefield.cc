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

#include "interval-minefield.hh"

Interval_minefield::Interval_minefield (Interval feasible_placements, Real bulk)
{
  feasible_placements_ = feasible_placements;
  bulk_ = bulk;
}

void
Interval_minefield::add_forbidden_interval (Interval forbidden)
{
  forbidden_intervals_.push_back (forbidden);
}

Interval
Interval_minefield::feasible_placements ()
{
  return feasible_placements_;
}

/*
  forbidden_intervals_ contains a vector of intervals in which
  the beam cannot start.  it iterates through these intervals,
  pushing feasible_placements_ epsilon over or epsilon under a
  collision.  when this type of change happens, the loop is marked
  as "dirty" and re-iterated.

  TODO: figure out a faster ways that this loop can happen via
  a better search algorithm.
*/
void
Interval_minefield::solve ()
{
  Real epsilon = 1.0e-10;
  bool dirty = false;
  do
    {
      dirty = false;
      for (vsize i = 0; i < forbidden_intervals_.size (); i++)
        {
          for (const auto d : {DOWN, UP})
            {
              Interval feasible_widened
                = Interval (feasible_placements_[d], feasible_placements_[d]);
              feasible_widened.widen (bulk_ / 2.);

              if (forbidden_intervals_[i][d] == d * infinity_f)
                feasible_placements_[d] = d * infinity_f;
              else if (forbidden_intervals_[i].contains (feasible_widened[d])
                       || forbidden_intervals_[i].contains (
                         feasible_widened[-d])
                       || feasible_widened.contains (forbidden_intervals_[i][d])
                       || feasible_widened.contains (
                         forbidden_intervals_[i][-d]))
                {
                  feasible_placements_[d]
                    = forbidden_intervals_[i][d] + d * (epsilon + (bulk_ / 2));
                  dirty = true;
                }
            }
        }
    }
  while (dirty);
}
