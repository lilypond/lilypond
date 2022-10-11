/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2020--2022 Han-Wen Nienhuys <hanwen@lilypond.org>

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

#ifndef LAZY_SKYLINE_PAIR_HH
#define LAZY_SKYLINE_PAIR_HH

#include "skyline-pair.hh"
#include "transform.hh"

#include <vector>

enum class Orientation
{
  CCW = -1, // counterclockwise
  CW = 1,   // clockwise
};

class Lazy_skyline_pair
{
  Axis a_;
  std::vector<Drul_array<Offset>> todo_;
  Drul_array<std::vector<Drul_array<Offset>>> per_dir_todo_;
  Skyline_pair skylines_;

public:
  Lazy_skyline_pair (Axis a) { a_ = a; }
  bool empty () const
  {
    return todo_.empty () && per_dir_todo_[UP].empty ()
           && per_dir_todo_[DOWN].empty ();
  }
  void add_segment (Transform const &tr, Offset p1, Offset p2)
  {
    todo_.push_back (Drul_array<Offset> (tr (p1), tr (p2)));
  }
  /* add segment, assuming it is a contour in some direction. */
  void add_contour_segment (Transform const &tr, Orientation orientation,
                            Offset p1, Offset p2)
  {
    Drul_array<Offset> seg (tr (p1), tr (p2));
    if ((seg[LEFT][a_] > seg[RIGHT][a_]) == (orientation == Orientation::CCW))
      {
        per_dir_todo_[(a_ == X_AXIS) ? UP : LEFT].push_back (seg);
      }
    else
      {
        per_dir_todo_[(a_ == X_AXIS) ? DOWN : RIGHT].push_back (seg);
      }
  }
  void add_segment (Transform const &tr, Offset p1, Offset p2, Real thickness)
  {
    if (thickness == 0)
      {
        add_segment (tr, p1, p2);
        return;
      }
    Real radius
      = (tr (Offset (thickness / 2, 0)) - tr (Offset (0, 0))).length ();

    Offset widen;
    widen[a_] = radius;
    Offset pad;
    pad[other_axis (a_)] = radius;

    p1 = tr (p1);
    p2 = tr (p2);
    if (p1[a_] > p2[a_])
      {
        std::swap (p1, p2);
      }
    p1 -= widen;
    p2 += widen;

    for (const auto d : {DOWN, UP})
      {
        per_dir_todo_[d].push_back (
          Drul_array<Offset> (p1 + d * pad, p2 + d * pad));
      }
  }

  Axis axis () const { return a_; }
  void add_box (Transform const &tr, Box b)
  {
    Offset ps[] = {Offset (b[X_AXIS][LEFT], b[Y_AXIS][DOWN]),
                   Offset (b[X_AXIS][LEFT], b[Y_AXIS][UP]),
                   Offset (b[X_AXIS][RIGHT], b[Y_AXIS][UP]),
                   Offset (b[X_AXIS][RIGHT], b[Y_AXIS][DOWN])};
    for (int i = 0; i < 4; i++)
      {
        add_contour_segment (tr, Orientation::CW, ps[i], ps[(i + 1) % 4]);
      }
  }

  void merge ()
  {
    for (const auto d : {DOWN, UP})
      {
        if (todo_.empty () && per_dir_todo_[d].empty ())
          continue;

        per_dir_todo_[d].insert (per_dir_todo_[d].end (), todo_.begin (),
                                 todo_.end ());
        skylines_[d].merge (Skyline (per_dir_todo_[d], a_, d));
        per_dir_todo_[d].clear ();
      }
    todo_.clear ();
  }

  Skyline_pair to_pair ()
  {
    merge ();
    return skylines_;
  }
};

Skyline_pair skylines_from_stencil (SCM sten, SCM rot, Axis a);

#endif
