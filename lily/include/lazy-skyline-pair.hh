/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2020 Han-Wen Nienhuys <hanwen@lilypond.org>

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

class Lazy_skyline_pair
{
  Axis a_;
  std::vector<Drul_array<Offset>> todo_;
  Skyline_pair skylines_;

public:
  Lazy_skyline_pair (Axis a) { a_ = a; }

  void add_segment (Transform const &tr, Offset p1, Offset p2)
  {
    todo_.push_back (Drul_array<Offset> (tr (p1), tr (p2)));
  }
  void add_segment (Transform const &tr, Offset p1, Offset p2, Real thickness)
  {
    if (!thickness) {
      add_segment(tr, p1, p2);
      return;
    }
    Real radius = (tr (Offset (thickness / 2, 0)) - tr (Offset (0, 0))).length ();

    Offset widen;
    widen[a_] = radius;
    Offset pad;
    pad[other_axis (a_)] = radius;

    p1 = tr (p1);
    p2 = tr (p2);
    p1 -= widen;
    p2 += widen;

    // TODO - should have separate todo_ for UP and DOWN
    todo_.push_back (Drul_array<Offset> (p1 + pad, p2 + pad));
    todo_.push_back (Drul_array<Offset> (p1 - pad, p2 - pad));
  }

  Axis axis () const { return a_; }
  void add_box (Transform const &tr, Box b) {
    Offset ps[] = {Offset(b[X_AXIS][LEFT],b[Y_AXIS][DOWN]),
                   Offset(b[X_AXIS][LEFT],b[Y_AXIS][UP]),
                   Offset(b[X_AXIS][RIGHT],b[Y_AXIS][UP]),
                   Offset(b[X_AXIS][RIGHT],b[Y_AXIS][DOWN])};
    for (int i = 0; i < 4; i++) {
      add_segment(tr, ps[i], ps[(i+1)%4]);
    }
  }

  void merge ()
  {
    if (todo_.empty ())
      return;

    Skyline_pair p (todo_, a_);
    skylines_.merge (p);
    todo_.clear ();
  }

  Skyline_pair to_pair ()
  {
    merge ();
    return skylines_;
  }
};

#endif
