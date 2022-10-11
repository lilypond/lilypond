/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

/*
  some 2D geometrical concepts
*/

#ifndef BOXES_HH
#define BOXES_HH

#include "interval.hh"
#include "offset.hh"
#include "smobs.hh"

class Box
{
public:
  static const char *const type_p_name_;

private:
  Interval interval_a_[NO_AXES];

public:
  Interval &x () { return interval_a_[X_AXIS]; }
  Interval &y () { return interval_a_[Y_AXIS]; }
  Interval x () const { return interval_a_[X_AXIS]; }
  Interval y () const { return interval_a_[Y_AXIS]; }
  Interval operator[] (Axis a) const;
  Interval &operator[] (Axis a);
  Real area () const;
  bool is_empty () const;
  bool is_empty (Axis a) const;

  Offset center () const;

  void translate (Offset o);

  void set_empty ();
  void add_point (Offset);
  void widen (Real x, Real y);
  void scale (Real r);

  /// smallest box enclosing `this` and `b`
  void unite (Box b);
  void intersect (Box b);
  void print ();
  Box ();
  Box (Interval ix, Interval iy);
};

#endif
