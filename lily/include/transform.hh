/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2018  David Kastrup <dak@gnu.org>

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

#ifndef TRANSFORM_HH
#define TRANSFORM_HH

#include <pango/pango-matrix.h>
#include "offset.hh"
#include "smobs.hh"

class Transform : public Simple_smob<Transform>, PangoMatrix
{
public:
  static const char * const type_p_name_;

  static const Transform identity;

  Transform ()
  {
    xx = 1.0;
    xy = 0.0;
    yx = 0.0;
    yy = 1.0;
    x0 = 0.0;
    y0 = 0.0;
  }

  Transform (Real p0, Real p1, Real p2, Real p3, Real p4, Real p5)
  {
    xx = p0;
    xy = p1;
    yx = p2;
    yy = p3;
    x0 = p4;
    y0 = p5;
  }

  explicit Transform (Offset p0)
  {
    xx = 1.0;
    xy = 0.0;
    yx = 0.0;
    yy = 1.0;
    x0 = p0[X_AXIS];
    y0 = p0[Y_AXIS];
  }

  Transform (Real angle, Offset center);

  // The following change the given matrix into one representing first
  // applying the transform indicated in the arguments and then the
  // original transform
  Transform & concat (const Transform &t);
  Transform & translate (Offset p);
  Transform & rotate (Real angle, Offset center);
  Transform & scale (Real xscale, Real yscale);

  Offset operator () (Offset point) const;
  Transform operator () (const Transform & t) const;

  Real get_xx () const { return xx; }
  Real get_xy () const { return xy; }
  Real get_yx () const { return yx; }
  Real get_yy () const { return yy; }
  Real get_x0 () const { return x0; }
  Real get_y0 () const { return y0; }
};

Offset scm_transform (SCM trans, Offset p);
Transform scm_transform (SCM trans, const Transform & t);

inline Transform
robust_scm2transform (SCM trans, const Transform &fallback = Transform::identity)
{
  if (Transform *tp = unsmob<Transform> (trans))
    return *tp;
  return fallback;
}

#endif
