/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

// Needed because of extension definitions for POSIX functions.
#include "config.hh"

#include "offset.hh"

#include <cmath>

using std::string;

#ifndef STANDALONE
string
Offset::to_string () const
{
  string s;
  s = string (" (") + std::to_string (coordinate_a_[X_AXIS]) + ", "
      + std::to_string (coordinate_a_[Y_AXIS]) + ")";
  return s;
}
#endif

/*
  free bsd fix by John Galbraith
*/

Offset
complex_multiply (Offset z1, Offset z2)
{
  Offset z;
  if (!std::isinf (z2[Y_AXIS]))
    {
      z[X_AXIS] = z1[X_AXIS] * z2[X_AXIS] - z1[Y_AXIS] * z2[Y_AXIS];
      z[Y_AXIS] = z1[X_AXIS] * z2[Y_AXIS] + z1[Y_AXIS] * z2[X_AXIS];
    }
  return z;
}

static inline Real
atan2d (Real y, Real x)
{
  return atan2 (y, x) * (180.0 / M_PI);
}

Real
Offset::angle_degrees () const
{
  Real x = coordinate_a_[X_AXIS];
  Real y = coordinate_a_[Y_AXIS];

  // We keep in the vicinity of multiples of 45 degrees here: this is
  // where straightforward angles for straightforward angular
  // relations are most expected.  The factors of 2 employed in the
  // comparison are not really perfect for that: sqrt(2)+1 would be
  // the factor giving exact windows of 45 degrees rather than what we
  // have here.  It's just that 2 is likely to generate nicer code
  // than 2.4 and the exact handover does not really matter.
  //
  // Comparisons here are chosen appropriately to let infinities end
  // up in their "exact" branch.  As opposed to the normal atan2
  // function behavior, this makes "competing" infinities result in
  // NAN angles.
  if (y < 0.0)
    {
      if (2 * x < -y)
        if (-x > -2 * y) // x < 0, y < 0, |x| > |2y|
          return -180 + atan2d (-y, -x);
        else if (-2 * x >= -y) // x < 0, y < 0, |y| < |2x| <= |4y|
          return -135 + atan2d (x - y, -y - x);
        else // y < 0, |y| >= |2x|
          return -90 + atan2d (x, -y);
      else if (x <= -2 * y) // x > 0, y < 0, |y| <= |2x| < |4y|
        return -45 + atan2d (x + y, x - y);
      // Drop through for y < 0, x > |2y|
    }
  else if (y > 0.0)
    {
      if (2 * x < y)
        if (-x > 2 * y) // x < 0, y >= 0, |x| > |2y|
          return 180 - atan2d (y, -x);
        else if (-2 * x >= y) // x < 0, y >= 0, |y| < |2x| <= |4y|
          return 135 - atan2d (x + y, y - x);
        else // y >= 0, |y| >= |2x|
          return 90 - atan2d (x, y);
      else if (x <= 2 * y) // x >= 0, y >= 0, |y| < |2x| < |4y|
        return 45 - atan2d (x - y, x + y);
      // Drop through for y > 0, x > |2y|
    }
  else
    // we return 0 for (0,0).  NAN would be an option but is a
    // nuisance for getting back to rectangular coordinates.  Strictly
    // speaking, this argument would be just as valid for (+inf.0,
    // +inf.0), but then infinities are already an indication of a
    // problem in LilyPond.
    return (x < 0.0) ? 180 : 0;
  return atan2d (y, x);
}

/**
   euclidian vector length / complex modulus
*/
Real
Offset::length () const
{
  return hypot (coordinate_a_[X_AXIS], coordinate_a_[Y_AXIS]);
}

bool
Offset::is_sane () const
{
  return !std::isnan (coordinate_a_[X_AXIS])
         && !std::isnan (coordinate_a_[Y_AXIS])
         && !std::isinf (coordinate_a_[X_AXIS])
         && !std::isinf (coordinate_a_[Y_AXIS]);
}

Offset
Offset::direction () const
{
  Offset d = *this;
  if (std::isinf (d[X_AXIS]))
    {
      if (!std::isinf (d[Y_AXIS]))
        return Offset ((d[X_AXIS] > 0.0 ? 1.0 : -1.0), 0.0);
    }
  else if (std::isinf (d[Y_AXIS]))
    return Offset (0.0, (d[Y_AXIS] > 0.0 ? 1.0 : -1.0));
  else if (d[X_AXIS] == 0.0 && d[Y_AXIS] == 0.0)
    return d;
  // The other cases propagate or produce NaN as appropriate.

  d /= length ();
  return d;
}

Offset
Offset::swapped () const
{
  return Offset (coordinate_a_[Y_AXIS], coordinate_a_[X_AXIS]);
}

Offset
offset_directed (Real angle)
{
  if (angle <= -360.0 || angle >= 360.0)
    angle = fmod (angle, 360.0);
  // Now |angle| < 360.0, and the absolute size is not larger than
  // before, so we haven't lost precision.
  if (angle <= -180.0)
    angle += 360.0;
  else if (angle > 180.0)
    angle -= 360.0;
  // Now -180.0 < angle <= 180.0 and we still haven't lost precision.
  // We don't work with angles greater than 90 degrees absolute in
  // order to minimize how rounding errors of M_PI/180 affect the
  // result.  The "handover" between one sine expression to the next
  // happens at angles of ±90 degrees where sin(pi/2+eps) is about
  // (1-eps²/2) which should be numerically 1 pretty safely.
  //
  // Sign of the sine is chosen to avoid -0.0 in results.  This
  // version delivers exactly equal magnitude on x/y for odd multiples
  // of 45 degrees at the cost of losing some less obvious invariants.

  if (angle > 0)
    if (angle > 90)
      return Offset (sin ((90 - angle) * M_PI / 180.0),
                     sin ((180 - angle) * M_PI / 180.0));
    else
      return Offset (sin ((90 - angle) * M_PI / 180.0),
                     sin (angle * M_PI / 180.0));
  else if (angle < -90)
    return Offset (sin ((90 + angle) * M_PI / 180.0),
                   sin ((-180 - angle) * M_PI / 180.0));
  else
    return Offset (sin ((90 + angle) * M_PI / 180.0),
                   sin (angle * M_PI / 180.0));
}
