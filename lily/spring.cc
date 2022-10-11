/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2007--2022 Joe Neeman <joeneeman@gmail.com>

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
  Springs help chains of objects, such as the notes in a line of music,
  distribute themselves evenly.
  Each spring decides the length from the reference point of one object
  along the line to the reference point of the next, based on a force
  applied to the entire chain (see Spring::length() for details):
     length = distance_ + flexibility * force

  distance_  is the ideal separation between reference points
  inverse_stretch_strength_ is the flexibility when the force is stretching
  inverse_compress_strength_ is the flexibility when the force is compressing
  min_distance_ sets a lower limit on length

  Typically, the force applied to a list of objects ranges from about
  -1 to about 1, though there are no set limits.
*/

#include "spring.hh"

using std::vector;

Spring::Spring ()
{
  ideal_distance_ = 1.0;
  min_distance_ = 1.0;
  inverse_stretch_strength_ = 1.0;
  inverse_compress_strength_ = 1.0;

  update_blocking_force ();
}

Spring::Spring (Real dist, Real min_dist)
{
  ideal_distance_ = 1.0;
  min_distance_ = 1.0;
  inverse_stretch_strength_ = 1.0;
  inverse_compress_strength_ = 1.0;

  set_ideal_distance (dist);
  set_min_distance (min_dist);
  set_default_strength ();
  update_blocking_force ();
}

void
Spring::update_blocking_force ()
{
  // blocking_force_ is the value of force
  //   below which length(force) is constant, and
  //   above which length(force) varies according to inverse_*_strength.
  // Simple_spacer::compress_line() depends on the condition above.
  // We assume inverse_*_strength are non-negative.
  if (min_distance_ > ideal_distance_)
    if (inverse_stretch_strength_ > 0.0)
      blocking_force_
        = (min_distance_ - ideal_distance_) / inverse_stretch_strength_;
    else
      // Conceptually, this should be +inf, but 0.0 meets the requirements
      //  of Simple_spacer and creates fewer cases of 0.0*inf to handle.
      blocking_force_ = 0.0;
  else if (inverse_compress_strength_ > 0.0)
    blocking_force_
      = (min_distance_ - ideal_distance_) / inverse_compress_strength_;
  else
    blocking_force_ = 0.0;
}

/* scale a spring, but in a way that doesn't violate min_distance */
void
Spring::operator*= (Real r)
{
  ideal_distance_ = std::max (min_distance_, ideal_distance_ * r);
  inverse_compress_strength_ = std::max (0.0, ideal_distance_ - min_distance_);
  inverse_stretch_strength_ *= r;
  update_blocking_force ();
}

bool
Spring::operator> (Spring const &other) const
{
  return blocking_force_ > other.blocking_force_;
}

/* merge springs, basically by averaging them, but leave a little headroom
   above the largest minimum distance so that things don't get too cramped */
Spring
merge_springs (vector<Spring> const &springs)
{
  Real avg_distance = 0;
  Real min_distance = 0;
  Real avg_stretch = 0;
  Real avg_compress = 0;

  for (vsize i = 0; i < springs.size (); i++)
    {
      avg_distance += springs[i].ideal_distance ();
      avg_stretch += springs[i].inverse_stretch_strength ();
      avg_compress += 1 / springs[i].inverse_compress_strength ();
      min_distance = std::max (springs[i].min_distance (), min_distance);
    }

  avg_stretch /= static_cast<Real> (springs.size ());
  avg_compress /= static_cast<Real> (springs.size ());
  avg_distance /= static_cast<Real> (springs.size ());
  avg_distance = std::max (min_distance + 0.3, avg_distance);

  Spring ret = Spring (avg_distance, min_distance);
  ret.set_inverse_stretch_strength (avg_stretch);
  ret.set_inverse_compress_strength (1 / avg_compress);

  return ret;
}

void
Spring::set_ideal_distance (Real d)
{
  if (d < 0 || !std::isfinite (d))
    programming_error ("insane spring distance requested, ignoring it");
  else
    {
      ideal_distance_ = d;
      update_blocking_force ();
    }
}

void
Spring::set_min_distance (Real d)
{
  if (d < 0 || !std::isfinite (d))
    programming_error ("insane spring min_distance requested, ignoring it");
  else
    {
      min_distance_ = d;
      update_blocking_force ();
    }
}

void
Spring::ensure_min_distance (Real d)
{
  set_min_distance (std::max (d, min_distance_));
}

void
Spring::set_inverse_stretch_strength (Real f)
{
  if (!std::isfinite (f) || f < 0)
    programming_error ("insane spring constant");
  else
    inverse_stretch_strength_ = f;

  update_blocking_force ();
}

void
Spring::set_inverse_compress_strength (Real f)
{
  if (!std::isfinite (f) || f < 0)
    programming_error ("insane spring constant");
  else
    inverse_compress_strength_ = f;

  update_blocking_force ();
}

void
Spring::set_blocking_force (Real f)
{
  if (!std::isfinite (f))
    {
      programming_error ("insane blocking force");
      return;
    }

  blocking_force_ = -infinity_f;
  min_distance_ = length (f);
  update_blocking_force ();
}

void
Spring::set_default_strength ()
{
  set_default_stretch_strength ();
  set_default_compress_strength ();
}

void
Spring::set_default_compress_strength ()
{
  inverse_compress_strength_
    = (ideal_distance_ >= min_distance_) ? ideal_distance_ - min_distance_ : 0;
  update_blocking_force ();
}

void
Spring::set_default_stretch_strength ()
{
  inverse_stretch_strength_ = ideal_distance_;
}

Real
Spring::length (Real f) const
{
  Real force = std::max (f, blocking_force_);
  Real inv_k
    = force < 0.0 ? inverse_compress_strength_ : inverse_stretch_strength_;

  if (std::isinf (force))
    {
      // This only happens for +inf; -inf is impossible, as
      // blocking_force_ is finite.
      programming_error ("cruelty to springs");
      force = 0.0;
    }

  // There is a corner case here: if min_distance_ is larger than
  // distance_ but the spring is fixed, then inv_k will be zero
  // and we need to make sure that we return min_distance_.
  return std::max (min_distance_, ideal_distance_ + force * inv_k);
}
