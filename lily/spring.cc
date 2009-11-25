/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2007--2009 Joe Neeman <joeneeman@gmail.com>

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

#include "spring.hh"

Spring::Spring ()
{
  distance_ = 1.0;
  min_distance_ = 1.0;
  inverse_stretch_strength_ = 1.0;
  inverse_compress_strength_ = 1.0;

  update_blocking_force ();
}

Spring::Spring (Real dist, Real min_dist)
{
  distance_ = 1.0;
  min_distance_ = 1.0;
  inverse_stretch_strength_ = 1.0;
  inverse_compress_strength_ = 1.0;

  set_distance (dist);
  set_min_distance (min_dist);
  set_default_strength ();
  update_blocking_force ();
}

void
Spring::update_blocking_force ()
{
  if (min_distance_ > distance_)
    blocking_force_ = (min_distance_ - distance_) / inverse_stretch_strength_;
  else
    blocking_force_ = (min_distance_ - distance_) / inverse_compress_strength_;

  // If the spring is fixed, it's not clear what the natural value
  // of blocking_force_ would be. -infinity_f works fine for now.
  if (isnan (blocking_force_) || blocking_force_ == infinity_f)
    blocking_force_ = -infinity_f;

  if (blocking_force_ >= 0)
    inverse_compress_strength_ = 0;
}

/* scale a spring, but in a way that doesn't violate min_distance */
void
Spring::operator*= (Real r)
{
  distance_ = max (min_distance_, distance_ * r);
  inverse_compress_strength_ = max (0.0, distance_ - min_distance_);
  inverse_stretch_strength_ *= 0.8;
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
      avg_distance += springs[i].distance ();
      avg_stretch += springs[i].inverse_stretch_strength ();
      avg_compress += 1 / springs[i].inverse_compress_strength ();
      min_distance = max (springs[i].min_distance (), min_distance);
    }

  avg_stretch /= springs.size ();
  avg_compress /= springs.size ();
  avg_distance /= springs.size ();
  avg_distance = max (min_distance + 0.3, avg_distance);

  Spring ret = Spring (avg_distance, min_distance);
  ret.set_inverse_stretch_strength (avg_stretch);
  ret.set_inverse_compress_strength (1 / avg_compress);

  return ret;
}

void
Spring::set_distance (Real d)
{
  if (d < 0 || isinf (d) || isnan (d))
    programming_error ("insane spring distance requested, ignoring it");
  else
    {
      distance_ = d;
      update_blocking_force ();
    }
}

void
Spring::set_min_distance (Real d)
{
  if (d < 0 || isinf (d) || isnan (d))
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
  set_min_distance (max (d, min_distance_));
}

void
Spring::set_inverse_stretch_strength (Real f)
{
  if (isinf (f) || isnan (f) || f < 0)
    programming_error ("insane spring constant");
  else
    inverse_stretch_strength_ = f;

  update_blocking_force ();
}

void
Spring::set_inverse_compress_strength (Real f)
{
  if (isinf (f) || isnan (f) || f < 0)
    programming_error ("insane spring constant");
  else
    inverse_compress_strength_ = f;

  update_blocking_force ();
}

void
Spring::set_blocking_force (Real f)
{
  if (isinf (f) || isnan (f))
    {
      programming_error ("insane blocking force");
      return;
    }

  blocking_force_ = -infinity_f;
  min_distance_ = length (f);
  distance_ = max (distance_, min_distance_);
  update_blocking_force ();
}

void
Spring::set_default_strength ()
{
  inverse_compress_strength_ = (distance_ >= min_distance_) ? distance_ - min_distance_ : 0;
  inverse_stretch_strength_ = distance_;
  update_blocking_force ();
}

Real
Spring::length (Real f) const
{
  Real force = max (f, blocking_force_);
  Real inv_k = force < 0.0 ? inverse_compress_strength_ : inverse_stretch_strength_;

  if (force == infinity_f)
    {
      programming_error ("cruelty to springs");
      force = 0.0;
    }

  // There is a corner case here: if min_distance_ is larger than
  // distance_ but the spring is fixed, then inv_k will be zero
  // and we need to make sure that we return min_distance_.
  return max (min_distance_, distance_ + force * inv_k);
}

