/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef SPRING_HH
#define SPRING_HH

#include "lily-proto.hh"
#include "smobs.hh"

#include <vector>

/*
  A spring changes length according to force, according to the formula

  length = max(ideal_distance_ + (force * inverse_strength), min_distance)

  inverse_strength can be set differently for stretching/compressing.
 */
class Spring : public Simple_smob<Spring>
{
public:
  static SCM equal_p (SCM, SCM);
  static const char *const type_p_name_;

private:
  // parameters
  Real ideal_distance_;
  Real min_distance_;
  Real inverse_stretch_strength_;
  Real inverse_compress_strength_;

  // derived data.
  Real blocking_force_;

  void update_blocking_force ();

public:
  Spring ();
  Spring (Real ideal_distance, Real min_distance);

  Real ideal_distance () const { return ideal_distance_; }
  Real min_distance () const { return min_distance_; }
  Real inverse_stretch_strength () const { return inverse_stretch_strength_; }
  Real inverse_compress_strength () const { return inverse_compress_strength_; }
  Real blocking_force () const { return blocking_force_; }

  Real length (Real force) const;

  void set_ideal_distance (Real);
  void set_min_distance (Real);
  void ensure_min_distance (Real);
  void set_inverse_stretch_strength (Real);
  void set_inverse_compress_strength (Real);
  void set_blocking_force (Real);
  void set_default_strength ();
  void set_default_compress_strength ();
  void set_default_stretch_strength ();

  void operator*= (Real);
  bool operator> (Spring const &) const;
};

Spring merge_springs (std::vector<Spring> const &springs);

#endif /* SPRING_HH */
