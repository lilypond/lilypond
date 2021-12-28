/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2022 Han-Wen Nienhuys <hanwen@lilypond.org>

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

#ifndef SCALE_HH
#define SCALE_HH

#include "smobs.hh"
#include "rational.hh"

#include <vector>

class Scale : public Smob<Scale>
{
public:
  virtual ~Scale ();
  Scale (std::vector<Rational> const &);
  Scale (Scale const &);

  Rational tones_at_step (int step, int octave) const;
  Rational step_size (int step) const;
  int step_count () const;
  int normalize_step (int step) const;

private:
  std::vector<Rational> step_tones_;
};

extern Scale *default_global_scale;

#endif /* SCALE_HH */
