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

#ifndef SPACING_OPTIONS_HH
#define SPACING_OPTIONS_HH

#include "lily-proto.hh"
#include "rational.hh"
#include "real.hh"

/*
  Various options for spacing. Usually inited from SpacingSpanner, but sometimes
  from GraceSpacing.
 */

class Spacing_options
{
public:
  bool packed_;
  bool stretch_uniformly_;
  bool float_nonmusical_columns_;
  bool float_grace_columns_;
  Rational global_shortest_;
  Real increment_;
  Real shortest_duration_space_;

  Spacing_options ();
  void init_from_grob (Grob *me);
  Real get_duration_space (Rational d) const;
};
#endif /* SPACING_OPTIONS_HH */
