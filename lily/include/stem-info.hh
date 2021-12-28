/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Jan Nieuwenhuizen <janneke@gnu.org>

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

#ifndef STEM_INFO_HH
#define STEM_INFO_HH

#include "real.hh"
#include "direction.hh"

/*
  Parameters for a stem, (multiply with stemdirection, to get real values
  for a downstem.)
*/
struct Stem_info
{
  Direction dir_;
  Real ideal_y_;
  Real shortest_y_;
  Stem_info ();
  void scale (Real);
};

#endif // STEM_INFO_HH
