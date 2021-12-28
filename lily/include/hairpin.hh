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

#ifndef HAIRPIN_HH
#define HAIRPIN_HH

#include "grob-interface.hh"
#include "lily-proto.hh"

struct Hairpin
{
public:
  static Real span_bar_correction (Spanner *me);
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (broken_bound_padding, (SCM));
  DECLARE_SCHEME_CALLBACK (pure_height, (SCM, SCM, SCM));
};

#endif // HAIRPIN_HH
