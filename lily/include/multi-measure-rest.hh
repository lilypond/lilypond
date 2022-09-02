/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2022 Jan Nieuwenhuizen <janneke@gnu.org>

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

#ifndef MULTI_MEASURE_REST_HH
#define MULTI_MEASURE_REST_HH

#include "grob-interface.hh"
#include "rod.hh"

class Multi_measure_rest
{
public:
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (height, (SCM));
  static void add_column (Spanner *, Item *);
  DECLARE_SCHEME_CALLBACK (set_spacing_rods, (SCM));
  DECLARE_SCHEME_CALLBACK (set_text_rods, (SCM));

  static void calculate_spacing_rods (Spanner *me, Real length);
  static Stencil big_rest (Grob *, Real);
  static Stencil symbol_stencil (Spanner *me, Real);
  static Stencil church_rest (Grob *, Font_metric *, int, int, Real);
  static Interval bar_width (Spanner *me);
};

#endif /* MULTI_MEASURE_REST_HH */
