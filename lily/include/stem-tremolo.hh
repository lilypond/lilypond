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

#ifndef ABBREV_HH
#define ABBREV_HH

#include "lily-proto.hh"
#include "grob-interface.hh"

class Stem_tremolo
{
public:
  DECLARE_SCHEME_CALLBACK (calc_cross_staff, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_slope, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_width, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_y_offset, (SCM));
  DECLARE_SCHEME_CALLBACK (pure_calc_y_offset, (SCM, SCM, SCM));
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (width, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_shape, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_direction, (SCM));
  DECLARE_SCHEME_CALLBACK (pure_height, (SCM, SCM, SCM));
  static Stencil raw_stencil (Grob *, Real slope, Direction stemdir);
  static Real y_offset (Grob *, bool pure);
  static Stencil untranslated_stencil (Grob *, Real slope);
  static Real get_beam_translation (Grob *me);
  static Real vertical_length (Grob *me);
};

#endif /* ABBREV_HH */
