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

#ifndef REST_HH
#define REST_HH

#include "direction.hh"
#include "grob-interface.hh"

class Grob;

class Rest
{
public:
  DECLARE_SCHEME_CALLBACK (y_offset_callback, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_cross_staff, (SCM));
  static std::string glyph_name (Grob *, int durlog, const std::string &style,
                                 bool, Real);
  static Real staff_position_internal (Grob *, int /* duration_log */,
                                       Direction);
  static SCM brew_internal_stencil (Grob *, bool);
  static SCM generic_extent_callback (Grob *, Axis);
  static void translate (Grob *me, int dy);
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (width, (SCM));
  DECLARE_SCHEME_CALLBACK (height, (SCM));
  DECLARE_SCHEME_CALLBACK (pure_height, (SCM, SCM, SCM));
};
#endif // REST_HH
