/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef LINE_INTERFACE_HH
#define LINE_INTERFACE_HH

#include "lily-proto.hh"
#include "grob-interface.hh"

struct Line_interface
{
  static Stencil line (Grob *me, Offset from, Offset to);
  static Stencil make_zigzag_line (Grob *me, Offset from, Offset to);
  static Stencil make_trill_line (Grob *me, Offset from, Offset to);
  static Stencil make_dashed_line (Real th, Offset from, Offset to, Real, Real);
  static Stencil make_line (Real th, Offset from, Offset to);
  static Stencil make_arrow (Offset beg, Offset end, Real thick, Real length,
                             Real width);
  static Stencil arrows (Grob *me, Offset from, Offset to, bool from_arrow,
                         bool to_arrow);
};

#endif /* LINE_INTERFACE_HH */
