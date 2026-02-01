/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2023 Jan Nieuwenhuizen <janneke@gnu.org>

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

#ifndef ARPEGGIO_HH
#define ARPEGGIO_HH

#include "lily-proto.hh"
#include "grob-interface.hh"

class Arpeggio
{
public:
  static Grob *get_common_y (Grob *);
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_positions, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_cross_staff, (SCM));
  DECLARE_SCHEME_CALLBACK (width, (SCM));
  DECLARE_SCHEME_CALLBACK (pure_height, (SCM, SCM, SCM));

  // square bracket
  static Stencil brew_chord_bracket (Grob *, Interval y_extent);
  DECLARE_SCHEME_CALLBACK (brew_chord_bracket, (SCM));
  DECLARE_SCHEME_CALLBACK (chord_bracket_width, (SCM));

  // vertical slur
  DECLARE_SCHEME_CALLBACK (brew_chord_slur, (SCM));
};

#endif /* ARPEGGIO_HH */
