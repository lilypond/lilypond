/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2012 Han-Wen Nienhuys

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

#ifndef BAR_LINE_HH
#define BAR_LINE_HH

#include "lily-proto.hh"
#include "grob-interface.hh"

class Bar_line
{
public:
  DECLARE_GROB_INTERFACE ();

  static Stencil dashed_bar_line (Grob *me, Interval const &extent, Real thick);
  static Stencil tick_bar_line (Grob *me, Real h, bool rounded);
  static Stencil compound_barline (Grob *, string, Interval const &extent,
                                   bool rounded);
  static Stencil simple_barline (Grob *, Real wid, Interval const &extent,
                                 bool rounded);
  static Interval bar_y_extent (Grob *, Grob *);
  static bool non_empty_barline (Grob *me);

  DECLARE_SCHEME_CALLBACK (calc_bar_extent, (SCM));
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_anchor, (SCM));
};
#endif // BAR_LINE_HH
