/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef SPAN_BAR_HH
#define SPAN_BAR_HH

#include "lily-proto.hh"
#include "grob-interface.hh"

/**
   This is a barline that is spanned across other bar lines.  This is
   the implementation of the long barlines that occur in orchestral
   score and other multi-staff music.
*/
class Span_bar
{
public:

  DECLARE_GROB_INTERFACE();
  static Interval get_spanned_interval (Grob *);
  static void add_bar (Grob *, Grob *);
  static void evaluate_glyph (Grob *);
  DECLARE_SCHEME_CALLBACK (width, (SCM smob));
  DECLARE_SCHEME_CALLBACK (calc_bar_size, (SCM));
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_glyph_name, (SCM));
  DECLARE_SCHEME_CALLBACK (before_line_breaking, (SCM));
  DECLARE_SCHEME_CALLBACK (center_on_spanned_callback, (SCM element));
};

#endif // SPAN_BAR_HH
